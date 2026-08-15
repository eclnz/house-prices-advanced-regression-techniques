library(tidyverse)
library(ranger)
library(e1071)

# --- Dataset -----------------------------------------------------------------

make_dataset <- function(df, predictor, test_df = NULL){
  n_train <- nrow(df)
  # Cleaned together so factor levels match across train and test.
  combined <- if (is.null(test_df)) df else bind_rows(df, test_df)
  prepared <- prepare_df(combined)

  list(
    df = prepared[seq_len(n_train), ],
    test = if (is.null(test_df)) NULL else prepared[-seq_len(n_train), ],
    test_ids = if (is.null(test_df)) NULL else test_df$Id,
    predictor = predictor
  )
}

prepare_df <- function(df){
  response <- "SalePrice"

  df %>%
    select(-any_of("Id")) %>%
    mutate(
      MSSubClass = as.character(MSSubClass),
      MoSold = as.character(MoSold),
      across(where(is.character), ~ replace_na(.x, "None")),
      # any_of: test rows have no response, and NA there is not a zero-price sale
      across(where(is.numeric) & !any_of(response), ~ replace_na(.x, 0)),
      across(any_of(response), log),
      across(where(is.character), as.factor)
    )
}

# --- Fitting -----------------------------------------------------------------

drop_null <- function(x) x[!vapply(x, is.null, logical(1))]

train_rf <- function(
  dataset,
  num_trees = 1000,
  mtry = NULL,               # NULL -> floor(sqrt(n_predictors))
  min_node_size = 5,
  max_depth = NULL,          # NULL/0 -> unlimited
  sample_fraction = ifelse(replace, 1, 0.632),
  replace = TRUE,
  splitrule = "variance",    # "variance", "extratrees", "maxstat"
  num_random_splits = 1,     # extratrees only
  respect_unordered_factors = "order",
  importance = "none"        # "permutation" is a large share of the fit time
){
  ranger(
    reformulate(".", response = dataset$predictor),
    data = dataset$df,
    num.trees = num_trees,
    mtry = mtry,
    min.node.size = min_node_size,
    max.depth = max_depth,
    sample.fraction = sample_fraction,
    replace = replace,
    splitrule = splitrule,
    num.random.splits = num_random_splits,
    respect.unordered.factors = respect_unordered_factors,
    importance = importance
    )
}

train_svm <- function(
  dataset,
  kernel = "radial",         # "radial", "linear", "polynomial", "sigmoid"
  cost = 1,
  gamma = NULL,              # NULL -> 1 / n_columns
  epsilon = 0.1,
  degree = 3,                # polynomial only
  scale = TRUE
){
  # The formula interface one-hot encodes factors via model.matrix.
  do.call(e1071::svm, drop_null(list(
    reformulate(".", response = dataset$predictor),
    data = dataset$df,
    type = "eps-regression",
    kernel = kernel,
    cost = cost,
    gamma = gamma,
    epsilon = epsilon,
    degree = degree,
    scale = scale
  )))
}

# --- Evaluation --------------------------------------------------------------

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

predict_model <- function(model, newdata){
  out <- predict(model, newdata)
  if (inherits(out, "ranger.prediction")) out$predictions else as.numeric(out)
}

# In-sample only. Use cv_model() to compare candidates.
eval_model <- function(model, dataset, back_transform = exp){
  actual <- dataset$df[[dataset$predictor]]
  predicted <- predict_model(model, dataset$df)
  is_rf <- inherits(model, "ranger")

  tibble(
    oob_rmse = if (is_rf) sqrt(model$prediction.error) else NA_real_,
    oob_r2 = if (is_rf) model$r.squared else NA_real_,
    train_rmse = rmse(actual, predicted),
    train_r2 = 1 - sum((actual - predicted)^2) / sum((actual - mean(actual))^2),
    train_rmse_response = if (is.null(back_transform)) {
      NA_real_
    } else {
      rmse(back_transform(actual), back_transform(predicted))
    }
  )
}

cv_model <- function(dataset, fit_fn = train_rf, k = 5, repeats = 2, seed = 42, ...){
  set.seed(seed)
  df <- dataset$df
  response <- dataset$predictor

  fold_rmse <- map(seq_len(repeats), function(rep){
    folds <- sample(rep_len(seq_len(k), nrow(df)))
    map_dbl(seq_len(k), function(i){
      fold <- list(predictor = response, df = df[folds != i, ])
      model <- fit_fn(fold, ...)
      held <- df[folds == i, ]
      rmse(held[[response]], predict_model(model, held))
    })
  }) %>% unlist()

  tibble(
    cv_rmse = mean(fold_rmse),
    cv_sd = sd(fold_rmse),
    cv_se = sd(fold_rmse) / sqrt(length(fold_rmse))
  )
}

print_metrics <- function(metrics, cv = NULL){
  cat("OOB RMSE (log scale): ", metrics$oob_rmse, "\n")
  cat("OOB R-squared:        ", metrics$oob_r2, "\n")
  cat("Train RMSE (log):     ", metrics$train_rmse, "\n")
  cat("Train R-squared:      ", metrics$train_r2, "\n")
  cat("Train RMSE ($):       ", metrics$train_rmse_response, "\n")
  if (!is.null(cv)) {
    cat("CV RMSE (log):        ", cv$cv_rmse, "+/- SE", cv$cv_se, "\n")
  }
  invisible(metrics)
}

write_submission <- function(model, dataset, path, back_transform = exp){
  stopifnot(!is.null(dataset$test))

  # Response dropped: it is all NA here, and predict.svm na.omits newdata.
  newdata <- dataset$test %>% select(-any_of(dataset$predictor))
  predicted <- predict_model(model, newdata)
  stopifnot(length(predicted) == length(dataset$test_ids))

  out <- tibble(Id = dataset$test_ids, SalePrice = back_transform(predicted))
  write.csv(out, path, row.names = FALSE)

  cat(sprintf("%s: %d rows, predicted $%.0f - $%.0f (median $%.0f)\n",
              path, nrow(out), min(out$SalePrice), max(out$SalePrice),
              median(out$SalePrice)))
  invisible(out)
}

# --- Run ---------------------------------------------------------------------

# No holdout: repeated full-data CV estimates better than one 292-row split
# (SE 0.002 vs 0.006). Preprocessing variants (ordinal quality scales, skew
# correction, median imputation, lumping) were all within noise; PCA was worse.

raw <- read.csv("train.csv", na.strings = c("NA", ""))
test_raw <- read.csv("test.csv", na.strings = c("NA", ""))

dataset <- make_dataset(raw, "SalePrice", test_df = test_raw)

rf <- train_rf(
  dataset,
  mtry = 25,                 # p/3, the regression rule of thumb
  importance = "permutation"
)

metrics <- eval_model(rf, dataset)
rf_cv <- cv_model(dataset, fit_fn = train_rf, k = 5, repeats = 5, mtry = 25)

cat("\nRandom forest:\n")
print_metrics(metrics, rf_cv)

# --- SVM ---------------------------------------------------------------------

svm_fit <- train_svm(
  dataset,
  cost = 10                  # only hyperparameter the search separated
)

svm_metrics <- eval_model(svm_fit, dataset)
svm_cv <- cv_model(dataset, fit_fn = train_svm, k = 5, repeats = 5, cost = 10)

cat("\nSVM:\n")
print_metrics(svm_metrics, svm_cv)

# --- Kaggle submission -------------------------------------------------------

# test.csv is unlabelled; the CV RMSE above is the estimate of these scores.

cat("\nSubmissions:\n")
rf_submission <- write_submission(rf, dataset, "submission_rf.csv")
svm_submission <- write_submission(svm_fit, dataset, "submission_svm.csv")

# --- Variable importance -----------------------------------------------------

importance_tbl <- tibble(
  variable = names(ranger::importance(rf)),
  importance = ranger::importance(rf)
) %>%
  arrange(desc(importance))

print(importance_tbl, n = 25)

importance_tbl %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = importance, y = fct_reorder(variable, importance))) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Permutation importance, random forest on log(SalePrice)",
    x = "Increase in MSE when permuted",
    y = NULL
  ) +
  theme_minimal()
