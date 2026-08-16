library(tidyverse)
library(ranger)
library(e1071)

# --- Helpers -----------------------------------------------------------------

high_clip <- function(vec, threshold) pmin(vec, threshold)

# --- Dataset -----------------------------------------------------------------

# glmnet is linear and the SVM's kernel is smooth, so a test row outside the
# training range is answered by extrapolation rather than by any observed house.
# Test Id 2550 has TotalSF 10190 against a training maximum of 6872 and drew
# $1.67M out of glmnet, 2.2x the dearest house ever sold in the data. Winsorising
# test predictors to the observed training range costs nothing on rows that are
# already inside it, and CV cannot see this at all: it splits training rows only.
clip_to_train_range <- function(train_df, test_df, response){
  numeric_predictors <- names(test_df) %>%
    keep(\(v) is.numeric(test_df[[v]]) && v != response)

  for (v in numeric_predictors) {
    limits <- range(train_df[[v]], na.rm = TRUE)
    test_df[[v]] <- high_clip(pmax(test_df[[v]], limits[1]), limits[2])
  }
  test_df
}

make_dataset <- function(df, predictor, test_df = NULL, clip_test = TRUE){
  n_train <- nrow(df)
  # Cleaned together so factor levels match across train and test.
  combined <- if (is.null(test_df)) df else bind_rows(df, test_df)
  prepared <- prepare_df(combined)

  train <- prepared[seq_len(n_train), ]
  test <- if (is.null(test_df)) NULL else prepared[-seq_len(n_train), ]
  if (!is.null(test) && clip_test) test <- clip_to_train_range(train, test, predictor)

  list(
    df = train,
    test = test,
    test_ids = if (is.null(test_df)) NULL else test_df$Id,
    predictor = predictor
  )
}

# Orderings taken from data_description.txt, worst first. Absent-feature levels
# ("None", from the NA fill) sit at the bottom because no basement is worse than
# a poor one. Encoding these as ranks keeps the rare endpoints intact: ExterQual
# Fa and HeatingQC Po are scale positions, not noise to be lumped away.
ordinal_scales <- list(
  ExterQual    = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  ExterCond    = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  BsmtQual     = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  BsmtCond     = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  HeatingQC    = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  KitchenQual  = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  FireplaceQu  = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  GarageQual   = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  GarageCond   = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  PoolQC       = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  BsmtExposure = c("None", "No", "Mn", "Av", "Gd"),
  BsmtFinType1 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  BsmtFinType2 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  GarageFinish = c("None", "Unf", "RFn", "Fin"),
  Functional   = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"),
  Fence        = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"),
  LotShape     = c("IR3", "IR2", "IR1", "Reg"),
  LandSlope    = c("Sev", "Mod", "Gtl"),
  PavedDrive   = c("N", "P", "Y")
)

# Where NA means missing rather than absent, the NA fill produces a "None" that
# no scale has a slot for. Functional's default is the dictionary's own
# instruction ("assume typical"); KitchenQual is a single test row.
ordinal_na_default <- list(Functional = "Typ", KitchenQual = "TA")

ordinal_rank <- function(x, scale, column){
  unmapped <- setdiff(unique(x), scale)
  if (length(unmapped)) {
    stop("unmapped level in ", column, ": ", paste(unmapped, collapse = ", "), call. = FALSE)
  }
  match(x, scale) - 1L
}

# rare_min and max_modal_share act on the combined train + test frame, so a
# level's rarity here is the rarity the model encoding actually sees.
prepare_df <- function(df, rare_min = 20, max_modal_share = 0.98){
  response <- "SalePrice"

  # A factor this concentrated is constant for practical purposes: whatever it
  # explains rests on a handful of rows, which is where omega squared is least
  # trustworthy and where the SVM's one-hot expansion pays the most.
  near_constant <- function(x){
    (is.character(x) || is.factor(x)) && max(table(x)) / sum(!is.na(x)) >= max_modal_share
  }

  df %>%
    select(-any_of("Id")) %>%
    mutate(
      MSSubClass = as.character(MSSubClass),
      MoSold = as.character(MoSold),
      across(where(is.character), ~ replace_na(.x, "None")),
      across(where(is.numeric) & !any_of(response), ~ replace_na(.x, 0))
    ) %>%
    mutate(
      TotalSF = TotalBsmtSF + X1stFlrSF + X2ndFlrSF,
      TotalBaths = FullBath + BsmtFullBath + 0.5 * (HalfBath + BsmtHalfBath),
      PorchSF = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + WoodDeckSF,
      Age = YrSold - YearBuilt,
      RemodAge = YrSold - YearRemodAdd
    ) %>%
    select(
      -TotalBsmtSF, -X1stFlrSF, -X2ndFlrSF,
      -YearBuilt, -YearRemodAdd,
      -FullBath, -HalfBath, -BsmtFullBath, -BsmtHalfBath,
      -OpenPorchSF, -EnclosedPorch, -X3SsnPorch, -ScreenPorch, -WoodDeckSF
    ) %>%
    # Drop before encoding, while the ordinal columns are still character:
    # PoolQC is 99.6% None and stays a drop, not a rank column of zeros.
    select(-where(near_constant)) %>%
    mutate(
      across(any_of(response), log),
      across(any_of(names(ordinal_na_default)),
             \(x) replace(x, x == "None", ordinal_na_default[[cur_column()]])),
      across(any_of(names(ordinal_scales)),
             \(x) ordinal_rank(x, ordinal_scales[[cur_column()]], cur_column()))
    ) %>%
    # Lump before the second drop: folding a tail into Other can push a column
    # over the near-constant line, and such a column is one worth dropping.
    mutate(across(where(is.character), ~ fct_lump_min(.x, min = rare_min, other_level = "Other"))) %>%
    select(-where(near_constant)) %>%
    mutate(across(where(is.character), as.factor))
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

# glmnet and xgboost need a numeric design matrix. terms/xlev are carried on the
# fitted object so predict() rebuilds identical columns on unseen rows.
model_matrix_parts <- function(dataset){
  form <- reformulate(".", response = dataset$predictor)
  mf <- model.frame(form, data = dataset$df, na.action = na.pass)
  trm <- terms(mf)
  list(
    x = model.matrix(trm, mf)[, -1, drop = FALSE],
    y = model.response(mf),
    terms = trm,
    xlev = .getXlevels(trm, mf)
  )
}

new_matrix_model <- function(model, parts, class){
  structure(
    list(model = model, terms = delete.response(parts$terms), xlev = parts$xlev),
    class = class
  )
}

matrix_newx <- function(object, newdata){
  mf <- model.frame(object$terms, newdata, xlev = object$xlev, na.action = na.pass)
  model.matrix(object$terms, mf, xlev = object$xlev)[, -1, drop = FALSE]
}

# lambda pinned rather than re-searched: cv.glmnet ran a 5-fold hunt inside every
# outer fold, and its lambda.min landed between 0.0017 and 0.0083 (median 0.0064)
# across the 25 folds, so the search was mostly re-deriving one number at 6x the
# fits. Still fit the full path and index into it -- glmnet warns against solving
# for a lone lambda, and the path costs nothing next to the dropped inner CV.
train_glmnet <- function(dataset, alpha = 0.5, lambda = 0.006){
  parts <- model_matrix_parts(dataset)
  fit <- glmnet::glmnet(parts$x, parts$y, alpha = alpha)
  model <- new_matrix_model(fit, parts, "glmnet_model")
  model$lambda <- lambda
  model
}

predict.glmnet_model <- function(object, newdata, ...){
  as.numeric(predict(object$model, newx = matrix_newx(object, newdata),
                     s = object$lambda))
}

train_xgb <- function(
  dataset,
  nrounds = 600,
  eta = 0.05,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  nthread = 0              # 0 -> all cores; lower it when forking folds
){
  parts <- model_matrix_parts(dataset)
  # xgb.train, not xgboost(): the 3.x wrapper silently ignores a params list.
  fit <- xgboost::xgb.train(
    params = list(
      objective = "reg:squarederror",
      eta = eta,
      max_depth = max_depth,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      min_child_weight = min_child_weight,
      nthread = nthread
    ),
    data = xgboost::xgb.DMatrix(data = parts$x, label = parts$y),
    nrounds = nrounds,
    verbose = 0
  )
  new_matrix_model(fit, parts, "xgb_model")
}

predict.xgb_model <- function(object, newdata, ...){
  as.numeric(predict(object$model, matrix_newx(object, newdata)))
}

# Weights from blend_sweep() over out-of-fold predictions. The random forest
# scored 0: weakest member, and most correlated with xgboost (0.87). Re-swept
# after the SVM was retuned; the surface is flat across svm 0.45-0.65, so this
# sits mid-plateau rather than on the argmin, which is only 0.0002 better and
# selected on the same folds it is scored against. xgb keeps a quarter despite
# being the weakest member because its residuals are the least correlated with
# the other two (0.83-0.85, against 0.93 between svm and glmnet).
blend_weights <- c(svm = 0.50, glmnet = 0.25, xgb = 0.25)

fitters <- list(
  # gamma pinned rather than left at e1071's 1 / n_columns default: ordinal
  # encoding cut the design matrix from 223 columns to 174, which silently
  # widened gamma by 28% and cost the SVM more than the encoding gained it.
  svm    = function(d) train_svm(d, cost = 10, gamma = 0.001),
  glmnet = function(d) train_glmnet(d),
  xgb    = function(d) train_xgb(d, nthread = 2)
)

train_blend <- function(dataset, weights = blend_weights){
  structure(
    list(models = map(fitters[names(weights)], ~ .x(dataset)), weights = weights),
    class = "blend"
  )
}

predict.blend <- function(object, newdata, ...){
  preds <- map(object$models, predict_model, newdata = newdata)
  as.numeric(reduce(map2(preds, object$weights[names(preds)], ~ .x * .y), `+`))
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

# One pass over the folds fitting every model, returning out-of-fold predictions.
# Per-model scores and any blend derive from these, so nothing is fit twice.
# Folds are drawn before fitting: the fitters consume RNG state, which would
# otherwise unpair the comparison between them.
oof_predictions <- function(dataset, fitters, k = 5, repeats = 5, seed = 42,
                            workers = 1){
  set.seed(seed)
  df <- dataset$df
  response <- dataset$predictor
  fold_sets <- map(seq_len(repeats), ~ sample(rep_len(seq_len(k), nrow(df))))
  jobs <- expand_grid(rep = seq_len(repeats), fold = seq_len(k))

  run <- function(j){
    folds <- fold_sets[[jobs$rep[j]]]
    i <- jobs$fold[j]
    train <- list(predictor = response, df = df[folds != i, ])
    held <- df[folds == i, ]
    bind_cols(
      tibble(rep = jobs$rep[j], fold = i, row = which(folds == i),
             actual = held[[response]]),
      as_tibble(map(fitters, ~ predict_model(.x(train), held)))
    )
  }

  out <- if (workers > 1) {
    parallel::mclapply(seq_len(nrow(jobs)), run, mc.cores = workers)
  } else {
    map(seq_len(nrow(jobs)), run)
  }
  bind_rows(out)
}

cv_scores <- function(oof, weights = NULL){
  models <- setdiff(names(oof), c("rep", "fold", "row", "actual"))
  # SE across folds, not repeats: repeat-level RMSEs each span every row and so
  # differ only by fold shuffling, which would understate uncertainty ~6x.
  fold_id <- paste(oof$rep, oof$fold)
  score <- function(p){
    by_fold <- split(seq_len(nrow(oof)), fold_id) %>%
      map_dbl(~ rmse(oof$actual[.x], p[.x]))
    tibble(cv_rmse = rmse(oof$actual, p),
           cv_se = sd(by_fold) / sqrt(length(by_fold)))
  }

  out <- map_dfr(models, ~ bind_cols(tibble(model = .x), score(oof[[.x]])))
  if (!is.null(weights)) {
    blended <- as.matrix(oof[names(weights)]) %*% weights
    out <- bind_rows(out, bind_cols(tibble(model = "blend"), score(blended)))
  }
  arrange(out, cv_rmse)
}

# Permutation importance for whatever model is actually being shipped, not for a
# forest fitted only to be asked. Each column is shuffled in the held-out fold
# and the model re-predicted, so the number is the out-of-sample RMSE cost of
# losing that column. The SE across folds gives the noise floor directly: an
# importance under about 2 SE is not distinguishable from a useless column.
permutation_importance <- function(dataset, fitter = train_blend, k = 5, repeats = 1,
                                   seed = 42, workers = 5){
  set.seed(seed)
  df <- dataset$df
  response <- dataset$predictor
  vars <- setdiff(names(df), response)

  fold_sets <- map(seq_len(repeats), ~ sample(rep_len(seq_len(k), nrow(df))))
  jobs <- expand_grid(rep = seq_len(repeats), fold = seq_len(k))

  run <- function(j){
    folds <- fold_sets[[jobs$rep[j]]]
    i <- jobs$fold[j]
    model <- fitter(list(predictor = response, df = df[folds != i, ]))
    held <- df[folds == i, ]
    base <- rmse(held[[response]], predict_model(model, held))

    map_dfr(vars, function(v){
      shuffled <- held
      shuffled[[v]] <- sample(shuffled[[v]])
      tibble(variable = v,
             delta = rmse(held[[response]], predict_model(model, shuffled)) - base)
    })
  }

  out <- if (workers > 1) {
    parallel::mclapply(seq_len(nrow(jobs)), run, mc.cores = workers)
  } else {
    map(seq_len(nrow(jobs)), run)
  }

  bind_rows(out) %>%
    group_by(variable) %>%
    summarise(importance = mean(delta), se = sd(delta) / sqrt(n()), .groups = "drop") %>%
    arrange(desc(importance))
}

# Grid over the weight simplex. The optimum is flat enough that the exact
# minimiser is not meaningful; read the top rows, not row one.
blend_sweep <- function(oof, step = 0.05){
  models <- setdiff(names(oof), c("rep", "fold", "row", "actual"))
  preds <- as.matrix(oof[models])

  grid <- models %>%
    set_names() %>%
    map(~ seq(0, 1, by = step)) %>%
    expand.grid() %>%
    as_tibble() %>%
    filter(abs(rowSums(across(everything())) - 1) < 1e-9)

  weights <- as.matrix(grid)
  grid %>%
    mutate(cv_rmse = apply(weights, 1, \(w) rmse(oof$actual, preds %*% w))) %>%
    arrange(cv_rmse)
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
# (SE 0.002 vs 0.006). Skew correction, median imputation and lumping were all
# within noise; PCA was worse. Ordinal quality scales help glmnet and xgb only
# slightly, but shrinking the design matrix moved the SVM's default gamma enough
# to matter, and pinning it is where most of the gain below comes from.

# Two >4000 sqft houses sold near $160k-$185k; partial sales, not market prices.
raw <- read.csv("train.csv", na.strings = c("NA", "")) %>%
  filter(!(GrLivArea > 4000 & SalePrice < 300000))
test_raw <- read.csv("test.csv", na.strings = c("NA", ""))

dataset <- make_dataset(raw, "SalePrice", test_df = test_raw)

oof <- oof_predictions(dataset, fitters, k = 5, repeats = 5, workers = 5)

print(cv_scores(oof, blend_weights))

# --- Kaggle submission -------------------------------------------------------

# test.csv is unlabelled; the CV RMSE above is the estimate of these scores.

blend_fit <- train_blend(dataset)

cat("\nSubmissions:\n")
blend_submission <- write_submission(blend_fit, dataset, "submission_blend.csv")

# --- Variable importance -----------------------------------------------------

importance_tbl <- permutation_importance(dataset)

print(importance_tbl, n = 25)

importance_tbl %>%
  slice_head(n = 25) %>%
  ggplot(aes(x = importance, y = fct_reorder(variable, importance))) +
  geom_col(fill = "steelblue") +
  geom_errorbarh(aes(xmin = importance - se, xmax = importance + se), height = 0.3,
                 colour = "grey30", linewidth = 0.3) +
  labs(
    title = "Permutation importance of the blend on log(SalePrice)",
    subtitle = paste("Out-of-fold RMSE cost of shuffling each column;",
                     "bars are +/- 1 SE across folds"),
    x = "Increase in RMSE when permuted",
    y = NULL
  ) +
  theme_minimal()
