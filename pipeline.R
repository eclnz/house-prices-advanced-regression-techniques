library(tidyverse)
library(e1071)

# --- Helpers -----------------------------------------------------------------

high_clip <- function(vec, threshold) pmin(vec, threshold)

# Runs expr under a local seed without disturbing the caller's RNG stream.
with_seed <- function(seed, expr){
  old <- if (exists(".Random.seed", envir = .GlobalEnv)) .Random.seed else NULL
  on.exit(if (is.null(old)) rm(".Random.seed", envir = .GlobalEnv)
          else assign(".Random.seed", old, envir = .GlobalEnv))
  set.seed(seed)
  expr
}

# --- Dataset -----------------------------------------------------------------

# glmnet and the SVM kernel both extrapolate badly outside the training range
# (one test row once produced a $1.67M prediction, 2.2x the highest sale seen).
clip_to_train_range <- function(train_df, test_df, response){
  numeric_predictors <- names(test_df) %>%
    keep(\(v) is.numeric(test_df[[v]]) && v != response)

  test_df %>%
    mutate(across(all_of(numeric_predictors), \(x) {
      limits <- range(train_df[[cur_column()]], na.rm = TRUE)
      pmin(pmax(x, limits[1]), limits[2])
    }))
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

quality_scale <- c("None", "Po", "Fa", "TA", "Gd", "Ex")
bsmt_fin_scale <- c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ")

ordinal_scales <- list(
  ExterQual = quality_scale, ExterCond = quality_scale,
  BsmtQual = quality_scale, BsmtCond = quality_scale,
  HeatingQC = quality_scale, KitchenQual = quality_scale,
  FireplaceQu = quality_scale, GarageQual = quality_scale,
  GarageCond = quality_scale, PoolQC = quality_scale,
  BsmtFinType1 = bsmt_fin_scale, BsmtFinType2 = bsmt_fin_scale,
  BsmtExposure = c("None", "No", "Mn", "Av", "Gd"),
  GarageFinish = c("None", "Unf", "RFn", "Fin"),
  Functional   = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"),
  Fence        = c("None", "MnWw", "GdWo", "MnPrv", "GdPrv"),
  LotShape     = c("IR3", "IR2", "IR1", "Reg"),
  LandSlope    = c("Sev", "Mod", "Gtl"),
  PavedDrive   = c("N", "P", "Y")
)

# NA means missing, not absent, for these two -- "None" has no slot on their scale.
ordinal_na_default <- list(Functional = "Typ", KitchenQual = "TA")

ordinal_rank <- function(x, scale, column){
  unmapped <- setdiff(unique(x), scale)
  if (length(unmapped)) {
    stop("unmapped level in ", column, ": ", paste(unmapped, collapse = ", "), call. = FALSE)
  }
  match(x, scale) - 1L
}

# rare_min/max_modal_share act on the combined train+test frame, matching what
# the model encoding actually sees.
prepare_df <- function(df, rare_min = 20, max_modal_share = 0.98){
  response <- "SalePrice"

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
    # GarageQual/Cond/YrBlt are redundant once GarageCars is 0.
    select(
      -TotalBsmtSF, -X1stFlrSF, -X2ndFlrSF,
      -YearBuilt, -YearRemodAdd,
      -GarageQual, -GarageCond, -GarageYrBlt,
      -FullBath, -HalfBath, -BsmtFullBath, -BsmtHalfBath,
      -OpenPorchSF, -EnclosedPorch, -X3SsnPorch, -ScreenPorch, -WoodDeckSF
    ) %>%
    # Must run before ordinal encoding: near_constant only recognises
    # character/factor columns.
    select(-where(near_constant)) %>%
    mutate(
      across(any_of(response), log),
      across(any_of(names(ordinal_na_default)),
             \(x) replace(x, x == "None", ordinal_na_default[[cur_column()]])),
      across(any_of(names(ordinal_scales)),
             \(x) ordinal_rank(x, ordinal_scales[[cur_column()]], cur_column()))
    ) %>%
    mutate(across(where(is.character), ~ fct_lump_min(.x, min = rare_min, other_level = "Other"))) %>%
    # Second pass: lumping rare levels into "Other" could push a column past the threshold too.
    select(-where(near_constant)) %>%
    mutate(across(where(is.character), as.factor)) # Format as factor at the end to prevent factor level weirdness.
}

# --- Fitting -----------------------------------------------------------------

train_svm <- function(
  dataset,
  gamma,                     # no library default: always tuned explicitly below
  kernel = "radial",
  cost = 1,
  epsilon = 0.1,
  degree = 3,                # polynomial only
  scale = TRUE
){
  e1071::svm(
    reformulate(".", response = dataset$predictor),
    data = dataset$df,
    type = "eps-regression",
    kernel = kernel,
    cost = cost,
    gamma = gamma,
    epsilon = epsilon,
    degree = degree,
    scale = scale
  )
}

# glmnet/xgboost need a numeric design matrix; terms/xlev travel with the
# fitted model so predict() rebuilds identical columns on unseen rows.
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

# lambda pinned rather than cross-validated per fold: cv.glmnet's lambda.min
# stayed within 0.0017-0.0083 across 25 outer folds, so an inner search mostly
# re-derived one number at 6x the cost.
train_glmnet <- function(dataset, alpha = 0.5, lambda = 0.006){
  parts <- model_matrix_parts(dataset)
  fit <- glmnet::glmnet(parts$x, parts$y, alpha = alpha)
  model <- new_matrix_model(fit, parts, "glmnet_model")
  model$lambda <- lambda
  model
}

predict.glmnet_model <- function(object, newdata, ...){
  as.numeric(predict(object$model, newx = matrix_newx(object, newdata), s = object$lambda))
}

train_xgb <- function(
  dataset,
  nrounds = 600,
  eta = 0.05,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  nthread = 0,              # 0 -> all cores
  seed = 42                 # subsample/colsample draw from xgboost's own RNG,
){                          # not R's -- left unset it silently varies per fit
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
      nthread = nthread,
      seed = seed
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

# xgb keeps a quarter despite being the weakest single model -- its residuals
# are least correlated with the other two.
blend_weights <- c(svm = 0.50, glmnet = 0.25, xgb = 0.25)

# Smoothed toward the global median by m pseudo-houses so thin neighborhoods
# don't carry a noisy ratio.
neighborhood_ppsf <- function(neighborhood, price, sf, m = 30){
  global <- median(price / sf)
  tapply(seq_along(neighborhood), neighborhood, function(idx){
    ratio <- price[idx] / sf[idx]
    (length(idx) * median(ratio) + m * global) / (length(idx) + m)
  }) %>% c(.default = global)
}

apply_ppsf <- function(map, neighborhood){
  unname(ifelse(is.na(map[as.character(neighborhood)]), map[[".default"]],
                map[as.character(neighborhood)]))
}

# Wraps a fitter to add the implied-price column. Training rows get an
# inner-CV encoding rather than one built from their own price, so the
# feature can't leak each row's answer into itself.
with_neighborhood_ppsf <- function(fit, m = 30, inner_k = 5, seed = 1){
  function(dataset){
    train <- dataset$df
    price <- exp(train[[dataset$predictor]])
    sf <- train$TotalSF
    inner <- with_seed(seed, sample(rep_len(seq_len(inner_k), nrow(train))))

    encoded <- map(seq_len(inner_k), function(i){
      fold_map <- neighborhood_ppsf(train$Neighborhood[inner != i], price[inner != i],
                                    sf[inner != i], m)
      tibble(row = which(inner == i),
             value = apply_ppsf(fold_map, train$Neighborhood[inner == i]))
    }) %>%
      bind_rows() %>%
      arrange(row) %>%
      pull(value)

    full_map <- neighborhood_ppsf(train$Neighborhood, price, sf, m)
    add_feature <- function(d, ppsf){ d$NbhdImpliedPrice <- log(ppsf * d$TotalSF); d }

    model <- fit(list(predictor = dataset$predictor, df = add_feature(train, encoded)))
    structure(list(model = model, map = full_map, add_feature = add_feature),
              class = "nbhd_ppsf_model")
  }
}

predict.nbhd_ppsf_model <- function(object, newdata, ...){
  predict_model(object$model,
                object$add_feature(newdata, apply_ppsf(object$map, newdata$Neighborhood)))
}

fitters <- list(
  # gamma pinned rather than left at e1071's 1/n_columns default, which drifts
  # as columns are added or removed.
  svm    = with_neighborhood_ppsf(function(d) train_svm(d, cost = 30, gamma = 0.001)),
  glmnet = with_neighborhood_ppsf(function(d) train_glmnet(d)),
  xgb    = with_neighborhood_ppsf(function(d) train_xgb(d, nrounds = 1200, eta = 0.03,
                                                        max_depth = 3, min_child_weight = 3,
                                                        nthread = 2))
)

train_blend <- function(dataset, weights = blend_weights){
  structure(
    list(models = map(fitters[names(weights)], ~ .x(dataset)), weights = weights),
    class = "blend"
  )
}

predict.blend <- function(object, newdata, ...){
  preds <- map(object$models, predict_model, newdata = newdata)
  as.numeric(as.matrix(as_tibble(preds)) %*% object$weights[names(preds)])
}

# Corrects under-dispersion in the blend's predictions (regressed on actuals,
# they come out with slope > 1). Must be fit on out-of-fold predictions --
# in-sample ones return a slope near 1 and correct nothing.
calibrate <- function(model, oof, weights = blend_weights){
  oof_pred <- as.numeric(as.matrix(oof[names(weights)]) %*% weights)
  structure(
    list(model = model,
         fit = lm(actual ~ oof_pred, data.frame(actual = oof$actual, oof_pred = oof_pred))),
    class = "calibrated"
  )
}

predict.calibrated <- function(object, newdata, ...){
  oof_pred <- predict_model(object$model, newdata)
  as.numeric(predict(object$fit, data.frame(oof_pred = oof_pred)))
}

# --- Evaluation --------------------------------------------------------------

rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))

predict_model <- function(model, newdata) as.numeric(predict(model, newdata))

# Fits every model per fold once; per-model scores and any blend derive from
# these out-of-fold predictions. Folds are drawn up front since fitting
# consumes RNG state, which would otherwise unpair the comparison.
#
# train_filter runs on each fold's training split only, never on the held-out
# rows that get scored -- this is what lets the honest-CV run below exclude
# the GrLivArea outliers from fitting without also excusing them from scoring.
oof_predictions <- function(dataset, fitters, k = 5, repeats = 5, seed = 42, workers = 1,
                             train_filter = identity){
  set.seed(seed)
  df <- dataset$df
  response <- dataset$predictor
  fold_sets <- map(seq_len(repeats), ~ sample(rep_len(seq_len(k), nrow(df))))
  jobs <- expand_grid(rep = seq_len(repeats), fold = seq_len(k))

  run <- function(j){
    folds <- fold_sets[[jobs$rep[j]]]
    i <- jobs$fold[j]
    train <- list(predictor = response, df = train_filter(df[folds != i, ]))
    held <- df[folds == i, ]
    bind_cols(
      tibble(rep = jobs$rep[j], fold = i, row = which(folds == i), actual = held[[response]]),
      as_tibble(map(fitters, ~ predict_model(.x(train), held)))
    )
  }

  out <- if (workers > 1) parallel::mclapply(seq_len(nrow(jobs)), run, mc.cores = workers)
         else map(seq_len(nrow(jobs)), run)
  bind_rows(out)
}

cv_scores <- function(oof, weights = NULL){
  models <- setdiff(names(oof), c("rep", "fold", "row", "actual"))
  fold_id <- paste(oof$rep, oof$fold)
  # SE across folds, not repeats: repeat-level RMSEs span every row and differ
  # only by shuffling, understating uncertainty ~6x.
  score <- function(p){
    by_fold <- split(seq_len(nrow(oof)), fold_id) %>% map_dbl(~ rmse(oof$actual[.x], p[.x]))
    tibble(cv_rmse = rmse(oof$actual, p), cv_se = sd(by_fold) / sqrt(length(by_fold)))
  }

  out <- map_dfr(models, ~ bind_cols(tibble(model = .x), score(oof[[.x]])))
  if (!is.null(weights)) {
    blended <- as.matrix(oof[names(weights)]) %*% weights
    out <- bind_rows(out, bind_cols(tibble(model = "blend"), score(blended)))
  }
  arrange(out, cv_rmse)
}

# Grid over the weight simplex. The optimum is flat; read the top rows, not row 1.
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

write_submission <- function(model, dataset, path, back_transform = exp){
  stopifnot(!is.null(dataset$test))

  # SalePrice is NA in test; drop it or predict.svm silently na.omits those rows.
  newdata <- dataset$test %>% select(-any_of(dataset$predictor))
  predicted <- predict_model(model, newdata)
  stopifnot(length(predicted) == length(dataset$test_ids))

  out <- tibble(Id = dataset$test_ids, SalePrice = back_transform(predicted))
  write.csv(out, path, row.names = FALSE)

  cat(sprintf("%s: %d rows, predicted $%.0f - $%.0f (median $%.0f)\n",
              path, nrow(out), min(out$SalePrice), max(out$SalePrice), median(out$SalePrice)))
  invisible(out)
}

# --- Run -----------------------------------------------------------------

# No holdout: repeated full-data CV estimates better than one 292-row split
# (SE 0.002 vs 0.006).

# Two >4000 sqft houses sold as partial sales far under market. Filtered from
# both fit and CV, so the CV RMSE below reads ~0.010 better than the
# leaderboard. Do not switch to scoring-only exclusion: tried once, cost
# 0.0045 on the leaderboard even though CV preferred it.
raw <- read.csv("train.csv", na.strings = c("NA", "")) %>%
  filter(!(GrLivArea > 4000 & SalePrice < 300000))
test_raw <- read.csv("test.csv", na.strings = c("NA", ""))

dataset <- make_dataset(raw, "SalePrice", test_df = test_raw)

oof <- oof_predictions(dataset, fitters, k = 5, repeats = 5, workers = 5)
print(cv_scores(oof, blend_weights))

# --- Kaggle submission --------------------------------------------------

blend_fit <- train_blend(dataset)
calibrated_fit <- calibrate(blend_fit, oof)  # reuses oof; fits nothing new

cat(sprintf("\nCalibration slope: %.4f\n", coef(calibrated_fit$fit)[2]))
cat("\nSubmissions:\n")
blend_submission <- write_submission(calibrated_fit, dataset, "submission_blend.csv")
