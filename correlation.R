library(tidyverse)

vars <- c(
  "MSZoning",
  "LotFrontage",
  "Street",
  "Alley",
  "LandContour",
  "LotConfig",
  "LandSlope",
  "Neighborhood",
  "Condition1",
  "Condition2"
)

# Categorical variables only


df_num_cor <- function(df, var_of_int){
  df %>%
    select(where(is.numeric), sym(var_of_int)) %>%
    select(-sym(var_of_int)) %>%
    summarise(
      across(
        everything(),
        list(
          pearson = ~ cor(.x, df[[var_of_int]], use = "complete.obs", method = "pearson"),
          spearman = ~ cor(.x, df[[var_of_int]], use = "complete.obs", method = "spearman")
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable_metric",
      values_to = "association"
    ) %>%
    separate(
      variable_metric,
      into = c("variable", "metric"),
      sep = "_(?=[^_]+$)"
    ) %>%
    mutate(
      abs_association = abs(association),
      type = "numeric"
    ) %>%
    group_by(variable) %>% 
    summarise(mean = mean(abs_association)) %>% 
    arrange(desc(mean))
}

df_cat_cor <- function(df, var_of_int){
  
  eta_squared <- function(x, y) {
    
    tmp <- tibble(
      x = as.factor(x),
      y = y
    ) %>%
      filter(!is.na(x), !is.na(y))
    
    if (n_distinct(tmp$x) < 2) return(NA_real_)
    
    grand_mean <- mean(tmp$y)
    
    ss_between <- tmp %>%
      group_by(x) %>%
      summarise(
        n = n(),
        mean_y = mean(y),
        .groups = "drop"
      ) %>%
      summarise(
        ss = sum(n * (mean_y - grand_mean)^2)
      ) %>%
      pull(ss)
    
    ss_total <- sum((tmp$y - grand_mean)^2)
    
    ss_between / ss_total
  }
  
  df %>%
    select(
      where(\(x) is.character(x) || is.factor(x)),
      all_of(var_of_int)
    ) %>%
    select(-all_of(var_of_int)) %>%
    summarise(
      across(
        everything(),
        ~ eta_squared(.x, df[[var_of_int]])
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = "variable",
      values_to = "association"
    ) %>%
    mutate(
      rank = dense_rank(desc(association))
    ) %>%
    arrange(rank)
}


df_num_cor(train,"SalePrice")
df_cat_cor(train,"SalePrice")

df_char_cor <- function(df, var_of_int){
  
  cramers_v <- function(x, y) {
    tbl <- table(x, y, useNA = "no")
    if (min(dim(tbl)) < 2) return(NA_real_)
    chi <- suppressWarnings(chisq.test(tbl, correct = FALSE))
    n <- sum(tbl)
    r <- nrow(tbl)
    k <- ncol(tbl)
    sqrt(as.numeric(chi$statistic) / (n * min(r - 1, k - 1)))
  }
  
  cat_df <- df %>% 
    select(where(\(x) is.character(x) || is.factor(x)), sym(var_of_int)) %>% 
    select(-sym(var_of_int))
  cat_var_names <- names(cat_df)

  assoc_ranked <- expand.grid(
    var1 = cat_var_names,
    var2 = cat_var_names,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    filter(var1 < var2) %>%   # Removes duplicates and self-pairs
    mutate(
      association = map2_dbl(
        var1,
        var2,
        ~ cramers_v(cat_df[[.x]], cat_df[[.y]])
      )
    ) %>%
    arrange(desc(association))

print(assoc_ranked,n= 36)

numeric_price_assoc %>%
  group_by(variable) %>% 
  summarise(mean = mean(association)) %>% 
  slice_max(mean, n=10)
