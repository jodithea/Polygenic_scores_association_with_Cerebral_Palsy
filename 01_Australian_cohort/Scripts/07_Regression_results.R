# Results from regression models

## ---- LoadDataFrame
load(file = "Results/Regression_models_with_covar.RData")
load(file = "Results/Regression_models_no_covar.RData")

load(file = "Data/PGS_variables.RData")
load(file = "Data/PGS_names.RData")
## ----

## ---- ResultsRegr
regression_df_function <- function(regression_data) {
  
  PGS_regr_univ_df <- map(pgs_vars, function(pgs) {
    tidy(regression_data$univariate_regression[[pgs]], conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>%
      filter(term == paste0("scale(", pgs, ")")) %>%
      rename(
        regr_OR = estimate,
        regr_SE = std.error,
        regr_Z = statistic,
        regr_p = p.value,
        regr_95perc_CI_lower = conf.low,
        regr_95perc_CI_upper = conf.high
      ) %>%
      mutate(regr_padj = p.adjust(regr_p, method = "BH", n = 7),,
             PGS = pgs,
             model = "univariate")
  }) %>%
    list_rbind()
  
  
  CP_all_pgs_model_df <- tidy(regression_data$multivariate_regression, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
    filter(str_detect(term, "PGS")) %>%
    rename(
      regr_OR = estimate,
      regr_SE = std.error,
      regr_Z = statistic,
      regr_p = p.value,
      regr_95perc_CI_lower = conf.low,
      regr_95perc_CI_upper = conf.high
    ) %>% 
    mutate(regr_padj = p.adjust(regr_p, method = "BH", n = 8),
           PGS = str_extract(term, "(?<=scale\\().+?(?=\\))"),
           model = "multivariate")
  
  PGS_regr_univ_df %>% 
    bind_rows(CP_all_pgs_model_df)
  
}

regression_df_full_with_covar <- regression_df_function(regression_full_with_covar)
regression_df_monogenic_with_covar <- regression_df_function(regression_monogenic_with_covar)
regression_df_non_monogenic_with_covar <- regression_df_function(regression_non_monogenic_with_covar)
regression_df_young525_with_covar <- regression_df_function(regression_young525_with_covar)

save(regression_df_full_with_covar,
     regression_df_monogenic_with_covar,
     regression_df_non_monogenic_with_covar, 
     regression_df_young525_with_covar,
     file = "Results/regression_with_covar_df.RData")

regression_df_full_no_covar <- regression_df_function(regression_full_no_covar)
regression_df_monogenic_no_covar <- regression_df_function(regression_monogenic_no_covar)
regression_df_non_monogenic_no_covar <- regression_df_function(regression_non_monogenic_no_covar)
regression_df_young525_no_covar <- regression_df_function(regression_young525_no_covar)

save(regression_df_full_no_covar,
     regression_df_monogenic_no_covar,
     regression_df_non_monogenic_no_covar, 
     regression_df_young525_no_covar,
     file = "Results/regression_no_covar_df.RData")
## ----

## ----FullResultsWithCovarRegrPrint
regression_df_full_with_covar %>% 
  kable(caption = "Regression results (full dataset, with covariates)")
## ----

## ----FullResultsNoCovarRegrPrint
regression_df_full_no_covar %>% 
  kable(caption = "Regression results (full dataset, no covariates)")
## ----

## ----MonoResultsWithCovarRegrPrint
regression_df_monogenic_with_covar %>% 
  kable(caption = "Regression results (individuals with a monogenic diagnosis only, with covariates)")
## ----

## ----MonoResultsNoCovarRegrPrint
regression_df_monogenic_no_covar %>% 
  kable(caption = "Regression results (individuals with a monogenic diagnosis only, no covariates)")
## ----

## ----NonmonoResultsWithCovarRegrPrint
regression_df_non_monogenic_with_covar %>% 
  kable(caption = "Regression results (individuals without a monogenic diagnosis, with covariates)")
## ----

## ----NonmonoResultsNoCovarRegrPrint
regression_df_non_monogenic_no_covar %>% 
  kable(caption = "Regression results (individuals without a monogenic diagnosis, no covariates)")
## ----

## ----Young525ResultsWithCovarRegrPrint
regression_df_young525_with_covar %>% 
  kable(caption = "Regression results (controls restricted to youngest 525 dataset, with covariates)")
## ----

## ----Young525ResultsNoCovarRegrPrint
regression_df_young525_no_covar %>% 
  kable(caption = "Regression results (controls restricted to youngest 525 dataset, no covariates)")
## ----

