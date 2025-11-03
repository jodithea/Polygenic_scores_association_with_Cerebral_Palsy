# Results from regression models
# Load libraries ----
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(vegan)
library(Hmisc)
library(ggthemes)
library(scales)
library(patchwork)
library(tidyverse)
library(reshape2)
library(lme4)
library(RColorBrewer)
library(MASS)
library(broom)
library(knitr)
library(lavaan)
library(metafor)
library(ggsci)
library(twoxtwo)
library(parallel)
library(ggh4x)
library(MatchIt)
library(haven)
library(labelled)
library(performance)  # for model checking
library(DHARMa)       # for model checking
library(effects)      # for nice summary of model results (allEffects)
library(broom.mixed)  # for tidy output
library(ggplotify)    # to use patchwork on lattice plots (created by allEffects)
library(MuMIn)        # to calculate R2
library(forestmodel)  # for nicely formatted caterpillar plot
library(pROC)         # for ROC curves and AUC

# LoadDataFrame ----
load(file = "/Results/CP_df_not_related.RData")

load(file = "/Results/Regression_models_with_covar.RData")
load(file = "/Results/Regression_models_no_covar.RData")

pgs_vars <- c("PGS_CP_SBayesRC", "PGS_EA_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Stroke_SBayesRC",
              "PGS_Gestdur_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Agewalking_SBayesRC")

# ResultsRegr ----
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

save(regression_df_full_with_covar,
     file = "/Results/regression_with_covar_df.RData")

regression_df_full_no_covar <- regression_df_function(regression_full_no_covar)

save(regression_df_full_no_covar,
     file = "/Results/regression_no_covar_df.RData")

# FullResultsWithCovarRegrPrint ----
regression_df_full_with_covar %>% 
  kable(caption = "Regression results (full dataset, with covariates)")


# FullResultsNoCovarRegrPrint ----
regression_df_full_no_covar %>% 
  kable(caption = "Regression results (full dataset, no covariates)")
