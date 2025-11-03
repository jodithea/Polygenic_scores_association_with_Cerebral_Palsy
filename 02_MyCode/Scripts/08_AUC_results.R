# Results from AUC of ROC
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

load(file = "/Results/roc_models.RData")

pgs_vars <- c("PGS_CP_SBayesRC", "PGS_EA_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Stroke_SBayesRC",
              "PGS_Gestdur_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Agewalking_SBayesRC")



# AUCResults ----
# Doing a one-sided test of significance (> 0.5) therefore use a one-sided 95%CI (equivalent to just the lower bound of a 90% CI)

roc_df_function <- function(roc_data) {
  PGS_univ_roc_df <- map(pgs_vars, function(pgs) {
    model <- roc_data$univariate_roc[[pgs]]
    
    auc_value <- as.numeric(auc(model))
    auc_ci <- as.numeric(ci(model, conf.level = 0.90))
    auc_se <- sqrt(var(model))
    auc_z <- (auc_value - 0.5) / auc_se
    auc_p <- pnorm(auc_z, mean = 0, sd = 1, lower.tail = F)
    
    tibble(
      term = paste0("scale(", pgs, ")"),
      AUC = auc_value,
      AUC_SE = auc_se,
      AUC_Z = auc_z,
      AUC_p = auc_p,
      AUC_90perc_CI_lower = auc_ci[1],
      AUC_90perc_CI_upper = auc_ci[3],
      PGS = pgs,
      model = "univariate"
    ) 
  }) %>%
    bind_rows()
  
  PGS_multiv_roc_df <- tibble(
    term = "scale(all_PGS)",
    AUC = as.numeric(auc(roc_data$multivariate_roc)),
    AUC_SE = sqrt(var(roc_data$multivariate_roc)),
    AUC_Z = (AUC - 0.5) / AUC_SE,
    AUC_p = pnorm(AUC_Z, mean = 0, sd = 1, lower.tail = F),
    AUC_90perc_CI_lower = as.numeric(ci(roc_data$multivariate_roc, conf.level = 0.90))[1],
    AUC_90perc_CI_upper = as.numeric(ci(roc_data$multivariate_roc, conf.level = 0.90))[3],
    PGS = "all_PGS",
    model = "multivariate"
  )
  
  
  PGS_univ_roc_df %>% 
    bind_rows(PGS_multiv_roc_df) %>% 
    mutate(AUC_padj = p.adjust(AUC_p, method = "BH", n = 9))
  
}

roc_df_full <- roc_df_function(roc_full)

save(roc_df_full,
     file = "/Results/roc_df.RData")

# -FullResultsAUCPrint ----
roc_df_full %>% 
  kable(caption = "AUC results (full dataset)")

