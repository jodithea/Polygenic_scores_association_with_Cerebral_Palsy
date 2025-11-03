# Results for variance explained in CP risk on the liability scale attributable to each PGS
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

pgs_vars <- c("PGS_CP_SBayesRC", "PGS_EA_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Stroke_SBayesRC",
              "PGS_Gestdur_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Agewalking_SBayesRC")

# VarExplainedLiabilityScaleFunction ----
# Variance explained by the PGS on the liability scale

# Convert observed R2 to the liability scale using CP prevalence of 0.003 (3 in 1000 = 0.3% = 0.003)
# Use R code from Lee SH, et al., A Better Coefficient of Determination for Genetic Profile Analysis. Genetic Epidemiology, 2012. 36(3):214-224. 
# Made into a function
# lm = linear model
# K = population prevalence
# P = sample prevalence

mapToLiabilityScale = function(lm, K, P){
  thd = qnorm(1 - K) #the threshold on the normal distribution which truncates the proportion of disease prevalence K
  zv = dnorm(thd) # z (normal density)
  mv = zv/K # mean liability for case
  mv2 = -mv*K/(1-K) # mean liability for control
  
  y = lm$model[[1]]
  ncase = sum(y == 1)
  nt = length(y)
  ncont = nt - ncase
  R2O = var(lm$fitted.values)/(ncase/nt*ncont/nt) # R2 on the observed scale 
  
  theta = mv*(P-K)/(1-K)*(mv*(P-K)/(1-K)-thd) # theta in equation 15 of the publication
  cv = K*(1-K)/zv^2*K*(1-K)/(P*(1-P)) # C in equation 15 of the publication
  R2 = R2O*cv/(1+R2O*theta*cv)
  
  return(R2)
}

# VarExplainedLiabilityScale ----
# For each model with only one PGS
# Calculate difference in R2 on liability scale of full model - covariates only model to identify variance attributable to PGS
# Bootstrap to get 90% CI
# Doing a one-sided test of significance (> 0) therefore use a one-sided 95% CI (equivalent to just the lower bound of a 90% CI)

r2l_function <- function(data) {
  set.seed(123)  # for reproducibility
  n_boot <- 1000
  
  # Univariate models
  PGS_r2l_univ_df <- purrr::map(pgs_vars, function(pgs) {
    r2_diffs_univ <- numeric(n_boot)
    
    for (i in 1:n_boot) {
      # Resample data with replacement
      boot_data <- data[sample(1:nrow(data), replace = TRUE), ]
      
      # Fit models with resampled data
      lm_cov <- lm(CP_all ~ Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = boot_data)
      
      formula_univ <- as.formula(paste0("CP_all ~ scale(", pgs, ") + Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10"))
      lm_univ <- lm(formula_univ, data = boot_data)
      
      # Calculate sample prevalence in the bootstrap sample
      ncase <- sum(boot_data$CP_all == 1)
      P_boot <- ncase / nrow(boot_data)
      
      # Liability R²
      r2_cov <- mapToLiabilityScale(lm_cov, K = 0.0014, P = P_boot)
      r2_univ <- mapToLiabilityScale(lm_univ, K = 0.0014, P = P_boot)
      
      r2_diffs_univ[i] <- r2_univ - r2_cov
    }
    
    # 90% confidence interval
    ci_lower_univ <- quantile(r2_diffs_univ, 0.05)
    ci_upper_univ <- quantile(r2_diffs_univ, 0.95)
    
    # standard error
    r2_se_univ <- sd(r2_diffs_univ) / sqrt(n_boot)
    
    # Mean (point estimate from bootstrapped distribution)
    r2_PGS_univ <- mean(r2_diffs_univ)
    
    # Z-test for r2l > 0
    r2_z_univ <- r2_PGS_univ / r2_se_univ
    p_val_univ <- pnorm(r2_z_univ, mean = 0, sd = 1, lower.tail = F)
    
    # Make tibble with R2 and CI 
    tibble(
      term = paste0("scale(", pgs, ")"),
      r2l = r2_PGS_univ,
      r2l_SE = r2_se_univ,
      r2l_Z = r2_z_univ,
      r2l_p = p_val_univ,
      r2l_90perc_CI_lower = ci_lower_univ,
      r2l_90perc_CI_upper = ci_upper_univ,
      PGS = pgs,
      model = "univariate"
    )
  }) %>% bind_rows()
  
  # Multivariate model
  r2_diffs_multi <- numeric(n_boot)
  
  for (i in 1:n_boot) {
    # Resample data with replacement
    boot_data <- data[sample(1:nrow(data), replace = TRUE), ]
    
    # Fit models on resampled data
    lm_cov <- lm(CP_all ~ Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = boot_data)
    
    lm_full <- lm(CP_all ~ PGS_CP_SBayesRC + PGS_EA_SBayesRC + PGS_Birthweight_SBayesRC + 
                    PGS_Stroke_SBayesRC + PGS_Gestdur_SBayesRC + PGS_Epilepsy_SBayesRC + 
                    PGS_Autism_SBayesRC + PGS_Agewalking_SBayesRC + 
                    Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
                  data = boot_data)
    
    # Calculate sample prevalence in the bootstrap sample
    ncase <- sum(boot_data$CP_all == 1)
    P_boot <- ncase / nrow(boot_data)
    
    # Liability R²
    r2_cov <- mapToLiabilityScale(lm_cov, K = 0.0014, P = P_boot)
    r2_full <- mapToLiabilityScale(lm_full, K = 0.0014, P = P_boot)
    
    r2_diffs_multi[i] <- r2_full - r2_cov
  }
  
  # 90% confidence interval
  ci_lower_multiv <- quantile(r2_diffs_multi, 0.05)
  ci_upper_multiv <- quantile(r2_diffs_multi, 0.95)
  
  # Mean (point estimate from bootstrapped distribution)
  r2_PGS_multiv <- mean(r2_diffs_multi)
  
  # standard error
  r2_se_multiv <- sd(r2_diffs_multi) / sqrt(n_boot)
  
  # Z-test for r2l > 0
  r2_z_multiv <- r2_PGS_multiv / r2_se_multiv
  p_val_multiv <- pnorm(r2_z_multiv, mean = 0, sd = 1, lower.tail = F)
  
  # Make tibble with R2 and CI 
  PGS_r2l_multiv_df <- tibble(
    term = "scale(all_PGS)",
    r2l = r2_PGS_multiv,
    r2l_SE = r2_se_multiv,
    r2l_Z = r2_z_multiv,
    r2l_p = p_val_multiv,
    r2l_90perc_CI_lower = ci_lower_multiv,
    r2l_90perc_CI_upper = ci_upper_multiv,
    PGS = "all_PGS",
    model = "multivariate"
  )
  
  # Combine into one dataset with univariate models + multivariate model and adjust p-value
  bind_rows(PGS_r2l_univ_df, PGS_r2l_multiv_df) %>%
    mutate(r2l_padj = p.adjust(r2l_p, method = "BH", n = 9))
}

r2l_df_full <- r2l_function(CP_df_not_related)

save(r2l_df_full,
     file = "/Results/r2l_df.RData")

# FullR2lPrint
r2l_df_full %>% 
  kable(caption = "Variance explained in CP risk on the liability scale attributable to each PGS or all PGS in same model (full dataset) (as a proportion, multiply by 100 for a %)")

