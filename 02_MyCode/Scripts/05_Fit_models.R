# Fit regression models
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

# RegrModelsWithCovar ----
regression_function <- function(data) {
 
  # Fit logistic regression model for each PGS, including covariates
  function_cp_pgs_model_fit <- function(pgs_var) {
    
    formula <- as.formula(paste("CP_all ~ scale(", pgs_var, ") + Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10"))
    glm(formula, family = binomial(link = 'logit'), data = data)
  }
  
  CP_pgs_models <- map(pgs_vars, function_cp_pgs_model_fit)
  
  # Name the models for easy access later
  names(CP_pgs_models) <- pgs_vars
  
  
  # Regression model with all PGS included, including covariates
  CP_all_pgs_model <- glm(CP_all ~ scale(PGS_CP_SBayesRC) + scale(PGS_EA_SBayesRC) + scale(PGS_Birthweight_SBayesRC) + 
                            scale(PGS_Stroke_SBayesRC) + scale(PGS_Gestdur_SBayesRC) + scale(PGS_Epilepsy_SBayesRC) + 
                            scale(PGS_Autism_SBayesRC) + scale(PGS_Agewalking_SBayesRC) +
                            Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
                          family = binomial(link = 'logit'),
                          data = data)
  
  list(
    univariate_regression = CP_pgs_models,
    multivariate_regression = CP_all_pgs_model
  )


}

regression_full_with_covar <- regression_function(CP_df_not_related)

save(regression_full_with_covar, 
     file = "/Results/Regression_models_with_covar.RData")


# RegrModelsNoCovar ----
regression_function <- function(data) {
  
  # Fit logistic regression model for each PGS (no caovariates)
  function_cp_pgs_model_fit <- function(pgs_var) {
    
    formula <- as.formula(paste("CP_all ~ scale(", pgs_var, ")"))
    glm(formula, family = binomial(link = 'logit'), data = data)
  }
  
  CP_pgs_models <- map(pgs_vars, function_cp_pgs_model_fit)
  
  # Name the models for easy access later
  names(CP_pgs_models) <- pgs_vars
  
  
  # Regression model with all PGS included (no covariates)
  CP_all_pgs_model <- glm(CP_all ~ scale(PGS_CP_SBayesRC) + scale(PGS_EA_SBayesRC) + scale(PGS_Birthweight_SBayesRC) + 
                            scale(PGS_Stroke_SBayesRC) + scale(PGS_Gestdur_SBayesRC) + scale(PGS_Epilepsy_SBayesRC) + 
                            scale(PGS_Autism_SBayesRC) + scale(PGS_Agewalking_SBayesRC),
                          family = binomial(link = 'logit'),
                          data = data)
  
  list(
    univariate_regression = CP_pgs_models,
    multivariate_regression = CP_all_pgs_model
  )
  
  
}

regression_full_no_covar <- regression_function(CP_df_not_related)

save(regression_full_no_covar, 
     file = "/Results/Regression_models_no_covar.RData")

# ROCFitPGS ----
roc_function <- function(data) {
  # 1 PGS in each ROC model
  function_cp_pgs_roc_fit <- function(pgs_var) {
    
    formula <- as.formula(paste("CP_all ~ scale(", pgs_var, ")"))
    model <- glm(formula, family = binomial(link = 'logit'), data = data)
    roc(data$CP_all, model$fitted.values)
    
  }
  
  CP_pgs_roc <- map(pgs_vars, function_cp_pgs_roc_fit)
  
  # Name the models for easy access later
  names(CP_pgs_roc) <- pgs_vars
  
  # All PGS in same ROC model
  model <- glm(CP_all ~ scale(PGS_CP_SBayesRC) + scale(PGS_EA_SBayesRC) + scale(PGS_Birthweight_SBayesRC) + 
                 scale(PGS_Stroke_SBayesRC) + scale(PGS_Gestdur_SBayesRC) + scale(PGS_Epilepsy_SBayesRC) + 
                 scale(PGS_Autism_SBayesRC) + scale(PGS_Agewalking_SBayesRC),
               family = binomial(link = 'logit'), 
               data = data)
  CP_all_pgs_roc <- roc(data$CP_all, model$fitted.values)
  
  list(
    univariate_roc = CP_pgs_roc,
    multivariate_roc = CP_all_pgs_roc
  )

}

roc_full <- roc_function(CP_df_not_related)

save(roc_full,
     file = "/Results/roc_models.RData")

