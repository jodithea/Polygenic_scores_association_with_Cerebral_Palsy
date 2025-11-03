# Fit regression models

## ---- LoadDataFrame
load(file = "Data/CP_df_not_related.Rdata")
load(file = "Data/Monogenic_only_df.RData")
load(file = "Data/Non_monogenic_df.RData")
load(file = "Data/Young525_df.RData")

load(file = "Data/PGS_variables.RData")
load(file = "Data/PGS_names.RData")
## ----

## ---- RegrModelsWithCovar
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
  CP_all_pgs_model <- glm(CP_all ~ scale(PGS_CP_FinnGen_SBayesRC) + scale(PGS_EA_SBayesRC) + scale(PGS_Birthweight_SBayesRC) + scale(PGS_Stroke_SBayesRC) + scale(PGS_Gestdur_SBayesRC) + scale(PGS_Epilepsy_SBayesRC) + scale(PGS_Autism_SBayesRC) + scale(PGS_Agewalking_SBayesRC) +
                            Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10,
                          family = binomial(link = 'logit'),
                          data = data)
  
  list(
    univariate_regression = CP_pgs_models,
    multivariate_regression = CP_all_pgs_model
  )


}

regression_full_with_covar <- regression_function(CP_df_not_related)
regression_monogenic_with_covar <- regression_function(Monogenic_only)
regression_non_monogenic_with_covar <- regression_function(Non_monogenic)
regression_young525_with_covar <- regression_function(Young525)

save(regression_full_with_covar,
     regression_monogenic_with_covar,
     regression_non_monogenic_with_covar,
     regression_young525_with_covar,
     file = "Results/Regression_models_with_covar.RData")

## ----

## ---- RegrModelsNoCovar
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
  CP_all_pgs_model <- glm(CP_all ~ scale(PGS_CP_FinnGen_SBayesRC) + scale(PGS_EA_SBayesRC) + scale(PGS_Birthweight_SBayesRC) + scale(PGS_Stroke_SBayesRC) + scale(PGS_Gestdur_SBayesRC) + scale(PGS_Epilepsy_SBayesRC) + scale(PGS_Autism_SBayesRC) + scale(PGS_Agewalking_SBayesRC),
                          family = binomial(link = 'logit'),
                          data = data)
  
  list(
    univariate_regression = CP_pgs_models,
    multivariate_regression = CP_all_pgs_model
  )
  
  
}

regression_full_no_covar <- regression_function(CP_df_not_related)
regression_monogenic_no_covar <- regression_function(Monogenic_only)
regression_non_monogenic_no_covar <- regression_function(Non_monogenic)
regression_young525_no_covar <- regression_function(Young525)

save(regression_full_no_covar,
     regression_monogenic_no_covar,
     regression_non_monogenic_no_covar,
     regression_young525_no_covar,
     file = "Results/Regression_models_no_covar.RData")

## ----



## ---- ROCFitPGS
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
  model <- glm(CP_all ~ scale(PGS_CP_FinnGen_SBayesRC) + scale(PGS_EA_SBayesRC) + scale(PGS_Birthweight_SBayesRC) + scale(PGS_Stroke_SBayesRC) + scale(PGS_Gestdur_SBayesRC) + scale(PGS_Epilepsy_SBayesRC) + scale(PGS_Autism_SBayesRC) + scale(PGS_Agewalking_SBayesRC),
               family = binomial(link = 'logit'), 
               data = data)
  CP_all_pgs_roc <- roc(data$CP_all, model$fitted.values)
  
  list(
    univariate_roc = CP_pgs_roc,
    multivariate_roc = CP_all_pgs_roc
  )

}

roc_full <- roc_function(CP_df_not_related)
roc_monogenic <- roc_function(Monogenic_only)
roc_non_monogenic <- roc_function(Non_monogenic)
roc_young525 <- roc_function(Young525)

save(roc_full,
     roc_monogenic,
     roc_non_monogenic, 
     roc_young525,
     file = "Results/roc_models.RData")
## ----

