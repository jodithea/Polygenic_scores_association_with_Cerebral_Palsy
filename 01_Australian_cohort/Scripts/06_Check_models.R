# Check regression models

## ---- LoadDataFrame
load(file = "Data/CP_df_not_related.Rdata")
load(file = "Data/Monogenic_only_df.RData")
load(file = "Data/Non_monogenic_df.RData")
load(file = "Data/Young525_df.RData")

load(file = "Results/Regression_models.RData")

load(file = "Data/PGS_variables.RData")
load(file = "Data/PGS_names.RData")
## ----


## Full Dataset - logistic regression with covariates
## ---- FullModelCheckPGSModelsWithCovarCheckModel
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_full_with_covar$univariate_regression[[pgs]]

  print(performance::check_model(model))

}
## ----

## ---- FullModelCheckPGSModelsWithCovarDHARMa
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_full_with_covar$univariate_regression[[pgs]]

  sim_res <- simulateResiduals(model, plot = TRUE)
}
## ----

## ---- FullModelWithCovarCheckAllPGSModel
performance::check_model(regression_full_with_covar$multivariate_regression)

simulateResiduals(regression_full_with_covar$multivariate_regression, plot=TRUE)
## ----


## Full Dataset - logistic regression no covariates
## ---- FullModelCheckPGSModelsNoCovarCheckModel
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_full_no_covar$univariate_regression[[pgs]]
  
  print(performance::check_model(model))
  
}
## ----

## ---- FullModelCheckPGSModelsNoCovarDHARMa
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_full_no_covar$univariate_regression[[pgs]]
  
  sim_res <- simulateResiduals(model, plot = TRUE)
}
## ----

## ---- FullModelNoCovarCheckAllPGSModel
performance::check_model(regression_full_no_covar$multivariate_regression)

simulateResiduals(regression_full_no_covar$multivariate_regression, plot=TRUE)
## ----




## Monogenic Dataset - with covariates

## ---- MonoModelWithCovarCheckPGSModelsCheckModel
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_monogenic_with_covar$univariate_regression[[pgs]]
  
  print(performance::check_model(model))
  
}
## ----

## ---- MonoModelWithCovarCheckPGSModelsDHARMa
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_monogenic_with_covar$univariate_regression[[pgs]]
  
  sim_res <- simulateResiduals(model, plot = TRUE)
}
## ----

## ---- MonoModelWithCovarCheckAllPGSModel
performance::check_model(regression_monogenic_with_covar$multivariate_regression)

simulateResiduals(regression_monogenic_with_covar$multivariate_regression, plot=TRUE)
## ----


## Monogenic Dataset - no covariates

## ---- MonoModelNoCovarCheckPGSModelsCheckModel
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_monogenic_no_covar$univariate_regression[[pgs]]
  
  print(performance::check_model(model))
  
}
## ----

## ---- MonoModelNoCovarCheckPGSModelsDHARMa
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_monogenic_no_covar$univariate_regression[[pgs]]
  
  sim_res <- simulateResiduals(model, plot = TRUE)
}
## ----

## ---- MonoModelNoCovarCheckAllPGSModel
performance::check_model(regression_monogenic_no_covar$multivariate_regression)

simulateResiduals(regression_monogenic_no_covar$multivariate_regression, plot=TRUE)
## ----




## Non-monogenic Dataset - with covariates

## ---- NonmonoModelWithCovarCheckPGSModelsCheckModel
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_non_monogenic_with_covar$univariate_regression[[pgs]]
  
  print(performance::check_model(model))
  
}
## ----

## ---- NonmonoModelWithCovarheckPGSModelsDHARMa
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_non_monogenic_with_covar$univariate_regression[[pgs]]
  
  sim_res <- simulateResiduals(model, plot = TRUE)
}
## ----

## ---- NonmonoModelWithCovarCheckAllPGSModel
performance::check_model(regression_non_monogenic_with_covar$multivariate_regression)

simulateResiduals(regression_non_monogenic_with_covar$multivariate_regression, plot=TRUE)
## ----



## Non-monogenic Dataset - no covariates

## ---- NonmonoModelNoCovarCheckPGSModelsCheckModel
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_non_monogenic_no_covar$univariate_regression[[pgs]]
  
  print(performance::check_model(model))
  
}
## ----

## ---- NonmonoModelNoCovarheckPGSModelsDHARMa
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_non_monogenic_no_covar$univariate_regression[[pgs]]
  
  sim_res <- simulateResiduals(model, plot = TRUE)
}
## ----

## ---- NonmonoModelNoCovarCheckAllPGSModel
performance::check_model(regression_non_monogenic_no_covar$multivariate_regression)

simulateResiduals(regression_non_monogenic_no_covar$multivariate_regression, plot=TRUE)
## ----






## Youngest 525 controls Dataset - with covariates

## ---- Young525ModelWithCovarCheckPGSModelsCheckModel
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_young525_with_covar$univariate_regression[[pgs]]
  
  print(performance::check_model(model))
  
}
## ----

## ---- Young525ModelWithCovarheckPGSModelsDHARMa
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_young525_with_covar$univariate_regression[[pgs]]
  
  sim_res <- simulateResiduals(model, plot = TRUE)
}
## ----

## ---- Young525ModelWithCovarCheckAllPGSModel
performance::check_model(regression_young525_with_covar$multivariate_regression)

simulateResiduals(regression_young525_with_covar$multivariate_regression, plot=TRUE)
## ----



## Youngest 525 Controls Dataset - no covariates

## ---- Young525ModelNoCovarCheckPGSModelsCheckModel
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_young525_no_covar$univariate_regression[[pgs]]
  
  print(performance::check_model(model))
  
}
## ----

## ---- Young525ModelNoCovarheckPGSModelsDHARMa
for (pgs in pgs_vars) {
  cat("\n====================================\n")
  cat("Diagnostics for:", pgs, "\n")
  cat("====================================\n\n")
  
  model <- regression_young525_no_covar$univariate_regression[[pgs]]
  
  sim_res <- simulateResiduals(model, plot = TRUE)
}
## ----

## ---- Young525ModelNoCovarCheckAllPGSModel
performance::check_model(regression_young525_no_covar$multivariate_regression)

simulateResiduals(regression_young525_no_covar$multivariate_regression, plot=TRUE)
## ----
