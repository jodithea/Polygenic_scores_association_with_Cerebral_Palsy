# Results from AUC of ROC

## ---- LoadDataFrame
load(file = "Results/roc_models.RData")

load(file = "Data/PGS_variables.RData")
load(file = "Data/PGS_names.RData")
## ----


## ---- AUCResults
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
roc_df_monogenic <- roc_df_function(roc_monogenic)
roc_df_non_monogenic <- roc_df_function(roc_non_monogenic)
roc_df_young525 <- roc_df_function(roc_young525)

save(roc_df_full,
     roc_df_monogenic,
     roc_df_non_monogenic, 
     roc_df_young525,
     file = "Results/roc_df.RData")
## ----


## Full dataset
## ----FullResultsAUCPrint
roc_df_full %>% 
  kable(caption = "AUC results (full dataset)")
## ----

## ---- FullAUCPlot
# One PGS in each model
function_plot_roc <- function(pgs_var, pgs_name) {
  
  model <- roc_full$univariate_roc[[pgs_var]]
  
  plot(model,
       print.auc = TRUE,
       main = paste("ROC curve for PGS of ", pgs_name, sep = ""))
}

plot_roc <- map2(pgs_vars, pgs_name, function_plot_roc)

walk(plot_roc, print)

# All PGS in same model
plot(roc_full$multivariate_roc,
     print.auc = TRUE,
     main = "ROC curve for all PGS in same model")
## ----



## Monogenic only
## ----MonoResultsAUCPrint
roc_df_monogenic %>% 
  kable(caption = "AUC results (individuals with monogenic diagnosis only)")
## ----

## ---- MonoAUCPlot
# One PGS in each model
function_plot_roc <- function(pgs_var, pgs_name) {
  
  model <- roc_monogenic$univariate_roc[[pgs_var]]
  
  plot(model,
       print.auc = TRUE,
       main = paste("ROC curve for PGS of ", pgs_name, sep = ""))
}

plot_roc <- map2(pgs_vars, pgs_name, function_plot_roc)

walk(plot_roc, print)

# All PGS in same model
plot(roc_monogenic$multivariate_roc,
     print.auc = TRUE,
     main = "ROC curve for all PGS in same model")
## ----



## Non-monogenic only
## ----NonmonoResultsAUCPrint
roc_df_non_monogenic %>% 
  kable(caption = "AUC results (individuals without a monogenic diagnosis)")
## ----

## ---- NonmonoAUCPlot
# One PGS in each model
function_plot_roc <- function(pgs_var, pgs_name) {
  
  model <- roc_non_monogenic$univariate_roc[[pgs_var]]
  
  plot(model,
       print.auc = TRUE,
       main = paste("ROC curve for PGS of ", pgs_name, sep = ""))
}

plot_roc <- map2(pgs_vars, pgs_name, function_plot_roc)

walk(plot_roc, print)

# All PGS in same model
plot(roc_non_monogenic$multivariate_roc,
     print.auc = TRUE,
     main = "ROC curve for all PGS in same model")
## ----





## Controls restricted to youngest 525 dataset
## ----Young525ResultsAUCPrint
roc_df_young525 %>% 
  kable(caption = "AUC results (Controls restricted to youngest 525 dataset)")
## ----

## ---- Young525AUCPlot
# One PGS in each model
function_plot_roc <- function(pgs_var, pgs_name) {
  
  model <- roc_young525$univariate_roc[[pgs_var]]
  
  plot(model,
       print.auc = TRUE,
       main = paste("ROC curve for PGS of ", pgs_name, sep = ""))
}

plot_roc <- map2(pgs_vars, pgs_name, function_plot_roc)

walk(plot_roc, print)

# All PGS in same model
plot(roc_young525$multivariate_roc,
     print.auc = TRUE,
     main = "ROC curve for all PGS in same model")
## ----