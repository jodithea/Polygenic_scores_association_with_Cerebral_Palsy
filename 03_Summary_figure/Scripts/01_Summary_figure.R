# Combine all data and make summary figure

## ---- LoadPackages
library(patchwork)    # for creating panelled figure
library(tidyverse)
## ----


## ---- LoadDataFrame
# Aus CP Biobank results
load(file = "../01_Australian_cohort/Results/regression_with_covar_df.RData")
load(file = "../01_Australian_cohort/Results/regression_no_covar_df.RData")
load(file = "../01_Australian_cohort/Results/roc_df.RData")
load(file = "../01_Australian_cohort/Results/r2l_df.RData")


# Geisinger results
MyCode_results <- read.table(file = "../02_MyCode/Results/MyCode_all_PGS_results_table.txt", header = T)
MyCode_results_matched <- read_csv(file = "../02_MyCode/Results/MyCode_all_PGS_results_matched_table.csv")
## ----

## ---- FullDataframe
# Aus CP Biobank: Combine all dataframes so have one df with all results

# Regression - including covariates
regression_df_full_with_covar <- regression_df_full_with_covar %>% 
  rename_with(
    ~ sub("^regr_", "regr_adjusted_", .x),
    starts_with("regr_")
  ) %>% 
  mutate(Dataset = "Australian (full)")

regression_df_monogenic_with_covar <- regression_df_monogenic_with_covar %>% 
  rename_with(
    ~ sub("^regr_", "regr_adjusted_", .x),
    starts_with("regr_")
  ) %>% 
  mutate(Dataset = "Australian (monogenic)")

regression_df_non_monogenic_with_covar <- regression_df_non_monogenic_with_covar %>% 
  rename_with(
    ~ sub("^regr_", "regr_adjusted_", .x),
    starts_with("regr_")
  ) %>% 
  mutate(Dataset = "Australian (non-monogenic)")

regression_df_young525_with_covar <- regression_df_young525_with_covar %>% 
  rename_with(
    ~ sub("^regr_", "regr_adjusted_", .x),
    starts_with("regr_")
  ) %>% 
  mutate(Dataset = "Australian (youngest 525 controls)")

# Regression - no covariates
regression_df_full_no_covar <- regression_df_full_no_covar %>% 
  rename_with(
    ~ sub("^regr_", "regr_unadjusted_", .x),
    starts_with("regr_")
  ) %>% 
  mutate(Dataset = "Australian (full)")

regression_df_monogenic_no_covar <- regression_df_monogenic_no_covar %>% 
  rename_with(
    ~ sub("^regr_", "regr_unadjusted_", .x),
    starts_with("regr_")
  ) %>% 
  mutate(Dataset = "Australian (monogenic)")

regression_df_non_monogenic_no_covar <- regression_df_non_monogenic_no_covar %>% 
  rename_with(
    ~ sub("^regr_", "regr_unadjusted_", .x),
    starts_with("regr_")
  ) %>% 
  mutate(Dataset = "Australian (non-monogenic)")

regression_df_young525_no_covar <- regression_df_young525_no_covar %>% 
  rename_with(
    ~ sub("^regr_", "regr_unadjusted_", .x),
    starts_with("regr_")
  ) %>% 
  mutate(Dataset = "Australian (youngest 525 controls)")

# Combine all regression results into 1 df
Aus_regression_with_covar_results <- regression_df_full_with_covar %>% 
  full_join(regression_df_monogenic_with_covar) %>% 
  full_join(regression_df_non_monogenic_with_covar) %>% 
  full_join(regression_df_young525_with_covar) 

Aus_regression_no_covar_results <- regression_df_full_no_covar %>% 
  full_join(regression_df_monogenic_no_covar) %>% 
  full_join(regression_df_non_monogenic_no_covar) %>% 
  full_join(regression_df_young525_no_covar)

Aus_regression_results <- Aus_regression_with_covar_results %>% 
  full_join(Aus_regression_no_covar_results)

# Combine all roc results into 1 df
Aus_roc_results <- (roc_df_full %>% mutate(Dataset = "Australian (full)")) %>% 
  full_join(roc_df_monogenic %>% mutate(Dataset = "Australian (monogenic)")) %>% 
  full_join(roc_df_non_monogenic %>% mutate(Dataset = "Australian (non-monogenic)")) %>%  
  full_join(roc_df_young525 %>% mutate(Dataset = "Australian (youngest 525 controls)"))

# Combine all r2l results into 1 df
Aus_r2l_results <- (r2l_df_full %>% mutate(Dataset = "Australian (full)")) %>% 
  full_join(r2l_df_monogenic %>% mutate(Dataset = "Australian (monogenic)")) %>% 
  full_join(r2l_df_non_monogenic %>% mutate(Dataset = "Australian (non-monogenic)")) %>% 
  full_join(r2l_df_young525 %>% mutate(Dataset = "Australian (youngest 525 controls)"))


Aus_all_results <- Aus_regression_results %>% 
  full_join(Aus_roc_results) %>% 
  full_join(Aus_r2l_results) %>% 
  mutate(PGS = factor(PGS, levels = c("all_PGS", "PGS_EA_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Stroke_SBayesRC", "PGS_Gestdur_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Agewalking_SBayesRC", "PGS_CP_FinnGen_SBayesRC")))

save(Aus_all_results, file = "Results/Aus_all_PGS_results_table.RData")

write.table(Aus_all_results, file = "Results/Aus_all_PGS_results_table.txt",
            row.names = F, col.names = T, sep = "\t", quote = F)

# MyCode: Combine all dataframes so have one df with all results
MyCode_results <- (MyCode_results %>% mutate(Dataset = "MyCode (unmatched)")) %>% 
  full_join(MyCode_results_matched %>% mutate(Dataset = "MyCode (matched)"))

# Combine Aus and MyCode results
Aus_MyCode_results <- Aus_all_results %>% 
  full_join(MyCode_results %>% mutate(across(c('term', 'PGS'),~ str_replace(., "PGS_CP_SBayesRC", "PGS_CP_FinnGen_SBayesRC")))) %>% 
  mutate(PGS = factor(PGS, levels = c("all_PGS", "PGS_EA_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Stroke_SBayesRC", "PGS_Gestdur_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Agewalking_SBayesRC", "PGS_CP_FinnGen_SBayesRC")),
         r2l_90perc_CI_lower = as.numeric(r2l_90perc_CI_lower),
         r2l_90perc_CI_upper = as.numeric(r2l_90perc_CI_upper))

save(Aus_MyCode_results, file = "Results/Aus_MyCode_PGS_results_table.RData")
write.table(Aus_MyCode_results, file = "Results/Aus_MyCode_PGS_results_table.txt",
            row.names = F, col.names = T, sep = "\t", quote = F)
## ----


## ---- SummaryFigureFull
# Create summary figure of all results for full Australian cohort and MyCode cohort using matched data (using adjusted regressions)

# Dodge doesn't work for significance stars when only one of the 2 cohorts are significant
# Create star data with numeric y positions for manual dodging
star_data <- Aus_MyCode_results %>% 
  filter((Dataset == "Australian (full)" | Dataset == "MyCode (matched)")) %>%
  mutate(PGS_factor = factor(PGS, levels = c("PGS_EA_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Stroke_SBayesRC", "PGS_Gestdur_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Agewalking_SBayesRC", "PGS_CP_FinnGen_SBayesRC")),
         PGS_num = as.numeric(PGS_factor),
         y_star = case_when(
           Dataset == "Australian (full)" ~ PGS_num + 0.15,       # nudge up
           Dataset == "MyCode (matched)" ~ PGS_num - 0.15   # nudge down
         ))

# Univariate regression
regr_plot_univ <- ggplot(star_data %>% filter(model == "univariate") %>% droplevels(), aes(x = regr_adjusted_OR, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 1, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = regr_adjusted_95perc_CI_lower, xmax = regr_adjusted_95perc_CI_upper)) +
  scale_x_continuous("Odds Ratio",
                     trans = "log2",
                     limits = c(0.6, 1.5),
                     breaks = c(0.67, 0.8, 1, 1.25, 1.5),
                     labels = c("0.67", "0.8", "1", "1.25", "1.5")) +
  scale_y_continuous("Polygenic Score",
                     breaks = sort(unique(star_data$PGS_num)),
                   labels = c("Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Cohort",
                      labels = c("Australia", "MyCode"),
                      values = c("#1F968BFF", "#440154FF")) +
  scale_shape("Cohort",
              labels = c("Australia", "MyCode")) +
  geom_text(data = star_data %>% 
              filter(model == "univariate", regr_adjusted_padj < 0.05),
            aes(x = regr_adjusted_95perc_CI_upper + 0.05, 
                y = y_star, 
                colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  # guides(color = guide_legend(title.position = "top",
  #                             title.hjust = 0.5,
  #                             nrow = 1,
  #                             byrow = TRUE,
  #                             direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none")

# regr_plot_univ


# Multivariate regression
regr_plot_multiv <- ggplot(star_data %>% filter(model == "multivariate" & PGS != "all_PGS"), aes(x = regr_adjusted_OR, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 1, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = regr_adjusted_95perc_CI_lower, xmax = regr_adjusted_95perc_CI_upper)) +
  scale_x_continuous("Odds Ratio",
                     trans = "log2",
                     limits = c(0.6, 1.5),
                     breaks = c(0.67, 0.8, 1, 1.25, 1.5),
                     labels = c("0.67", "0.8", "1", "1.25", "1.5")) +
  scale_y_continuous("",
                     breaks = sort(unique(star_data$PGS_num)),
                   labels = c("Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Cohort",
                      labels = c("Australia", "MyCode"),
                      values = c("#1F968BFF", "#440154FF")) +
  scale_shape("Cohort",
              labels = c("Australia", "MyCode")) +
  geom_text(data = star_data %>% 
              filter(model == "multivariate" & PGS != "all_PGS", regr_adjusted_padj < 0.05),
            aes(x = regr_adjusted_95perc_CI_upper + 0.05, y = y_star, colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  # guides(color = guide_legend(title.position = "top",
  #                             title.hjust = 0.5,
  #                             nrow = 1,
  #                             byrow = TRUE,
  #                             direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none")

# regr_plot_multiv

star_data2 <- Aus_MyCode_results %>% 
  filter((Dataset == "Australian (full)" | Dataset == "MyCode (matched)")) %>%
  mutate(PGS_factor = factor(PGS, levels = c("all_PGS", "PGS_EA_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Stroke_SBayesRC", "PGS_Gestdur_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Agewalking_SBayesRC", "PGS_CP_FinnGen_SBayesRC")),
         PGS_num = as.numeric(PGS_factor),
         y_star = case_when(
           Dataset == "Australian (full)" ~ PGS_num + 0.15,       # nudge up
           Dataset == "MyCode (matched)" ~ PGS_num - 0.15   # nudge down
         ))

# AUC
auc_plot <- ggplot(star_data2, aes(x = AUC, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 0.5, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = AUC_90perc_CI_lower, xmax = AUC_90perc_CI_upper)) +
  scale_x_continuous("AUC",
                     limits = c(0.45, 0.66),
                     breaks = c(0.45, 0.5, 0.55, 0.6, 0.65)) +
  scale_y_continuous("Polygenic Score",
                     breaks = sort(unique(star_data2$PGS_num)),
                   labels = c("All", "Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Cohort",
                      labels = c("Australia", "MyCode"),
                      values = c("#1F968BFF", "#440154FF")) +
  scale_shape("Cohort",
              labels = c("Australia", "MyCode")) +
  geom_text(data = star_data2 %>% 
              filter(AUC_padj < 0.05),
            aes(x = AUC_90perc_CI_upper + 0.01, y = y_star, colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  # guides(color = guide_legend(title.position = "top",
  #                             title.hjust = 0.5,
  #                             nrow = 1,
  #                             byrow = TRUE,
  #                             direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none")

# auc_plot


# R2 on the liability scale
r2l_plot <- ggplot(star_data2, aes(x = r2l * 100, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 0, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = r2l_90perc_CI_lower * 100, xmax = r2l_90perc_CI_upper * 100)) +
  scale_x_continuous(expression("Liability"~R^2~"(%)"),
                     limits = c(0, 2.1)) +
  scale_y_continuous("",
                     breaks = sort(unique(star_data2$PGS_num)),
                   labels = c("All", "Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Cohort",
                      labels = c("Australia", "MyCode"),
                      values = c("#1F968BFF", "#440154FF")) +
  scale_shape("Cohort",
              labels = c("Australia", "MyCode")) +
  geom_text(data = star_data2 %>% 
              filter(r2l_padj < 0.05),
            aes(x = r2l_90perc_CI_upper * 100 + 0.1, y = y_star, colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  # guides(color = guide_legend(title.position = "top",
  #                             title.hjust = 0.5,
  #                             nrow = 1,
  #                             byrow = TRUE,
  #                             direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none")

# r2l_plot

# Figure with all plots
figure <-(guide_area() /
            (regr_plot_univ | regr_plot_multiv) / 
            (auc_plot | r2l_plot)) +
  plot_layout(heights = c(0.2, 1, 1),
              guides = "collect") +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = "bold"),
        legend.position = "top")

figure
## ----


## ---- SaveFigureFull
ggsave(figure, width = 18, height = 19, dpi = 300, unit = "cm", 
       file = "Figures/Figure_Aus_fulldataset_Geisinger_matched_regr_adjusted_auc_r2l.png")
ggsave(figure, width = 18, height = 19, dpi = 300, unit = "cm", 
       file = "Figures/Figure_Aus_fulldataset_Geisinger_matched_regr_adjusted_auc_r2l.pdf", 
       device = cairo_pdf)
## ----


## ---- SummaryFigureAusStrat
# Create summary figure of Australian cohort: full and stratified by monogenic status. Use adjusted regressions.

# Dodge doesn't work for significance stars when only one of the 2 cohorts are significant
# Create star data with numeric y positions for manual dodging
star_data <- Aus_MyCode_results %>% 
  filter(Dataset == "Australian (full)" | Dataset == "Australian (monogenic)" | Dataset == "Australian (non-monogenic)") %>%
  mutate(PGS_factor = factor(PGS, levels = c("PGS_EA_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Stroke_SBayesRC", "PGS_Gestdur_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Agewalking_SBayesRC", "PGS_CP_FinnGen_SBayesRC")),
         PGS_num = as.numeric(PGS_factor),
         y_star = case_when(
           Dataset == "Australian (full)" ~ PGS_num + 0.2,       
           Dataset == "Australian (monogenic)" ~ PGS_num,
           Dataset == "Australian (non-monogenic)" ~ PGS_num - 0.2
         ))

# Univariate regression
regr_plot_univ <- ggplot(star_data %>% filter(model == "univariate") %>% droplevels(), aes(x = regr_adjusted_OR, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 1, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = regr_adjusted_95perc_CI_lower, xmax = regr_adjusted_95perc_CI_upper)) +
  scale_x_continuous("Odds Ratio",
                     trans = "log2",
                     limits = c(0.6, 1.5),
                     breaks = c(0.67, 0.8, 1, 1.25, 1.5),
                     labels = c("0.67", "0.8", "1", "1.25", "1.5")) +
  scale_y_continuous("Polygenic Score",
                     breaks = sort(unique(star_data$PGS_num)),
                   labels = c("Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Australian cohort",
                      labels = c("Full", "Monogenic", "Non-monogenic"),
                      values = c("#1F968BFF", "#73D055FF", "#DCE319FF")) +
  scale_shape_manual("Australian cohort",
              values = c(16, 17, 15),
              labels = c("Full", "Monogenic", "Non-monogenic")) +
  geom_text(data = star_data %>% 
              filter(model == "univariate", regr_adjusted_padj < 0.05),
            aes(x = regr_adjusted_95perc_CI_upper + 0.05, 
                y = y_star, 
                colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 1,
                              byrow = TRUE,
                              direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none")

# regr_plot_univ

# Multivariate regression
regr_plot_multiv <- ggplot(star_data %>% filter(model == "multivariate" & PGS != "all_PGS"), aes(x = regr_adjusted_OR, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 1, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = regr_adjusted_95perc_CI_lower, xmax = regr_adjusted_95perc_CI_upper)) +
  scale_x_continuous("Odds Ratio",
                     trans = "log2",
                     limits = c(0.6, 1.5),
                     breaks = c(0.67, 0.8, 1, 1.25, 1.5),
                     labels = c("0.67", "0.8", "1", "1.25", "1.5")) +
  scale_y_continuous("",
                     breaks = sort(unique(star_data$PGS_num)),
                   labels = c("Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Australian cohort",
                      labels = c("Full", "Monogenic", "Non-monogenic"),
                      values = c("#1F968BFF", "#73D055FF", "#DCE319FF")) +
  scale_shape_manual("Australian cohort",
              values = c(16, 17, 15),
              labels = c("Full", "Monogenic", "Non-monogenic")) +
  geom_text(data = star_data %>% 
              filter(model == "multivariate", regr_adjusted_padj < 0.05),
            aes(x = regr_adjusted_95perc_CI_upper + 0.05, y = y_star, colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 1,
                              byrow = TRUE,
                              direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none")

# regr_plot_multiv

star_data2 <- Aus_MyCode_results %>% 
  filter(Dataset == "Australian (full)" | Dataset == "Australian (monogenic)" | Dataset == "Australian (non-monogenic)") %>%
  mutate(PGS_factor = factor(PGS, levels = c("all_PGS", "PGS_EA_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Stroke_SBayesRC", "PGS_Gestdur_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Agewalking_SBayesRC", "PGS_CP_FinnGen_SBayesRC")),
         Dataset = factor(Dataset,
                          levels = c("Australian (full)", "Australian (monogenic)", "Australian (non-monogenic)")),
         PGS_num = as.numeric(PGS_factor),
         y_star = case_when(
           Dataset == "Australian (full)" ~ PGS_num + 0.2,       
           Dataset == "Australian (monogenic)" ~ PGS_num,
           Dataset == "Australian (non-monogenic)" ~ PGS_num - 0.2
         ))


# AUC
auc_plot <- ggplot(star_data2, aes(x = AUC, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 0.5, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = AUC_90perc_CI_lower, xmax = AUC_90perc_CI_upper)) +
  scale_x_continuous("AUC",
                     limits = c(0.46, 0.675),
                     breaks = c(0.5, 0.55, 0.6, 0.65)) +
  scale_y_continuous("Polygenic Score",
                     breaks = sort(unique(star_data2$PGS_num)),
                   labels = c("All", "Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Australian cohort",
                      breaks = c("Australian (full)", "Australian (monogenic)", "Australian (non-monogenic)"),
                      labels = c("Full", "Monogenic", "Non-monogenic"),
                      values = c("#1F968BFF", "#73D055FF", "#DCE319FF")) +
  scale_shape_manual("Australian cohort",
                     c("Australian (full)", "Australian (monogenic)", "Australian (non-monogenic)"),
                     values = c(16, 17, 15),
              labels = c("Full", "Monogenic", "Non-monogenic")) +
  geom_text(data = star_data2 %>% 
              filter(AUC_padj < 0.05),
            aes(x = AUC_90perc_CI_upper + 0.01, y = y_star, colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 1,
                              byrow = TRUE,
                              direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none")

# auc_plot


# R2 on the liability scale
r2l_plot <- ggplot(star_data2, aes(x = r2l * 100, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 0, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = r2l_90perc_CI_lower * 100, xmax = r2l_90perc_CI_upper * 100)) +
  scale_x_continuous(expression("Liability"~R^2~"(%)"),
                     limits = c(0, 4.09)) +
  scale_y_continuous("",
                     breaks = sort(unique(star_data2$PGS_num)),
                   labels = c("All", "Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Australian cohort",
                      breaks = c("Australian (full)", "Australian (monogenic)", "Australian (non-monogenic)"),
                      labels = c("Full", "Monogenic", "Non-monogenic"),
                      values = c("#1F968BFF", "#73D055FF", "#DCE319FF")) +
  scale_shape_manual("Australian cohort",
                     breaks = c("Australian (full)", "Australian (monogenic)", "Australian (non-monogenic)"),
                     values = c(16, 17, 15),
              labels = c("Full", "Monogenic", "Non-monogenic")) +
  geom_text(data = star_data2 %>% 
              filter(r2l_padj < 0.05),
            aes(x = r2l_90perc_CI_upper * 100 + 0.1, y = y_star, colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 1,
                              byrow = TRUE,
                              direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "top")

# r2l_plot


# Figure with all plots
figure_aus_strat <- (guide_area() /
  (regr_plot_univ | regr_plot_multiv) / 
  (auc_plot | r2l_plot)) +
  plot_layout(heights = c(0.2, 1, 1),
              guides = "collect") +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = "bold"),
        legend.position = "top")

figure_aus_strat 

## ----


## ---- SaveFigureAusStrat
ggsave(figure_aus_strat, width = 18, height = 21, dpi = 300, unit = "cm", 
       file = "Figures/Figure_Aus_fulldataset_monogenic_nonmonogenic_regr_adjusted_auc_r2l.png")

ggsave(figure_aus_strat, width = 18, height = 21, dpi = 300, unit = "cm", 
       file = "Figures/Figure_Aus_fulldataset_monogenic_nonmonogenic_regr_adjusted_auc_r2l.pdf",
       device = cairo_pdf)
## ----





## ---- SummaryFigureFullAndCheckAge
# Create summary figure of all results for full Australian cohort and MyCode (using adjusted regressions) as well as analyses to check influence of age - Australian cohort with only youngest 525 retained, and MyCode matched on sex and age

# Dodge doesn't work for significance stars when only one of the 2 cohorts are significant
# Create star data with numeric y positions for manual dodging
star_data <- Aus_MyCode_results %>% 
  filter(Dataset == "Australian (full)" | Dataset == "Australian (youngest 525 controls)" | Dataset == "MyCode (unmatched)" | Dataset == "MyCode (matched)") %>%
  mutate(PGS_factor = factor(PGS, levels = c("PGS_EA_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Stroke_SBayesRC", "PGS_Gestdur_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Agewalking_SBayesRC", "PGS_CP_FinnGen_SBayesRC")),
         Dataset = factor(Dataset, 
                          levels = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)")),
         PGS_num = as.numeric(PGS_factor),
         y_star = case_when(
           Dataset == "Australian (full)" ~ PGS_num + 0.3,      
           Dataset == "Australian (youngest 525 controls)" ~ PGS_num + 0.1, 
           Dataset == "MyCode (unmatched)" ~ PGS_num - 0.1,
           Dataset == "MyCode (matched)" ~ PGS_num - 0.3 
         ))

# Univariate regression
regr_plot_univ <- ggplot((star_data %>% filter(model == "univariate") %>% droplevels()), aes(x = regr_adjusted_OR, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 1, colour = "grey80", linetype = "dashed") +
  geom_point(position = position_dodge(width = -0.8)) +
  geom_linerange(aes(xmin = regr_adjusted_95perc_CI_lower, xmax = regr_adjusted_95perc_CI_upper),
                 position = position_dodge(width = -0.8)) +
  scale_x_continuous("Odds Ratio",
                     trans = "log2",
                     limits = c(0.6, 1.5),
                     breaks = c(0.67, 0.8, 1, 1.25, 1.5),
                     labels = c("0.67", "0.8", "1", "1.25", "1.5")) +
  scale_y_continuous("Polygenic Score",
                     breaks = sort(unique(star_data$PGS_num)),
                     labels = c("Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Cohort",
                      breaks = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)"),
                      labels = c("Australia (full)", "Australia (youngest controls)", "MyCode (unmatched)", "MyCode (matched)"),
                      values = c("#1F968BFF", "#73D055FF", "#440154FF", "#404788FF")) +
  scale_shape("Cohort",
              breaks = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)"),
              labels = c("Australia (full)", "Australia (youngest controls)", "MyCode (unmatched)", "MyCode (matched)")) +
  geom_text(data = star_data %>% 
              filter(model == "univariate", regr_adjusted_padj < 0.05),
            aes(x = regr_adjusted_95perc_CI_upper + 0.05, 
                y = y_star, 
                colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 2,
                              byrow = TRUE,
                              direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none",
        legend.spacing.y = unit(-0.3, "cm"),
        legend.key.height = unit(0.3, "lines"))

# regr_plot_univ

# Multivariate regression
regr_plot_multiv <- ggplot((star_data %>% filter(model == "multivariate" & PGS != "all_PGS") %>% droplevels()), 
                           aes(x = regr_adjusted_OR, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 1, colour = "grey80", linetype = "dashed") +
  geom_point(position = position_dodge(width = -0.8)) +
  geom_linerange(aes(xmin = regr_adjusted_95perc_CI_lower, xmax = regr_adjusted_95perc_CI_upper),
                 position = position_dodge(width = -0.8)) +
  scale_x_continuous("Odds Ratio",
                     trans = "log2",
                     breaks = c(0.67, 0.8, 1, 1.25, 1.5),
                     labels = c("0.67", "0.8", "1", "1.25", "1.5")) +
  scale_y_continuous("",
                     breaks = sort(unique(star_data$PGS_num)),
                   labels = c("Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Cohort",
                      breaks = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)"),
                      labels = c("Australia (full)", "Australia (youngest controls)", "MyCode (unmatched)", "MyCode (matched)"),
                      values = c("#1F968BFF", "#73D055FF", "#440154FF", "#404788FF")) +
  scale_shape("Cohort",
              breaks = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)"),
              labels = c("Australia (full)", "Australia (youngest controls)", "MyCode (unmatched)", "MyCode (matched)")) +
  geom_text(data = star_data %>% 
              filter(model == "multivariate", regr_adjusted_padj < 0.05),
            aes(x = regr_adjusted_95perc_CI_upper + 0.05, 
                y = y_star, 
                colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 2,
                              byrow = TRUE,
                              direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none",
        legend.spacing.y = unit(-0.3, "cm"),
        legend.key.height = unit(0.3, "lines"))

# regr_plot_multiv

star_data2 <- Aus_MyCode_results %>% 
  filter(Dataset == "Australian (full)" | Dataset == "Australian (youngest 525 controls)" | Dataset == "MyCode (unmatched)" | Dataset == "MyCode (matched)") %>%
  mutate(PGS_factor = factor(PGS, 
                             levels = c("all_PGS", "PGS_EA_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Stroke_SBayesRC", "PGS_Gestdur_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Agewalking_SBayesRC", "PGS_CP_FinnGen_SBayesRC")),
         Dataset = factor(Dataset, 
                          levels = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)")),
         PGS_num = as.numeric(PGS_factor),
         y_star = case_when(
           Dataset == "Australian (full)" ~ PGS_num + 0.3,      
           Dataset == "Australian (youngest 525 controls)" ~ PGS_num + 0.1, 
           Dataset == "MyCode (unmatched)" ~ PGS_num - 0.1,
           Dataset == "MyCode (matched)" ~ PGS_num - 0.3 
         ))

# AUC
auc_plot <- ggplot((star_data2 %>% filter(model == "univariate" | (model == "multivariate" & PGS == "all_PGS"))),
                   aes(x = AUC, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 0.5, colour = "grey80", linetype = "dashed") +
  geom_point() +
  geom_linerange(aes(xmin = AUC_90perc_CI_lower, xmax = AUC_90perc_CI_upper)) +
  scale_x_continuous("AUC",
                     limits = c(0.45, 0.66),
                     breaks = c(0.5, 0.55, 0.6, 0.65)) +
  scale_y_continuous("Polygenic Score",
                     breaks = sort(unique(star_data2$PGS_num)),
                     labels = c("All", "Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Cohort",
                      breaks = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)"),
                      labels = c("Australia (full)", "Australia (youngest controls)", "MyCode (unmatched)", "MyCode (matched)"),
                      values = c("#1F968BFF", "#73D055FF", "#440154FF", "#404788FF")) +
  scale_shape("Cohort",
              breaks = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)"),
              labels = c("Australia (full)", "Australia (youngest controls)", "MyCode (unmatched)", "MyCode (matched)")) +
  geom_text(data = star_data2 %>% filter(model == "univariate" | (model == "multivariate" & PGS == "all_PGS")) %>% 
              filter(AUC_padj < 0.05),
            aes(x = AUC_90perc_CI_upper + 0.01, y = y_star, colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 2,
                              byrow = TRUE,
                              direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none",
        legend.spacing.y = unit(-0.3, "cm"),
        legend.key.height = unit(0.3, "lines"))

# auc_plot


# R2 on he liability scale
r2l_plot <- ggplot((star_data2 %>% filter(model == "univariate" | (model == "multivariate" & PGS == "all_PGS"))),
                   aes(x = r2l * 100, y = y_star, colour = Dataset, shape = Dataset)) +
  geom_vline(xintercept = 0, colour = "grey80", linetype = "dashed") +
  geom_point(position = position_dodge(width = -0.8)) +
  geom_linerange(position = position_dodge(width = -0.8),
                 aes(xmin = r2l_90perc_CI_lower * 100, xmax = r2l_90perc_CI_upper * 100)) +
  scale_x_continuous(expression("Liability"~R^2~"(%)"),
                     limits = c(0, 2.6),
                     breaks = seq(0, 2.5, 0.5)) +
  scale_y_continuous("",
                     breaks = sort(unique(star_data2$PGS_num)),
                   labels = c("All", "Educational \nAttainment", "Epilepsy", "Autism", "Stroke", "Gestational \nduration", "Birth weight", "Walking age", "Cerebral Palsy")) +
  scale_colour_manual("Cohort",
                      breaks = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)"),
                      labels = c("Australia (full)", "Australia (youngest controls)", "MyCode (unmatched)", "MyCode (matched)"),
                      values = c("#1F968BFF", "#73D055FF", "#440154FF", "#404788FF")) +
  scale_shape("Cohort",
              breaks = c("Australian (full)", "Australian (youngest 525 controls)", "MyCode (unmatched)", "MyCode (matched)"),
              labels = c("Australia (full)", "Australia (youngest controls)", "MyCode (unmatched)", "MyCode (matched)")) +
  geom_text(data = star_data2 %>% 
              filter(r2l_padj < 0.05),
            aes(x = r2l_90perc_CI_upper * 100 + 0.1, y = y_star, colour = Dataset),
            label = "*",
            size = 4,
            inherit.aes = FALSE,
            show.legend = FALSE) +
  theme_classic() +
  guides(color = guide_legend(title.position = "top",
                              title.hjust = 0.5,
                              nrow = 2,
                              byrow = TRUE,
                              direction = "horizontal")) +
  theme(text = element_text(family = "sans"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 10, colour = "black", margin = margin(10,0,0,0)),
        axis.title.y = element_text(size = 10, colour = "black", margin = margin(0,10,0,0)),
        legend.text = element_text(size = 8, colour = "black"),
        legend.title = element_text(size = 10, colour = "black"),
        legend.position = "none",
        legend.spacing.y = unit(-0.3, "cm"),
        legend.key.height = unit(0.3, "lines"))

# r2l_plot

# Figure with all plots
figure_check_age <-(guide_area() /
            (regr_plot_univ | regr_plot_multiv) / 
            (auc_plot | r2l_plot)) +
  plot_layout(heights = c(0.2, 1, 1),
              guides = "collect") +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(face = "bold"),
        legend.position = "top")

figure_check_age
## ----


## ---- SaveFigureFullAndCheckAge
ggsave(figure_check_age, width = 18, height = 27, dpi = 300, unit = "cm", 
       file = "Figures/Figure_Aus_fulldataset_youngest525controls_Geisinger_full_matched_regr_adjusted_auc_r2l.png")

ggsave(figure_check_age, width = 18, height = 27, dpi = 300, unit = "cm", 
       file = "Figures/Figure_Aus_fulldataset_youngest525controls_Geisinger_full_matched_regr_adjusted_auc_r2l.pdf",
       device = cairo_pdf)
## ----