# Data exploration before fitting regression models

## ---- LoadDataFrame
load(file = "Data/CP_df_not_related.Rdata")
load(file = "Data/Monogenic_only_df.RData")
load(file = "Data/Non_monogenic_df.RData")
load(file = "Data/Young525_df.RData")
## ----

## ---- SetUpPGSVars
pgs_vars <- c("PGS_CP_FinnGen_SBayesRC", "PGS_EA_SBayesRC", "PGS_Birthweight_SBayesRC", "PGS_Stroke_SBayesRC",
              "PGS_Gestdur_SBayesRC", "PGS_Epilepsy_SBayesRC", "PGS_Autism_SBayesRC", "PGS_Agewalking_SBayesRC")

pgs_name <- c("Cerebral Palsy", "Educational Attainment", "Birth weight", "Stroke", "Gestational duration", "Epilepsy", "Autism", "Walking age")

save(pgs_vars, file = "Data/PGS_variables.RData")
save(pgs_name, file = "Data/PGS_names.RData")
## ----


## ---- Exploration 
exploration_function <- function(data) {
  
  # Histograms
  function_pgs_hist <- function(pgs_var) {
    # Raw plot
    a <- ggplot(data, aes(x = .data[[pgs_var]])) +
      geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
      labs(x = paste("Raw:", pgs_var), y = "Count") +
      theme_classic()
    
    # Standardized plot
    b <- ggplot(data, aes(x = scale(.data[[pgs_var]]))) +
      geom_histogram(bins = 30, fill = "#404080", color = "white") +
      labs(x = paste("Standardized:", pgs_var), y = "Count") +
      theme_classic()
    
    a + b
  }
  
  
  pgs_hist <- map(pgs_vars, function_pgs_hist)
  
  # Density + Boxplots
  function_pgs_distr_by_cp <- function(pgs_var, pgs_name) {
    
    means <- CP_df_not_related %>%
      group_by(CP_all) %>%
      summarise(mean_value = mean(scale(.data[[pgs_var]]), na.rm = TRUE))
    
    labels <- c("1" = "Case", "0" = "Control")
    
    # Density plot
    density_plot <- ggplot(data, aes(x = scale(.data[[pgs_var]]), colour = as.factor(CP_all), fill = as.factor(CP_all))) +
      geom_density(alpha = 0.6, position = "identity", adjust = 1.5) +
      geom_vline(data = means, aes(xintercept = mean_value, colour = as.factor(CP_all)), 
                 linetype = "dashed", linewidth = 1, alpha = 1,
                 show.legend = F) +
      scale_colour_viridis_d(name = "Cerebral Palsy", labels = labels) +
      scale_fill_viridis_d(name = "Cerebral Palsy", labels = labels) +
      scale_x_continuous(paste("Polygenic Score for", pgs_name, "(standardised)")) +
      scale_y_continuous("") +
      theme_classic() +
      theme(text = element_text(family = "Calibri"),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_text(size = 12, colour = "black", margin = margin(10,0,0,0)),
            axis.title.y = element_text(size = 12, colour = "black", margin = margin(0,10,0,0)),
            legend.title = element_blank(),
            legend.text = element_text(size = 10, colour = "black"),
            legend.position = "right")
    
    # Boxplot
    box_plot <- ggplot(CP_df_not_related, aes(x = as.factor(CP_all), y = scale(.data[[pgs_var]]))) +
      geom_boxplot() +
      geom_point(position = position_jitter(), alpha = 0.05)
    
    density_plot / box_plot
  }
  
  pgs_distr_by_cp <- map2(pgs_vars, pgs_name, function_pgs_distr_by_cp)
  
  
  # Summary table
  pgs_summary_table <- data %>%
    pivot_longer(cols = all_of(pgs_vars), names_to = "PGS_type", values_to = "value") %>%
    group_by(PGS_type) %>%
    summarise(
      mean_PRS = mean(value, na.rm = TRUE),
      sd_PRS = sd(value, na.rm = TRUE),
      mean_PRS_standardised = mean(scale(value), na.rm = TRUE),
      sd_PRS_standardised = sd(scale(value), na.rm = TRUE)
    )
  
  list(
    histograms = pgs_hist,
    distributions = pgs_distr_by_cp,
    summary_table = pgs_summary_table
  )
  
}

exploration_full <- exploration_function(CP_df_not_related)
exploration_monogenic <- exploration_function(Monogenic_only)
exploration_non_monogenic <- exploration_function(Non_monogenic)
exploration_young525 <- exploration_function(Young525)

save(exploration_full,
     exploration_monogenic,
     exploration_non_monogenic,
     exploration_young525,
     file = "Results/Exploration.RData")
## ----


## Full dataset
## ---- FullExplorePGSDistributionRawStand
walk(exploration_full$histograms, print)
## ----

## ---- FullExplorePGSDistributionRawStandSummaryTable
exploration_full$summary_table %>% kable()
## ----

## ---- FullExploreDistributionbyCP
walk(exploration_full$distributions, print)
## ----


## Monogenic
## ---- MonoExplorePGSDistributionRawStand
walk(exploration_monogenic$histograms, print)
## ----

## ---- MonoExplorePGSDistributionRawStandSummaryTable
exploration_monogenic$summary_table %>% kable()
## ----

## ---- MonoExploreDistributionbyCP
walk(exploration_monogenic$distributions, print)
## ----


## Non-monogenic
## ---- NonmonoExplorePGSDistributionRawStand
walk(exploration_non_monogenic$histograms, print)
## ----

## ---- NonmonoExplorePGSDistributionRawStandSummaryTable
exploration_non_monogenic$summary_table %>% kable()
## ----

## ---- NonmonoExploreDistributionbyCP
walk(exploration_non_monogenic$distributions, print)
## ----


## Youngest 525 controls
## ---- Young525ExplorePGSDistributionRawStand
walk(exploration_young525$histograms, print)
## ----

## ---- Young525ExplorePGSDistributionRawStandSummaryTable
exploration_young525$summary_table %>% kable()
## ----

## ---- Young525ExploreDistributionbyCP
walk(exploration_young525$distributions, print)
## ----

