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
# Load MyCode data ----
mycode_sample_manifest <- read.delim("mycode_sample_manifest.txt")
colnames(mycode_sample_manifest)[1] <- "SEQN_ID"

# Demographics file
demographics <- read.delim("demographics.csv", sep = "|")

# combine dataframes
mycode_data <- inner_join(demographics,mycode_sample_manifest, by="SEQN_ID") # Drop down to 169269. A loss of 3238 samples

# Add an age at last visit column
# Need to split birth date and time into separate columns
mycode_data <- separate(data = mycode_data, col = PT_BIRTH_DT, sep = " ", into = c("PT_BIRTH_DT", "PT_BIRTH_TIME"))
# Remove birth time column
mycode_data <- subset(mycode_data, select = -c(PT_BIRTH_TIME))
# Convert visit date and birth date to dates
mycode_data$LAST_ENCOUNTER_DT <- as.Date(mycode_data$LAST_ENCOUNTER_DT)
mycode_data$PT_BIRTH_DT <- as.Date(mycode_data$PT_BIRTH_DT)
# Make an age_at_last_visit column (years)
mycode_data <- mycode_data %>%
  mutate(age_at_last_visit = (LAST_ENCOUNTER_DT - PT_BIRTH_DT)/365)
# Convert to a numeric
mycode_data$age_at_last_visit <- as.numeric(mycode_data$age_at_last_visit)

# Add ancestry
mycode_pop_pred <- read_tsv("mycode_pop_pred.txt")
mycode_pop_pred <- mycode_pop_pred %>%
  dplyr::select(IID,POP_PRED,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10)
colnames(mycode_pop_pred) <- c("SEQN_ID","POP_PRED","PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")
mycode_pop_pred <- mycode_pop_pred %>%
  mutate(PT_ID = str_extract(mycode_pop_pred$SEQN_ID,"PT[^_]+")) %>% 
  dplyr::select(PT_ID,POP_PRED,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8,PC9,PC10) 

mycode_data <- left_join(mycode_data,mycode_pop_pred, by="PT_ID") 

# Load CP cases ----
CP_IDs <- read.delim("CP_cases_after_review.txt")

# Add CP cases
mycode_data <- mycode_data %>%
  mutate(CP_all = ifelse(PT_ID %in% CP_IDs$PT_ID,1,0))

# EUR subset ----
mycode_data_eur <- mycode_data %>%
  filter(PT_SEX != "Unknown") %>%
  filter(!is.na(age_at_last_visit)) %>%
  filter(POP_PRED == "EUR")

# Load PRS ----

add_prs_to_data <- function(data, prs_file_path, phenotype) {
  # Read the PRS file
  PRS <- read.delim(prs_file_path, header = TRUE, sep = "\t")
  
  # Extract PT_ID
  PRS <- PRS %>%
    mutate(PT_ID = stringr::str_extract(IID, "PT[^_]+"))
  
  # Rename columns dynamically
  new_colnames <- c(
    paste0(phenotype, "_Named_Allele_Dosage_Sum"),
    paste0("PGS_",phenotype, "_SBayesRC")
  )
  
  if (ncol(PRS) >= 5) {
    colnames(PRS)[4:5] <- new_colnames
  } else {
    stop("PRS file does not have enough columns to rename.")
  }
  
  # Select relevant columns
  PRS <- PRS %>%
    dplyr::select(PT_ID, all_of(new_colnames[2]))
  
  # Merge with main dataset
  data <- dplyr::left_join(data, PRS, by = "PT_ID")
  
  # Check if the PRS column exists and is not all NA
  prs_col <- new_colnames[2]
  if (!prs_col %in% colnames(data)) {
    stop(paste("Column", prs_col, "not found in merged data."))
  }
  if (all(is.na(data[[prs_col]]))) {
    stop(paste("Column", prs_col, "contains only NA values."))
  }
  
  # Add z-score column as numeric vector
  z_col <- paste0(phenotype, "_PRS_z_score")
  data[[z_col]] <- as.numeric(scale(data[[prs_col]]))
  
  return(data)
}


mycode_data_eur <- add_prs_to_data(
  data = mycode_data_eur,
  prs_file_path = "/MyCode_Autism_GRCh38_topmed.sscore",
  phenotype = "Autism"
)
mycode_data_eur <- add_prs_to_data(
  data = mycode_data_eur,
  prs_file_path = "/MyCode_Birthweight_GRCh38_topmed.sscore",
  phenotype = "Birthweight"
)
mycode_data_eur <- add_prs_to_data(
  data = mycode_data_eur,
  prs_file_path = "/MyCode_CP_FinnGen_GRCh38_topmed.sscore",
  phenotype = "CP"
)
mycode_data_eur <- add_prs_to_data(
  data = mycode_data_eur,
  prs_file_path = "/MyCode_EA_GRCh38_topmed.sscore",
  phenotype = "EA"
)
mycode_data_eur <- add_prs_to_data(
  data = mycode_data_eur,
  prs_file_path = "/MyCode_Epilepsy_GRCh38_topmed.sscore",
  phenotype = "Epilepsy"
)
mycode_data_eur <- add_prs_to_data(
  data = mycode_data_eur,
  prs_file_path = "/MyCode_Gestdur_GRCh38_topmed.sscore",
  phenotype = "Gestdur"
)
mycode_data_eur <- add_prs_to_data(
  data = mycode_data_eur,
  prs_file_path = "/MyCode_Stroke_GRCh38_topmed.sscore",
  phenotype = "Stroke"
)

mycode_data_eur <- add_prs_to_data(
  data = mycode_data_eur,
  prs_file_path = "/MyCode_Agewalking_GRCh38_topmed.sscore",
  phenotype = "Agewalking"
)

# Generate CP cohort  ----
cp_cohort <- mycode_data_eur %>%
  filter(PT_SEX != "Unknown") %>%
  filter(!is.na(age_at_last_visit)) %>%
  filter(!(ADMI_FLAG == "1" & CP_all == "0")) %>% # Removes Department of Developmental Medicine patients from controls
  filter(!is.na(CP_PRS_z_score)) %>%
  mutate(Genetic_Sex = case_when(PT_SEX == "Male" ~ "1",
                         PT_SEX == "Female" ~ "2"))

# Load relatives 
cp_relatives <- read.delim("cp_cohort_with_controls_relationships.txt")

# Remove relatives prioritizing CP cases
CP_df_not_related <- cp_cohort %>%
  filter(PT_ID %in% filter(cp_relatives,remove_to_get_maximally_unrelated_cohort_prioritizing_cp_participants == "0")$short_id)


# Match ----
matchit <- matchit(CP_all ~ age_at_last_visit,
                   method="nearest", 
                   distance = "glm",
                   data = CP_df_not_related,
                   ratio = 5)
CP_df_not_related <- match.data(matchit)

save(CP_df_not_related, file = "/Results/CP_df_not_related.RData")

