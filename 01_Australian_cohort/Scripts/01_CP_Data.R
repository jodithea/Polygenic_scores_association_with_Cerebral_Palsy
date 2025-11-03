## ---- LoadPackages
library(igraph)       # for building graph of related individuals
library(knitr)        # for kable tables
library(performance)  # for model checking
library(DHARMa)       # for model checking
library(effects)      # for nice summary of model results (allEffects)
library(broom.mixed)  # for tidy output
library(patchwork)    # for putting together plots into a figure
library(ggplotify)    # to use patchwork on lattice plots (created by allEffects)
library(MuMIn)        # to calculate R2
library(forestmodel)  # for nicely formatted caterpillar plot
library(pROC)         # for ROC curves and AUC
library(tidyverse)
## ----

## ---- LoadData
# List of IDs for 536 individuals to include in the analysis as CP cases (from Aus CP Biobank and undergone QC)
CP_cases <- read.table("/path/SNP_data/CP_case_IDs_QCd.txt", header = F, col.names = "IID")

# List of IDs for 23,902 individuals to include in the analysis as controls (from QSKIN 1 and 2 and undergone QC)
CP_ctrls <- read.table("/path/SNP_data/QSkin_control_IDs_QCd.txt", header = F, col.names = "IID") 

# List of people to exclude = outside a European-centered box in PC1, PC2 which is set at mean +/- 6sd for a set of European 1000G reference individuals
exclude <- read.table("/path/SNP_data/AncestryOutlierExclusions.txt", header = F, col.names = c("FID", "IID")) 

# Ancestry PCs data
PCs <- read.table("/path/SNP_data/AncestryChecks/CP_Adelaide_QSkin_QSkin2_GSA_Ancestry_26Nov2024_filtered_MAFge0.05_PrincipalComponentsScores.txt", header = T) 

# Individs genotyped including genetic sex data
genotyped <- read.table(file = "/path/SNP_data/ImputationRuns/results_HRCr1.1/DerivedFiles/CP_QSkin_HRCr1.1_4Nov2024_chr10.fam", header = F, col.names = c("FID", "IID", "FatherID", "MotherID", "Sex", "Phenotype"))

# PGS data (CT = clumping and thresholding using all SNPs, SBayesRC = using SBayesRC)
Autism_CT <- read.table(file = "/path/PRS/Final_PGS/Autism_CT_S8.profile", header = T) %>% 
  rename(PGS_Autism_CT = SCORESUM) %>% 
  select(-c(PHENO, CNT))
Autism_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/Autism_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_Autism_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

Birthweight_CT <- read.table(file = "/path/PRS/Final_PGS/Birthweight_CT_PRS.txt", header = T) %>% 
  rename(PGS_Birthweight_CT = SCORESUM) %>% 
  select(-c(PHENO, CNT))
Birthweight_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/Birthweight_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_Birthweight_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

CP_Hale_CT <- read.table(file = "/path/PRS/Final_PGS/Cerebral_palsy_CT_PRS.txt", header = T) %>% 
  rename(PGS_CP_Hale_CT = SCORESUM) %>% 
  select(-c(PHENO, CNT))
CP_Hale_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/CerebralPalsy_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_CP_Hale_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

CP_FinnGen_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/CerebralPalsy_Finngen_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_CP_FinnGen_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

EA_CT <- read.table(file = "/path/PRS/Final_PGS/EA_ExclQIMR_CT_PRS.txt", header = T) %>% 
  rename(PGS_EA_CT = SCORESUM) %>% 
  select(-c(PHENO, CNT))
EA_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/EA_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_EA_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

Epilepsy_CT <- read.table(file = "/path/PRS/Final_PGS/Epilepsy_CT_PRS.txt", header = T) %>% 
  rename(PGS_Epilepsy_CT = SCORESUM) %>% 
  select(-c(PHENO, CNT))
Epilepsy_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/Epilepsy_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_Epilepsy_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

Gestdur_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/Gestational_duration_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_Gestdur_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

Stroke_CT <- read.table(file = "/path/PRS/Final_PGS/Stroke_CT_PRS.txt", header = T) %>% 
  rename(PGS_Stroke_CT = SCORESUM) %>% 
  select(-c(PHENO, CNT))
Stroke_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/Stroke_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_Stroke_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

Agewalking_SBayesRC <- read.table(file = "/path/PRS/Final_PGS/AOWGui2025_SBayR_PRS.profile", header = T) %>% 
  rename(PGS_Agewalking_SBayesRC = SCORESUM) %>% 
  select(-c(PHENO, CNT))

# Aus CP Biobank phenotype data
# Sheet with all of the data
CP_pheno <- read.csv("/path/Sample_data/CP-GWAS_clinicalinfo_CP_only_sheet_28.02.25.csv", 
                     header = T, na.strings = "N/A")
# Add this sheet which has age at saliva collection and MRIorUSabnormal
CP_pheno2 <- read.csv("/path/Sample_data/CP-GWAS_clinicalinfo_CP_biobank_20240726_sheet_12.03.25.csv", 
                     header = T, na.strings = "N/A")

# QSKIN 1 and 2 phenotype data
QSKIN1_pheno <- read.table("/path/Sample_data/QSKIN1_60_CerebralPalsy.txt", 
                         header = T, sep = "\t")

QSKIN2_pheno <- read.table("/path/Sample_data/QSKIN2_60_CerebralPalsy.txt", 
                           header = T, sep = "\t")

# Relatedness data
related <- read.table("/path/Cerebral_palsy/Aus_CP_Biobank/Relatedness/Aus_CPBiobank_QSKIN_relatedness.genome",
                      header = T)
## ----


## ---- TidyData
CP_cases <- CP_cases %>% 
  mutate("CP_all" = 1)

CP_ctrls <- CP_ctrls %>% 
  mutate("CP_all" = 0)

# Ancestry PCs
PCs <- PCs %>% 
  rename("FID" = "FAMID",
         "IID" = "INDID")

CP_cases <- CP_cases %>% 
  left_join(PCs)

CP_ctrls <- CP_ctrls %>% 
  left_join(PCs)

# Remove people to exclude (ancestry outliers)
CP_cases <- CP_cases %>% 
  anti_join(exclude) %>% 
  select(-FID)

CP_ctrls <- CP_ctrls %>% 
  anti_join(exclude) %>% 
  select(-FID)

# Genotype data = genetic sex data and retain FID from .fam file
genotyped <- genotyped %>% 
  select(FID, IID, Sex)

# PGS data
PGS_data <- Autism_CT %>% 
  full_join(Autism_SBayesRC, by = join_by(FID, IID)) %>% 
  full_join(Birthweight_CT, by = join_by(FID, IID)) %>% 
  full_join(Birthweight_SBayesRC, by = join_by(FID, IID)) %>% 
  full_join(CP_Hale_CT, by = join_by(FID, IID)) %>% 
  full_join(CP_Hale_SBayesRC, by = join_by(FID, IID)) %>% 
  full_join(CP_FinnGen_SBayesRC, by = join_by(FID, IID)) %>% 
  full_join(EA_CT, by = join_by(FID, IID)) %>% 
  full_join(EA_SBayesRC, by = join_by(FID, IID)) %>% 
  full_join(Epilepsy_CT, by = join_by(FID, IID)) %>% 
  full_join(Epilepsy_SBayesRC, by = join_by(FID, IID)) %>% 
  full_join(Gestdur_SBayesRC, by = join_by(FID, IID)) %>% 
  full_join(Stroke_CT, by = join_by(FID, IID)) %>% 
  full_join(Stroke_SBayesRC, by = join_by(FID, IID)) %>% 
  full_join(Agewalking_SBayesRC, by = join_by(FID, IID)) 


# Combine case and controls IDs with genotype data and PGS data
CP_cases_geno <- CP_cases %>% 
  left_join(genotyped) %>% 
  left_join(PGS_data) %>% 
  rename(Genetic_Sex = Sex)

CP_ctrls_geno <- CP_ctrls %>% 
  left_join(genotyped) %>% 
  left_join(PGS_data) %>%
  rename(Genetic_Sex = Sex)

# Add CP phenotype/clinical data
# Choose cols want from pheno2 dataset: age_consentdate = age blood sample taken, MRIorUSabnormal = Abnormal brain damage pattern found on MRI, CT or Ultrasound
CP_pheno2 <- CP_pheno2 %>% 
  select(IDNumber, age_consentdate, MRIorUSabnormal) %>% 
  rename(age_sample = age_consentdate) %>% 
  mutate(age_sample = as.numeric(age_sample))

CP_pheno_all <- CP_pheno %>% 
  full_join(CP_pheno2, by = join_by("CP_ID" == "IDNumber"))

CP_cases_geno_pheno <- CP_cases_geno %>% 
  left_join(CP_pheno_all, by = join_by("IID" == "Sample_ID"))

# Some individuals genotyped twice so in dataframe twice but have exactly the same data for each entry. Just keep first entry for each
CP_cases_geno_pheno <- CP_cases_geno_pheno %>% 
  distinct(IID, .keep_all = TRUE)

# QSKIN1 phenotype data
# QSKIN1_pheno UID needs QSKIN added as a prefix to UID
# + Baseline survey 2010 - 2011
# + Genetic survey and saliva given 2015 (Q12_ and Q13_)
# + Follow-up survey 2021 (Q7_)
# age_r = age at baseline survey
# Age_saliva = age at genetic survey and when saliva given
# AgeFupSurvey = age at follow-up survey
# Q12_EPILEPSY
# Q12_STROKE
# Q12_ADHD
# Q12_AUTISM
# Q13_PRETERM
# Q7_EPILEPSY
# Q7_STROKE
# Q7_ADHD
# Q7_AUTISM
# Combine Q12 and Q7 for each condition to determine if ever experienced condition

QSKIN1_pheno <- QSKIN1_pheno %>% 
  rename(Sex_reported = Sex,
        Age_baseline = age_r,
         Age_FU = AgeFupSurvey) %>% 
  mutate(IID = paste0("QSKIN", UID),
         EPILEPSY = case_when(
    Q12_EPILEPSY == "Yes" | Q7_EPILEPSY == "1" ~ 1,
    Q12_EPILEPSY == "No" | is.na(Q7_EPILEPSY) ~ 0
  ),
  STROKE = case_when(
    Q12_STROKE == "Yes" | Q7_STROKE == "1" ~ 1,
    Q12_STROKE == "No" | is.na(Q7_STROKE) ~ 0
  ),
  ADHD = case_when(
    Q12_ADHD == "Yes" | Q7_ADHD == "1" ~ 1,
    Q12_ADHD == "No" | is.na(Q7_ADHD) ~ 0
  ),
  AUTISM = case_when(
    Q12_AUTISM == "Yes" | Q7_AUTISM == "1" ~ 1,
    Q12_AUTISM == "No" | is.na(Q7_AUTISM) ~ 0
  ),
  )

QSKIN1_pheno_use <- QSKIN1_pheno %>% 
  select(IID, Age_saliva, EPILEPSY, STROKE, ADHD, AUTISM) %>% 
  rename(age_sample = Age_saliva) %>% 
  mutate(age_sample = as.numeric(age_sample))


# QSKIN2
# QSKIN2_pheno studycode matches IID
# Age_r = age at survey and saliva sample given. Change to Age_saliva so merges with QSKIN1
# Recode Qs so have same col name as QSKIN1 and if have string = 1 and if blank = 0
# Q12_7_0 = Attention Deficit Hyperactivity Disorder (ADHD)
# Q12_8_0 = Autism or Asperger syndrome
# Q12_18_0 = Epilepsy
# Q12_37 = Stroke

QSKIN2_pheno <- QSKIN2_pheno %>% 
  mutate(Age_saliva = Age_r,
         IID = studycode,
         EPILEPSY = case_when(
           Q12_18_0 == "Epilepsy" ~ 1,
           Q12_18_0 == "" ~ 0
         ),
         STROKE = case_when(
           Q12_37 == "Stroke" ~ 1,
           Q12_37 == "" ~ 0
         ),
         ADHD = case_when(
           Q12_7_0 == "Attention Deficit Hyperactivity Disorder (ADHD)" ~ 1,
           Q12_7_0 == "" ~ 0
         ),
         AUTISM = case_when(
           Q12_8_0 == "Autism or Asperger syndrome" ~ 1,
           Q12_8_0 == "" ~ 0
         )
         )

QSKIN2_pheno_use <- QSKIN2_pheno %>% 
  select(IID, Age_saliva, EPILEPSY, STROKE, ADHD, AUTISM) %>% 
  rename(age_sample = Age_saliva) %>% 
  mutate(age_sample = as.numeric(age_sample))

# Combine QSKIN1 and 2
QSKIN12_pheno <- bind_rows(QSKIN1_pheno_use, QSKIN2_pheno_use) %>% 
  rename_with(~ paste0(., "_QSKIN"), .cols = -c(IID, age_sample))

# All pheno and geno data for controls
CP_ctrls_geno_pheno <- CP_ctrls_geno %>% 
  left_join(QSKIN12_pheno, by = join_by("IID" == "IID"))

# Create CP dataframe with cases and controls
CP_df <- CP_cases_geno_pheno %>% 
  full_join(CP_ctrls_geno_pheno) %>% 
  relocate(FID, .before = IID) %>% 
  relocate(CP_ID, .after = IID)

save(CP_df, file = "Data/CP_df.Rdata")
## ----

## ---- Relatedness
ggplot(related, aes(x = PI_HAT)) +
  geom_histogram(binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0, 1, 0.1))

related %>% 
  summarise(mean_pi_hat = mean(PI_HAT),
            min_pi_hat = min(PI_HAT),
            max_pi_hat = max(PI_HAT)) %>% 
  kable()

related %>% 
  filter(FID1 == "CP_Adelaide_GSA" & FID2 == "CP_Adelaide_GSA") %>% 
  summarise(
    n_pihat_0.09 = sum(PI_HAT > 0.09),
    n_pihat_0.09375 = sum(PI_HAT > 0.09375),
    n_pihat_0.125 = sum(PI_HAT > 0.125),
    n_pihat_0.1875 = sum(PI_HAT > 0.1875),
    n_pihat_0.25 = sum(PI_HAT > 0.25),
    n_pihat_0.5 = sum(PI_HAT > 0.5)
  ) %>% 
  kable(caption = "no. cases lost if pi hat is > 0.09375 (halfway between 3rd and 4th - use as cut off for 3rd deg relative), 0.125 (3rd deg relative or more), 0.1875 (halfway between 3rd and 2nd - often used as a cut-off for 2nd deg relatives), 0.25 (2nd degree relative or more), 0.5 (1st degree relative or more")
## ----

## ---- RemoveRelatedness
# Remove 1 individual from each pair that have pi_hat > 0.1875 with controls preferentially removed.

# Df with case/ctrl status for each IID
CP_status <- CP_df %>% 
  select(IID, CP_all)

# Join related df with CP status so have case/control status for both IID1 and IID2
related_CP_status <- related %>%
  left_join(CP_status, by = c("IID1" = "IID")) %>%
  rename(CP1 = CP_all) %>%
  left_join(CP_status, by = c("IID2" = "IID")) %>%
  rename(CP2 = CP_all)

# Filter pairs with PI_HAT > 0.09375, preferentially removing controls and keeping cases
# Making sure to account for individuals in multiple relationship 'pairs'

# Use igraph to build a graph of related individuals and then identify connected components (clusters of overlapping relationships)
# Build edges from related pairs
edges <- related_CP_status %>%
  filter(PI_HAT > 0.09375) %>%
  select(IID1, IID2)

# Build undirected graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Find clusters (connected components)
clusters <- components(g)

# sizes of clusters (number of overlapping IDs)
table(clusters$csize)  

# Prune each cluster by removing controls first, and retaining as many cases as possible
# Attach case/control info to vertices
cp_lookup <- CP_status %>%
  select(IID, CP_all) %>%
  rename(CP = CP_all)

V(g)$CP <- cp_lookup$CP[match(V(g)$name, cp_lookup$IID)]

set.seed(3624)  # ensures reproducibility

to_remove <- character()

for (cl in unique(clusters$membership)) {
  members <- names(clusters$membership[clusters$membership == cl])
  
  # If only 1 person in cluster, keep them
  if (length(members) == 1) next
  
  # Case/control status of members
  member_cp <- V(g)$CP[match(members, V(g)$name)]
  
  # Prefer to keep cases
  controls <- members[member_cp == 0]
  
  if (length(controls) > 0) {
    # Remove all controls, keep cases
    to_remove <- c(to_remove, controls)
  } else {
    # All are cases -> randomly remove all but one (reproducible with set.seed)
    keep_one <- sample(members, 1)  
    to_remove <- c(to_remove, setdiff(members, keep_one))
  }
}

# Remove these individuals from df
CP_df_not_related <- CP_df %>%
  anti_join(tibble(IID = to_remove), by = "IID")


save(CP_df_not_related, file = "Data/CP_df_not_related.Rdata")
## ----

## ---- StratDF
Monogenic_only <- CP_df_not_related %>% 
  filter((CP_all == 0) | (genetic == "Yes" & CP_all == 1))

save(Monogenic_only, file = "Data/Monogenic_only_df.RData")

Non_monogenic <- CP_df_not_related %>% 
  filter((CP_all == 0) | (genetic == "No" & CP_all == 1))

save(Non_monogenic, file = "Data/Non_monogenic_df.RData")
## ----

## ---- AgeDF
cases <- CP_df_not_related %>% 
  filter(CP_all == 1)

Young525_ctrls <- CP_df_not_related %>% 
  filter(CP_all == 0) %>% 
  arrange(age_sample) %>% 
  slice(1:525)

Young525 <- cases %>% 
  bind_rows(Young525_ctrls)

save(Young525, file = "Data/Young525_df.RData")
## ----

