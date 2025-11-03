## ---- LoadDataFrame
load(file = "Data/CP_df_not_related.Rdata")
load(file = "Data/Monogenic_only_df.RData")
load(file = "Data/Non_monogenic_df.RData")
load(file = "Data/Young525_df.RData")
## ----

## ----SampleSizesFullDF
# CP sample size
CP_df_not_related %>% 
  count(CP_all) %>% 
  kable(caption = "Sample size of Cerebral Palsy (cases = 1, controls = 0) in which all Aus CP Biobank individuals are cases and all QSKIN individuals are controls (after QC)")

# Is sex data available for all?
CP_df_not_related %>% 
  filter(is.na(Genetic_Sex)) %>% 
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on sex")

# Sex sample sizes
CP_df_not_related %>%
  group_by(CP_all, Genetic_Sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, Genetic_Sex) %>%
  kable(caption = "Sample size and sex percentages (1 = male, 2 = female), grouped by Cerebral palsy status (0 = control, 1 = case)")

# Compare sex across cases and controls
chisq.test(table(CP_df_not_related$Genetic_Sex, CP_df_not_related$CP_all))

# Is ancestry PCs data available for all?
CP_df_not_related %>% 
  filter(if_any(starts_with("PC"), is.na)) %>%   
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on ancestry PCs")

# Is age data available for all?
CP_df_not_related %>% 
  filter(is.na(age_sample)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on age at sampling, grouped by cerebral palsy status (0 = control, 1 = case)")

# Age range, mean and SD
CP_df_not_related %>% 
  filter(!is.na(age_sample)) %>%
  group_by(CP_all) %>% 
  summarise(mean_age = mean(age_sample),
            sd_age = sd(age_sample),
            min_age = min(age_sample),
            max_age = max(age_sample)) %>% 
  kable(caption = "Age characteristics, grouped by cerebral palsy status ( = control, 1 = case)")

# Compare ages across cases and controls (assume normally distributed - large sample so assume central limit theorem)
t.test(age_sample ~ CP_all, data = CP_df_not_related)

# Birth weight range, mean and SD
CP_df_not_related %>% 
  filter(!is.na(birthwtgrams),
         birthwtgrams != "unknown") %>%
  mutate(birthwtgrams = as.numeric(birthwtgrams)) %>% 
  group_by(CP_all) %>% 
  summarise(mean = mean(birthwtgrams),
            sd = sd(birthwtgrams),
            min = min(birthwtgrams),
            max = max(birthwtgrams)) %>% 
  kable(caption = "Birth weight (g) characteristics, grouped by cerebral palsy status ( = control, 1 = case)")


CP_df_not_related %>% 
  filter(!is.na(birthwtgrams),
         birthwtgrams == "unknown") %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with birth weight unknown")

CP_df_not_related %>% 
  filter(is.na(birthwtgrams)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with birth weight missing")
  
  
# Gestational duration range, mean and SD
CP_df_not_related %>% 
  filter(!is.na(gestation),
         gestation != "unknown") %>%
  mutate(gestation = as.numeric(gestation)) %>% 
  group_by(CP_all) %>% 
  summarise(mean = mean(gestation),
            sd = sd(gestation),
            min = min(gestation),
            max = max(gestation)) %>% 
  kable(caption = "Gestational duration (days) characteristics, grouped by cerebral palsy status ( = control, 1 = case)")

CP_df_not_related %>% 
  filter(!is.na(gestation),
         gestation == "unknown") %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with gestational duration unknown")

CP_df_not_related %>% 
  filter(is.na(gestation)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with gestational duration missing")


# GMFCS sample sizes
CP_df_not_related %>%
  group_by(CP_all, gmfcs) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, gmfcs) %>%
  kable(caption = "Sample size and percentages of gmfcs, grouped by Cerebral palsy status (0 = control, 1 = case)")

# Monogeneic diagnosis for CP sample sizes
CP_df_not_related %>%
  group_by(CP_all, genetic) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, genetic) %>%
  kable(caption = "Sample size and percentages of individuals with a monogenic diagnosis for CP, grouped by Cerebral palsy status (0 = control, 1 = case)")
## ----

## ----SampleSizesMonoDF
# CP sample size
Monogenic_only %>% 
  count(CP_all) %>% 
  kable(caption = "Sample size of Cerebral Palsy (cases = 1, controls = 0) in which Aus CP Biobank individuals with a monogenic diagnosis are cases and all QSKIN individuals are controls (after QC)")

# Is sex data available for all?
Monogenic_only %>% 
  filter(is.na(Genetic_Sex)) %>% 
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on sex")

# Sex sample sizes
Monogenic_only %>%
  group_by(CP_all, Genetic_Sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, Genetic_Sex) %>%
  kable(caption = "Sample size and sex percentages (1 = male, 2 = female), grouped by Cerebral palsy status (0 = control, 1 = case)")

# Compare sex across cases and controls
chisq.test(table(Monogenic_only$Genetic_Sex, Monogenic_only$CP_all))

# Is ancestry PCs data available for all?
Monogenic_only %>% 
  filter(if_any(starts_with("PC"), is.na)) %>%   
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on ancestry PCs")

# Is age data available for all?
Monogenic_only %>% 
  filter(is.na(age_sample)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on age at sampling, grouped by cerebral palsy status (0 = control, 1 = case)")

# Age range, mean and SD
Monogenic_only %>% 
  filter(!is.na(age_sample)) %>%
  group_by(CP_all) %>% 
  summarise(mean_age = mean(age_sample),
            sd_age = sd(age_sample),
            min_age = min(age_sample),
            max_age = max(age_sample)) %>% 
  kable(caption = "Age characteristics, grouped by cerebral palsy status ( = control, 1 = case)")

# Compare ages across cases and controls (assume normally distributed - large sample so assume central limit theorem)
t.test(age_sample ~ CP_all, data = Monogenic_only)

# Birth weight range, mean and SD
Monogenic_only %>% 
  filter(!is.na(birthwtgrams),
         birthwtgrams != "unknown") %>%
  mutate(birthwtgrams = as.numeric(birthwtgrams)) %>% 
  group_by(CP_all) %>% 
  summarise(mean = mean(birthwtgrams),
            sd = sd(birthwtgrams),
            min = min(birthwtgrams),
            max = max(birthwtgrams)) %>% 
  kable(caption = "Birth weight (g) characteristics, grouped by cerebral palsy status ( = control, 1 = case)")


Monogenic_only %>% 
  filter(!is.na(birthwtgrams),
         birthwtgrams == "unknown") %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with birth weight unknown")

Monogenic_only %>% 
  filter(is.na(birthwtgrams)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with birth weight missing")


# Gestational duration range, mean and SD
Monogenic_only %>% 
  filter(!is.na(gestation),
         gestation != "unknown") %>%
  mutate(gestation = as.numeric(gestation)) %>% 
  group_by(CP_all) %>% 
  summarise(mean = mean(gestation),
            sd = sd(gestation),
            min = min(gestation),
            max = max(gestation)) %>% 
  kable(caption = "Gestational duration (days) characteristics, grouped by cerebral palsy status ( = control, 1 = case)")

Monogenic_only %>% 
  filter(!is.na(gestation),
         gestation == "unknown") %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with gestational duration unknown")

Monogenic_only %>% 
  filter(is.na(gestation)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with gestational duration missing")


# GMFCS sample sizes
Monogenic_only %>%
  group_by(CP_all, gmfcs) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, gmfcs) %>%
  kable(caption = "Sample size and percentages of gmfcs, grouped by Cerebral palsy status (0 = control, 1 = case)")
## ----


## ----SampleSizesNonMonoDF
# CP sample size
Non_monogenic %>% 
  count(CP_all) %>% 
  kable(caption = "Sample size of Cerebral Palsy (cases = 1, controls = 0) in which Aus CP Biobank individuals without a monogenic diagnosis are cases and all QSKIN individuals are controls (after QC)")

# Is sex data available for all?
Non_monogenic %>% 
  filter(is.na(Genetic_Sex)) %>% 
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on sex")

# Sex sample sizes
Non_monogenic %>%
  group_by(CP_all, Genetic_Sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, Genetic_Sex) %>%
  kable(caption = "Sample size and sex percentages (1 = male, 2 = female), grouped by Cerebral palsy status (0 = control, 1 = case)")

# Compare sex across cases and controls
chisq.test(table(Non_monogenic$Genetic_Sex, Non_monogenic$CP_all))

# Is ancestry PCs data available for all?
Non_monogenic %>% 
  filter(if_any(starts_with("PC"), is.na)) %>%   
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on ancestry PCs")

# Is age data available for all?
Non_monogenic %>% 
  filter(is.na(age_sample)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on age at sampling, grouped by cerebral palsy status (0 = control, 1 = case)")

# Age range, mean and SD
Non_monogenic %>% 
  filter(!is.na(age_sample)) %>%
  group_by(CP_all) %>% 
  summarise(mean_age = mean(age_sample),
            sd_age = sd(age_sample),
            min_age = min(age_sample),
            max_age = max(age_sample)) %>% 
  kable(caption = "Age characteristics, grouped by cerebral palsy status ( = control, 1 = case)")

# Compare ages across cases and controls (assume normally distributed - large sample so assume central limit theorem)
t.test(age_sample ~ CP_all, data = Non_monogenic)

# Birth weight range, mean and SD
Non_monogenic %>% 
  filter(!is.na(birthwtgrams),
         birthwtgrams != "unknown") %>%
  mutate(birthwtgrams = as.numeric(birthwtgrams)) %>% 
  group_by(CP_all) %>% 
  summarise(mean = mean(birthwtgrams),
            sd = sd(birthwtgrams),
            min = min(birthwtgrams),
            max = max(birthwtgrams)) %>% 
  kable(caption = "Birth weight (g) characteristics, grouped by cerebral palsy status ( = control, 1 = case)")


Non_monogenic %>% 
  filter(!is.na(birthwtgrams),
         birthwtgrams == "unknown") %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with birth weight unknown")

Non_monogenic %>% 
  filter(is.na(birthwtgrams)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with birth weight missing")


# Gestational duration range, mean and SD
Non_monogenic %>% 
  filter(!is.na(gestation),
         gestation != "unknown") %>%
  mutate(gestation = as.numeric(gestation)) %>% 
  group_by(CP_all) %>% 
  summarise(mean = mean(gestation),
            sd = sd(gestation),
            min = min(gestation),
            max = max(gestation)) %>% 
  kable(caption = "Gestational duration (days) characteristics, grouped by cerebral palsy status ( = control, 1 = case)")

Non_monogenic %>% 
  filter(!is.na(gestation),
         gestation == "unknown") %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with gestational duration unknown")

Non_monogenic %>% 
  filter(is.na(gestation)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with gestational duration missing")


# GMFCS sample sizes
Non_monogenic %>%
  group_by(CP_all, gmfcs) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, gmfcs) %>%
  kable(caption = "Sample size and percentages of gmfcs, grouped by Cerebral palsy status (0 = control, 1 = case)")
## ----




## ----SampleSizesYoung525DF
# CP sample size
Young525 %>% 
  count(CP_all) %>% 
  kable(caption = "Sample size of Cerebral Palsy (cases = 1, controls = 0) in which all Aus CP Biobank individuals are cases and QSKIN individuals restrcited to the youngest 525 individuals are controls (after QC)")

# Is sex data available for all?
Young525 %>% 
  filter(is.na(Genetic_Sex)) %>% 
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on sex")

# Sex sample sizes
Young525 %>%
  group_by(CP_all, Genetic_Sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, Genetic_Sex) %>%
  kable(caption = "Sample size and sex percentages (1 = male, 2 = female), grouped by Cerebral palsy status (0 = control, 1 = case)")

# Compare sex across cases and controls
chisq.test(table(Young525$Genetic_Sex, Young525$CP_all))

# Is ancestry PCs data available for all?
Young525 %>% 
  filter(if_any(starts_with("PC"), is.na)) %>%   
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on ancestry PCs")

# Is age data available for all?
Young525 %>% 
  filter(is.na(age_sample)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "Number of individuals missing data on age at sampling, grouped by cerebral palsy status (0 = control, 1 = case)")

# Age range, mean and SD
Young525 %>% 
  filter(!is.na(age_sample)) %>%
  group_by(CP_all) %>% 
  summarise(mean_age = mean(age_sample),
            sd_age = sd(age_sample),
            min_age = min(age_sample),
            max_age = max(age_sample)) %>% 
  kable(caption = "Age characteristics, grouped by cerebral palsy status ( = control, 1 = case)")

# Compare ages across cases and controls (assume normally distributed - large sample so assume central limit theorem)
t.test(age_sample ~ CP_all, data = Young525)

# Birth weight range, mean and SD
Young525 %>% 
  filter(!is.na(birthwtgrams),
         birthwtgrams != "unknown") %>%
  mutate(birthwtgrams = as.numeric(birthwtgrams)) %>% 
  group_by(CP_all) %>% 
  summarise(mean = mean(birthwtgrams),
            sd = sd(birthwtgrams),
            min = min(birthwtgrams),
            max = max(birthwtgrams)) %>% 
  kable(caption = "Birth weight (g) characteristics, grouped by cerebral palsy status ( = control, 1 = case)")


Young525 %>% 
  filter(!is.na(birthwtgrams),
         birthwtgrams == "unknown") %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with birth weight unknown")

Young525 %>% 
  filter(is.na(birthwtgrams)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with birth weight missing")


# Gestational duration range, mean and SD
Young525 %>% 
  filter(!is.na(gestation),
         gestation != "unknown") %>%
  mutate(gestation = as.numeric(gestation)) %>% 
  group_by(CP_all) %>% 
  summarise(mean = mean(gestation),
            sd = sd(gestation),
            min = min(gestation),
            max = max(gestation)) %>% 
  kable(caption = "Gestational duration (days) characteristics, grouped by cerebral palsy status ( = control, 1 = case)")

Young525 %>% 
  filter(!is.na(gestation),
         gestation == "unknown") %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with gestational duration unknown")

Young525 %>% 
  filter(is.na(gestation)) %>%
  group_by(CP_all) %>% 
  summarise(n = n()) %>% 
  kable(caption = "No. individuals with gestational duration missing")


# GMFCS sample sizes
Young525 %>%
  group_by(CP_all, gmfcs) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CP_all) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(CP_all, gmfcs) %>%
  kable(caption = "Sample size and percentages of gmfcs, grouped by Cerebral palsy status (0 = control, 1 = case)")
## ----



## ----CheckAgeDistribution
ggplot(CP_df_not_related %>% filter(CP_all == 1), aes(x = age_sample)) +
  geom_histogram() +
  ggtitle("Age distribution of cases")

ggplot(CP_df_not_related %>% filter(CP_all == 0), aes(x = age_sample)) +
  geom_histogram() +
  ggtitle("Age distribution of controls")

ggplot(Young525 %>% filter(CP_all == 0), aes(x = age_sample)) +
  geom_histogram() +
  ggtitle("Age distribution of controls restricted to youngest 525")

## ----
