# Sanity check PGS 

## ---- LoadDataFrame
load(file = "Data/CP_df_not_related.Rdata")
load(file = "Data/Monogenic_only_df.RData")
load(file = "Data/Non_monogenic_df.RData")
load(file = "Data/Young525_df.RData")
## ----

## ---- CheckPGSEpilepsy
model_epilepsy_sbayesrc <- glm(EPILEPSY_QSKIN ~ scale(PGS_Epilepsy_SBayesRC) + 
                                 Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
    family = binomial(link = 'logit'), 
    data = CP_df_not_related)

tidy(model_epilepsy_sbayesrc, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_Epilepsy_SBayesRC)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")


model_epilepsy_sbayesct <- glm(EPILEPSY_QSKIN ~ scale(PGS_Epilepsy_CT) + 
                                 Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                               family = binomial(link = 'logit'), 
                               data = CP_df_not_related)

tidy(model_epilepsy_sbayesct, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_Epilepsy_CT)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")
## ----

## ---- CheckPGSStroke
model_stroke_sbayesrc <- glm(STROKE_QSKIN ~ scale(PGS_Stroke_SBayesRC) + 
                                 Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                               family = binomial(link = 'logit'), 
                               data = CP_df_not_related)

tidy(model_stroke_sbayesrc, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_Stroke_SBayesRC)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")


model_stroke_sbayesct <- glm(STROKE_QSKIN ~ scale(PGS_Stroke_CT) + 
                                 Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                               family = binomial(link = 'logit'), 
                               data = CP_df_not_related)

tidy(model_stroke_sbayesct, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_Stroke_CT)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")
## ----

## ---- CheckPGSAutism
model_autism_sbayesrc <- glm(AUTISM_QSKIN ~ scale(PGS_Autism_SBayesRC) + 
                               Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                             family = binomial(link = 'logit'), 
                             data = CP_df_not_related)

tidy(model_autism_sbayesrc, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_Autism_SBayesRC)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")


model_autism_sbayesct <- glm(AUTISM_QSKIN ~ scale(PGS_Autism_CT) + 
                               Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                             family = binomial(link = 'logit'), 
                             data = CP_df_not_related)

tidy(model_autism_sbayesct, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_Autism_CT)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")
## ----


## ---- CheckPGSCP
model_cp_hale_sbayesrc <- glm(CP_all ~ scale(PGS_CP_Hale_SBayesRC) + 
                                 Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                               family = binomial(link = 'logit'), 
                               data = CP_df_not_related)

tidy(model_cp_hale_sbayesrc, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_CP_Hale_SBayesRC)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")


model_cp_hale_ct <- glm(EPILEPSY_QSKIN ~ scale(PGS_CP_Hale_CT) + 
                                 Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                               family = binomial(link = 'logit'), 
                               data = CP_df_not_related)

tidy(model_cp_hale_ct, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_CP_Hale_CT)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")

model_cp_finngen_sbayesrc <- glm(EPILEPSY_QSKIN ~ scale(PGS_CP_FinnGen_SBayesRC) + 
                          Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                        family = binomial(link = 'logit'), 
                        data = CP_df_not_related)

tidy(model_cp_finngen_sbayesrc, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE) %>% 
  filter(term == "scale(PGS_CP_FinnGen_SBayesRC)") %>% 
  kable(caption = "Back-transformed results (i.e. estimate = odds ratio")
## ----


## ---- CheckPGSBirthWeight
model_bw_sbayesrc <- glm(as.numeric(birthwtgrams) ~ scale(PGS_Birthweight_SBayesRC) + 
                           Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                         family = gaussian(link = 'identity'), 
                         data = CP_df_not_related %>% filter(birthwtgrams != "unknown"))

tidy(model_bw_sbayesrc, conf.int = TRUE, conf.level = 0.95) %>% 
  filter(term == "scale(PGS_Birthweight_SBayesRC)") %>% 
  kable(caption = "Linear regression results (i.e. estimate = beta)")


model_bw_sbayesct <- glm(as.numeric(birthwtgrams) ~ scale(PGS_Birthweight_CT) + 
                           Genetic_Sex + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
                         family = gaussian(link = 'identity'),
                         data = CP_df_not_related %>% filter(birthwtgrams != "unknown"))

tidy(model_bw_sbayesct, conf.int = TRUE, conf.level = 0.95) %>% 
  filter(term == "scale(PGS_Birthweight_CT)") %>% 
  kable(caption = "Linear regression results (i.e. estimate = beta)")
## ----
