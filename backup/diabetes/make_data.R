# Author: Madison Coots
# Date: January 10, 2023
# ----------------------
# This file pulls all of the requisite NHANES data tables (from 2011-2018)
# and performs a join for the analysis. The following tables are downloaded:
# 1. Demographics (DEMO)
# 2. Diabetes questionnaire responses (DIQ)
# 3. Body measurements (BMX)
# 4. Glycohemoglobin measurements (GHB)
# 5. Health insurance status (HIQ)
# 6. Food insecurity data (FSQ)
# 7. Physical activity data (PAQ)
# 8. Medical conditions data (MCQ)
# 9. Mental health data (DPQ)
# 10. Weight history (WHQ)
#
# After joining the tables across years, this script implements the same
# filtering on age, BMI, and pregnancy status described in Aggarwal et al. (2022).
# We also replicate this paper's method of constructing the diabetes labels.
#
# The full data object is written to the path specified in write_path.

library(haven)
library(tidyverse)
library(janitor)

write_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/"

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DEMO_G.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DIQ_G.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BMX_G.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GHB_G.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/HIQ_G.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/FSQ_G.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/PAQ_G.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/MCQ_G.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DPQ_G.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/WHQ_G.XPT", whq <- tempfile(), mode="wb")
raw_demographics_11_12 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_11_12 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_body_measurements_11_12 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_11_12 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_health_ins_11_12 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_11_12 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_11_12 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_11_12 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_11_12 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_weight_hist_11_12 <- foreign::read.xport(whq) %>% 
  clean_names()


# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DIQ_H.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BMX_H.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/GHB_H.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/HIQ_H.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/FSQ_H.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/PAQ_H.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/MCQ_H.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DPQ_H.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/WHQ_H.XPT", whq <- tempfile(), mode="wb")
raw_demographics_13_14 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_13_14 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_body_measurements_13_14 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_13_14 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_health_ins_13_14 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_13_14 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_13_14 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_13_14 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_13_14 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_weight_hist_13_14 <- foreign::read.xport(whq) %>% 
  clean_names()


# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DIQ_I.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BMX_I.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/GHB_I.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/HIQ_I.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/FSQ_I.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/PAQ_I.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/MCQ_I.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DPQ_I.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/WHQ_I.XPT", whq <- tempfile(), mode="wb")
raw_demographics_15_16 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_15_16 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_body_measurements_15_16 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_15_16 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_health_ins_15_16 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_15_16 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_15_16 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_15_16 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_15_16 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_weight_hist_15_16 <- foreign::read.xport(whq) %>% 
  clean_names()

# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DIQ_J.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BMX_J.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/GHB_J.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/HIQ_J.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/FSQ_J.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/PAQ_J.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/MCQ_J.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DPQ_J.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/WHQ_J.XPT", whq <- tempfile(), mode="wb")
raw_demographics_17_18 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_17_18 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_body_measurements_17_18 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_17_18 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_health_ins_17_18 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_17_18 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_17_18 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_17_18 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_17_18 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_weight_hist_17_18 <- foreign::read.xport(whq) %>% 
  clean_names()

# ------------------- Combine -------------------

# Demographics data
raw_demographics_all <- bind_rows(
  raw_demographics_11_12,
  raw_demographics_13_14,
  raw_demographics_15_16,
  raw_demographics_17_18
)

# Diabetes survey data
raw_survey_responses_all <- bind_rows(
  raw_survey_responses_11_12,
  raw_survey_responses_13_14,
  raw_survey_responses_15_16,
  raw_survey_responses_17_18
)

# Body measurements data
raw_body_measurements_all <- bind_rows(
  raw_body_measurements_11_12,
  raw_body_measurements_13_14,
  raw_body_measurements_15_16,
  raw_body_measurements_17_18
)

# Glycohemoglobin data
raw_glycohemoglobin_all <- bind_rows(
  raw_glycohemoglobin_11_12,
  raw_glycohemoglobin_13_14,
  raw_glycohemoglobin_15_16,
  raw_glycohemoglobin_17_18
)

# Health insurance data
raw_health_ins_all <- bind_rows(
  raw_health_ins_11_12,
  raw_health_ins_13_14,
  raw_health_ins_15_16,
  raw_health_ins_17_18
)

# Food insecurity data
raw_food_insec_all <- bind_rows(
  raw_food_insec_11_12,
  raw_food_insec_13_14,
  raw_food_insec_15_16,
  raw_food_insec_17_18
)

# Physical activity data
raw_physical_act_all <- bind_rows(
  raw_physical_act_11_12,
  raw_physical_act_13_14,
  raw_physical_act_15_16,
  raw_physical_act_17_18
)

# Medical conditions data
raw_medical_cond_all <- bind_rows(
  raw_medical_cond_11_12,
  raw_medical_cond_13_14,
  raw_medical_cond_15_16,
  raw_medical_cond_17_18
)

# Mental health data
raw_mental_health_all <- bind_rows(
  raw_mental_health_11_12,
  raw_mental_health_13_14,
  raw_mental_health_15_16,
  raw_mental_health_17_18
)

# Weight history data
raw_weight_hist_all <- bind_rows(
  raw_weight_hist_11_12,
  raw_weight_hist_13_14,
  raw_weight_hist_15_16,
  raw_weight_hist_17_18
)



























data <- raw_demographics_all %>%
  inner_join(raw_survey_responses_all, by = c("seqn")) %>%
  inner_join(raw_body_measurements_all, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin_all, by = c("seqn")) %>%
  inner_join(raw_health_ins_all, by = c("seqn")) %>%
  inner_join(raw_food_insec_all, by = c("seqn")) %>%
  inner_join(raw_physical_act_all, by = c("seqn")) %>%
  inner_join(raw_medical_cond_all, by = c("seqn")) %>%
  inner_join(raw_mental_health_all, by = c("seqn")) %>%
  inner_join(raw_weight_hist_all, by = c("seqn")) %>%
  filter(ridageyr >= 18) %>% # Taken from Supplement
  filter(ridageyr <= 70) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  filter(bmxbmi >= 18.5, # Taken from Supplement
         bmxbmi <= 50) %>%
  # Making the race variable more readable
  mutate(
    race = case_when(ridreth3 == 1 | ridreth3 == 2 ~ "Hispanic",
                     ridreth3 == 3 ~ "White",
                     ridreth3 == 4 ~ "Black",
                     ridreth3 == 6 ~ "Asian"),
    race = factor(race),
    # Re-leveling the race factor, so that White is base level (as in paper)
    race = relevel(race, ref = "White")) %>%
  # Recoding the gender covariate
  mutate(gender = case_when(riagendr == 2 ~ "Woman",
                            riagendr == 1 ~ "Man",
                            riagendr == "." ~ as.character(NA))) %>%
  # Recoding the education codes to interpretable values (dmdeduc2 and dmdeduc3 record educ for diff age groups)
  mutate(dmdeduc3_adj = case_when(dmdeduc3 %in% c(0,1,2,3,4,5,6,7,8,55,66) ~ "Less than 9th grade",
                                  dmdeduc3 %in% c(9,10,11,12) ~ "9-11th grade (Includes 12th grade with no diploma)",
                                  dmdeduc3 %in% c(13,14) ~ "High school graduate/GED or equivalent",
                                  dmdeduc3 == 15 ~ "More than high school",
                                  dmdeduc3 %in% c(66,77,".") ~ as.character(NA))
  ) %>%
  mutate(dmdeduc2_adj = case_when(dmdeduc2 == 1 ~ "Less than 9th grade",
                                  dmdeduc2 == 2 ~ "9-11th grade (Includes 12th grade with no diploma)",
                                  dmdeduc2 == 3 ~ "High school graduate/GED or equivalent",
                                  dmdeduc2 == 4 ~ "Some college or AA degree",
                                  dmdeduc2 == 5 ~ "College graduate or above",
                                  dmdeduc2 %in% c(7,8,".") ~ as.character(NA))
  ) %>%
  # Combining the education covariate for both age groups
  mutate(educ = case_when(is.na(dmdeduc3_adj) ~ dmdeduc2_adj,
                          is.na(dmdeduc2_adj) ~ dmdeduc3_adj)) %>%
  # Recoding the income codes to interpretable values
  mutate(income = case_when(indhhin2 == 1 ~ "$0 to $4,999",
                            indhhin2 == 2 ~ "$5,000 to $9,999",
                            indhhin2 == 3 ~ "$10,000 to $14,999",
                            indhhin2 == 4 ~ "$15,000 to $19,999",
                            indhhin2 == 5 ~ "$20,000 to $24,999",
                            indhhin2 == 6 ~ "$25,000 to $34,999",
                            indhhin2 == 7 ~ "$35,000 to $44,999",
                            indhhin2 == 8 ~ "$45,000 to $54,999",
                            indhhin2 == 9 ~ "$55,000 to $64,999",
                            indhhin2 == 10 ~ "$65,000 to $74,999",
                            indhhin2 == 12 ~ "$20,000 and Over",
                            indhhin2 == 13 ~ "Under $20,000",
                            indhhin2 == 14 ~ "$75,000 to $99,999",
                            indhhin2 == 15 ~ "$100,000 and Over",
                            indhhin2 %in% c(77,99,".") ~ as.character(NA))
  ) %>%
  # Recoding the health insurance codes to interpretable values
  mutate(health_insurance = case_when(hiq011 == 1 ~ "Yes",
                                      hiq011 == 2 ~ "No",
                                      hiq011 %in% c(7, 9, ".") ~ as.character(NA))
  ) %>%
  # Recoding the food security codes to interpretable values
  mutate(food_security = case_when(fsd032a == 1 ~ "Often worried",
                                   fsd032a == 2 ~ "Sometimes worried",
                                   fsd032a == 3 ~ "Never worried",
                                   fsd032a %in% c(7, 9, ".") ~ as.character(NA))
  ) %>%
  # Recoding the increased diabetes risk covariate
  mutate(increased_diabetes_risk = case_when(diq170 == 1 ~ "Yes",
                                             diq170 == 2 ~ "No",
                                             diq170 %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Recoding the blood relatives had diabetes covariate
  mutate(relatives_had_diabetes = case_when(mcq300c == 1 ~ "Yes",
                                            mcq300c == 2 ~ "No",
                                            mcq300c %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Recoding the felt depressed covariate
  mutate(felt_depressed = case_when(dpq020 == 0 ~ "Not at all",
                                    dpq020 == 1 ~ "Several days",
                                    dpq020 == 2 ~ "More than half the days",
                                    dpq020 == 3 ~ "Nearly every day",
                                    dpq020 %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Recoding the feels like they're at risk for diabetes or prediabetes covariate
  mutate(feels_at_risk_diabetes = case_when(diq172 == 1 ~ "Yes",
                                            diq172 == 2 ~ "No",
                                            diq172 %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Adjusting weights for 8 years of data
  mutate(wtmec8yr = wtmec2yr/4) %>%
  # Making diabetes labels as described in the paper and replication code
  mutate(lbxgh = as.numeric(as.character((lbxgh))),
         diq010 = as.numeric(as.character((diq010))),
         a1c = cut(lbxgh,breaks=c(0,5.7,6.5,1000),right=FALSE),
         diabetes_diagnosis = case_when(diq010 %in% 1 ~ 1, 
                                        diq010 %in% c(2,3,9) ~ 0,
                                        diq010 %in% 7 ~ as.numeric(NA)),
         diabetes = diabetes_diagnosis,
         diabetes = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), 1, diabetes),
         diabetes = as.integer(diabetes),
         diabetes = if_else(diabetes == 1, TRUE, FALSE)
  ) %>%
  # Normalizing weights for numerical stability in regressions
  mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))

saveRDS(data, file = paste(write_path, "data.rds", sep = ""))
