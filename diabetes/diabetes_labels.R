library(tidyverse)

data <- readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/data.rds") %>%
  rename(doctor_diagnosis = diabetes_diagnosis) %>%
  mutate(blood_test_diagnosis = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), TRUE, FALSE),
         doctor_diagnosis = doctor_diagnosis == 1,
         diabetes = blood_test_diagnosis | doctor_diagnosis)

data %>% 
  count(race, blood_test_diagnosis, doctor_diagnosis) %>%
  mutate(labels_agree = blood_test_diagnosis == doctor_diagnosis) %>%
  group_by(race) %>%
  mutate(total_in_group = sum(n),
         pct = n / total_in_group)  %>%
  filter(labels_agree) %>%
  summarize(total_pct_labels_agree = sum(pct))
  group_by(race, labels_agree) %>%
  summarize(labels_agree_pct = sum(n/total_in_group))

