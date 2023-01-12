library(tidyverse)

data <- readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/data.rds") %>%
  rename(doctor_diagnosis = diabetes_diagnosis) %>%
  mutate(blood_test_diagnosis = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), TRUE, FALSE),
         doctor_diagnosis = doctor_diagnosis == 1,
         ground_truth_diabetes = blood_test_diagnosis | doctor_diagnosis)

# Computing Pr(doctor diagnosis | diabetes)
data %>% 
  filter(ground_truth_diabetes) %>%
  count(race, doctor_diagnosis) %>%
  group_by(race) %>%
  mutate(pct = n / sum(n)) %>%
  filter(doctor_diagnosis)

# Models trained using different labels
doctor_diagnosis_model <- glm(doctor_diagnosis ~ ridageyr + bmxbmi + race,
                             data = data,
                             family = "binomial",
                             weights = round(wtmec8yr/1000))

true_label_model <- glm(ground_truth_diabetes ~ ridageyr + bmxbmi + race,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000))

doctor_diagnosis_model_pred <- predict(doctor_diagnosis_model, newdata = data, type = "response")
true_label_model_pred <- predict(true_label_model, newdata = data, type = "response")

# Label bias calibration plot
label_bias_plot_data <- data %>%
  mutate(risk_score = doctor_diagnosis_model_pred,
         est_diabetes_prob = true_label_model_pred) %>%
  filter(!is.na(risk_score),
         !is.na(est_diabetes_prob)) %>%
  select(race, risk_score, est_diabetes_prob) %>%
  mutate(risk_score_bin = round(risk_score, 2)) %>% # round to the nearest 0.005
  # mutate(risk_score_bin = round(risk_score * 100 * 2) / 2 / 100) %>%
  group_by(race, risk_score_bin) %>%
  summarize(diabetes_prev = mean(est_diabetes_prob))

label_bias_plot_data %>%
  ggplot(aes(x=risk_score_bin, y=diabetes_prev, color=race)) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  xlab("Risk score") +
  ylab("Diabetes rate") + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0,0.05), ylim = c(0, 0.05)) +
  theme_bw() +
  theme(legend.position = c(.2, .82),
        legend.title = element_blank()) +
  scale_color_brewer(palette = "Set2")







