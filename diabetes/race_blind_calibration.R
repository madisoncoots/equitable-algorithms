library(tidyverse)

data <- readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/data.rds")

# Race blind model calibration plot (reproduction of Figure 3a)
race_blind_model <- glm(diabetes ~ ridageyr + bmxbmi,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000))

race_aware_model <- glm(diabetes ~ ridageyr + bmxbmi + race,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000))

race_blind_model_pred <- predict(race_blind_model, newdata = data, type = "response")
race_aware_model_pred <- predict(race_aware_model, newdata = data, type = "response")

race_blind_calibration_plot_data <- data %>%
  mutate(risk_score = race_blind_model_pred,
         est_diabetes_prob = race_aware_model_pred) %>%
  filter(!is.na(risk_score),
         !is.na(est_diabetes_prob)) %>%
  select(race, risk_score, est_diabetes_prob) %>%
  mutate(risk_score_bin = round(risk_score, 2)) %>% 
  # mutate(risk_score_bin = floor(risk_score * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
  group_by(race, risk_score_bin) %>%
  summarize(diabetes_prev = mean(est_diabetes_prob)) %>%
  filter(race == "White American" | race == "Asian American")

race_blind_calibration_plot_data %>%
  ggplot(aes(x=risk_score_bin, y=diabetes_prev, color=race)) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_vline(xintercept=0.015) +
  xlab("Race-blind risk score") +
  ylab("Diabetes rate") + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0,0.05), ylim = c(0, 0.05)) +
  theme_bw() +
  theme(legend.position = c(.81, .87),
        legend.title = element_blank()) +
  scale_color_brewer(palette = "Set2")



