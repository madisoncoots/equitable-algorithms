library(tidyverse)
library(RColorBrewer)


data <- readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/data.rds")
save_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/diabetes/figures/"

race_blind_model <- glm(diabetes ~ ridageyr + bmxbmi,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000))

race_aware_model <- glm(diabetes ~ ridageyr + bmxbmi + race,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000))

full_model <- diabetes ~ race + ridageyr + bmxbmi + gender +
  whd140 + bmxwt + bmxht + bmxwaist + relatives_had_diabetes + felt_depressed +
  income + health_insurance  + food_security

race_aware_model_plus <- glm(full_model,
                             data = data,
                             family = "binomial",
                             weights = round(wtmec8yr/1000))

race_blind_model_pred <- predict(race_blind_model, newdata = data, type = "response")
race_aware_model_pred <- predict(race_aware_model, newdata = data, type = "response")
full_model_pred <- predict(race_aware_model_plus, newdata = data, type = "response")

# Counting the number of data points
data %>%
  mutate(risk_score = race_aware_model_pred,
         est_diabetes_prob = full_model_pred) %>%
  filter(!is.na(risk_score),
         !is.na(est_diabetes_prob)) %>%
  nrow()

race_blind_calibration_plot_data <- data %>%
  mutate(risk_score = race_blind_model_pred,
         est_diabetes_prob = full_model_pred) %>%
  filter(!is.na(risk_score),
         !is.na(est_diabetes_prob)) %>%
  select(race, risk_score, est_diabetes_prob) %>%
  mutate(risk_score_bin = floor((risk_score  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
  group_by(race, risk_score_bin) %>%
  summarize(bin_avg_risk_score = mean(risk_score),
            diabetes_prev = mean(est_diabetes_prob))

race_aware_calibration_plot_data <- data %>%
  mutate(risk_score = race_aware_model_pred,
         est_diabetes_prob = full_model_pred) %>%
  filter(!is.na(risk_score),
         !is.na(est_diabetes_prob)) %>%
  select(race, risk_score, est_diabetes_prob) %>%
  mutate(risk_score_bin = floor((risk_score  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
  group_by(race, risk_score_bin) %>%
  summarize(bin_avg_risk_score = mean(risk_score),
            diabetes_prev = mean(est_diabetes_prob))

# This chunk determines the vertical order of the lines in the plot
# so that we can have the order of the lines in the legend reflect
# the order of the lines in the plot so that it is easier to read
risk_score_upper_bound <- 0.05
line_order <- race_blind_calibration_plot_data %>%
  # Make sure x-range lines up with what is visualized in plot
  filter(risk_score_bin < risk_score_upper_bound) %>%
  group_by(race) %>%
  summarize(mean_prev = mean(diabetes_prev)) %>%
  arrange(as.character(race)) %>%
  mutate(alph_index = row_number()) %>%
  arrange(desc(mean_prev)) %>%
  pull(alph_index)

# This is the color palette used for the plot
color_palette <- brewer.pal(n=4,"Set2")
# This maps colors to groups
group_color_map <- c("Asian American" = color_palette[2],
                     "Black American" = color_palette[3],
                     "Hispanic American" = color_palette[4],
                     "White American" = color_palette[1])

# This provides the color map in the right order for the legend
ordered_group_color_map <- group_color_map[line_order]

# Race-aware plot by itself
race_aware_calibration_plot_data %>%
  ggplot(aes(x=bin_avg_risk_score, y=diabetes_prev, color=race)) +
  geom_vline(xintercept=0.015) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Risk score") +
  ylab("") + 
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.12, 0.02)) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, 0.12)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.7, 0.78)) +
  scale_color_manual(values=ordered_group_color_map)

ggsave(paste(save_path, "race_aware_calibration_plot.pdf", sep = ""),
       width = 3.5,
       height = 3.5)

# Race-blind plot by itself
race_blind_calibration_plot_data %>%
  ggplot(aes(x=bin_avg_risk_score, y=diabetes_prev, color=race)) +
  geom_vline(xintercept=0.015) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Risk score") +
  ylab("Diabetes rate") + 
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.12, 0.02)) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, 0.12)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "none") +
  scale_color_manual(values=ordered_group_color_map)

ggsave(paste(save_path, "race_blind_calibration_plot.pdf", sep = ""),
       width = 3.5,
       height = 3.5)

facet_plot_data <- bind_rows(
  race_blind_calibration_plot_data %>% mutate(plot = "blind"),
  race_aware_calibration_plot_data %>% mutate(plot = "aware")
  ) %>%
  mutate(plot = fct_recode(plot, `Race-blind model` = "blind", `Race-aware model` = "aware"))

facet_plot_data %>%
  ggplot(aes(x=bin_avg_risk_score, y=diabetes_prev, color=race)) +
  geom_line() + 
  geom_point() +
  facet_wrap(vars(fct_rev(plot))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  geom_vline(xintercept=0.015) +
  xlab("Risk score") +
  ylab("Diabetes rate") + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, 0.12)) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=ordered_group_color_map)

ggsave(paste(save_path, "facet_calibration_plot.pdf", sep = ""),
       width = 6,
       height = 3)
