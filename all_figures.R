# Author: Madison Coots
# Date: February 17, 2023
# ======================
# Code for Figures
# 1a, 1b, 2, 3b, 4b, and 5

library(tidyverse)
library(ggpattern)
library(RColorBrewer)
library(ggtext)
library(readr)
library(janitor)
library(lubridate)

source("/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/colors.R")
theme_set(theme_bw(base_size = 15))

data <- readRDS("/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/data.rds")
cleaned_survey_results <- readRDS("/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/survey.rds")
save_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/figures/"

# ====================== Figure 1a: Calibration facet plot =====================

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

# This provides the color map in the right order for the legend
ordered_group_color_map <- group_color_map[line_order]
ordered_group_names <- group_names[line_order]

facet_plot_data <- bind_rows(
  race_blind_calibration_plot_data %>% mutate(plot = "blind"),
  race_aware_calibration_plot_data %>% mutate(plot = "aware")
) %>%
  mutate(plot = fct_recode(plot, `Model without race/ethnicity` = "blind", `Model with race/ethnicity` = "aware"))

facet_plot_data %>%
  ggplot(aes(x=bin_avg_risk_score, y=diabetes_prev, color=race)) +
  geom_vline(xintercept=0.015) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(fct_rev(plot))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Risk score") +
  ylab("Diabetes rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.12, 0.02)) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, 0.12)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.83)) +
  scale_color_manual(values=ordered_group_color_map,
                     breaks = ordered_group_names)

ggsave(paste(save_path, "facet_calibration_plot.pdf", sep = ""),
       width = 5.25,
       height = 5)

# For paper stats
hypothetical_asian_patient <- data.frame(
  race = "Asian",
  ridageyr = 30, 
  bmxbmi = 21.5
)

predict(race_blind_model, newdata = hypothetical_asian_patient, type = "response")
predict(race_aware_model, newdata = hypothetical_asian_patient, type = "response")

data %>%
  mutate(risk_score = race_blind_model_pred,
         est_diabetes_prob = full_model_pred) %>%
  filter(!is.na(risk_score),
         !is.na(est_diabetes_prob)) %>%
  select(race, risk_score, est_diabetes_prob) %>%
  mutate(risk_score_bin = floor((risk_score  + 0.0005) * 1000) / 1000) %>% # round to the nearest 0.001
  group_by(race, risk_score_bin) %>%
  summarize(bin_avg_risk_score = mean(risk_score),
            diabetes_prev = mean(est_diabetes_prob)) %>%
  filter(race == "Asian", risk_score_bin %in% c(0.011, 0.012))


hypothetical_white_patient <- data.frame(
  race = "White",
  ridageyr = 37, 
  bmxbmi = 21.5
)

predict(race_blind_model, newdata = hypothetical_white_patient, type = "response")
predict(race_aware_model, newdata = hypothetical_white_patient, type = "response")

data %>%
  mutate(risk_score = race_blind_model_pred,
         est_diabetes_prob = full_model_pred) %>%
  filter(!is.na(risk_score),
         !is.na(est_diabetes_prob)) %>%
  select(race, risk_score, est_diabetes_prob) %>%
  mutate(risk_score_bin = floor((risk_score  + 0.0005) * 1000) / 1000) %>% # round to the nearest 0.001
  group_by(race, risk_score_bin) %>%
  summarize(bin_avg_risk_score = mean(risk_score),
            diabetes_prev = mean(est_diabetes_prob)) %>%
  filter(race == "White", risk_score_bin == 0.019)

# ======================= Figure 1b: Histogram facet plot ======================

combined_risk_scores <- 
  data.frame(race = data$race,
             race_blind_model_pred,
             race_aware_model_pred) %>%
  filter(!is.na(race)) %>%
  mutate(blind_screened = race_blind_model_pred > 0.015,
         aware_screened = race_aware_model_pred > 0.015,
         wrong = blind_screened != aware_screened,
         wrong = if_else(wrong, "wrong", "ok")) %>%
  mutate(named_blind_screened = if_else(blind_screened, "Screened", "Not screened"))

text_annotation <- data.frame(
  label = c("Expected net<br>**benefit** from screening", "Expected net<br>**cost** from screening"),
  x = c(0.016, 0.014),
  y = 0.07,
  race = c("Asian", "Asian"),
  wrong = c("ok", "ok"),
  named_blind_screened = c("Screened", "Screened"),
  hjust = c(0, 1)
)

# For paper stats
combined_risk_scores %>%
  filter(race == "White") %>%
  summarize(sum(wrong == "wrong") / n())

combined_risk_scores %>%
  filter(race == "Asian") %>%
  summarize(sum(wrong == "wrong") / n())

histogram <-
  combined_risk_scores %>%
  filter(race == "Asian" | race == "White") %>%
  ggplot(aes(x = race_aware_model_pred, pattern = wrong, fill = named_blind_screened)) +
  facet_wrap(vars(fct_rev(race)), ncol = 1) + 
  annotate("rect", xmin = 0.015, xmax = 1, ymin = -1, ymax = 1,
           alpha = .075) + 
  geom_histogram(binwidth = 0.0025, boundary = 0, aes(y = after_stat(count/tapply(count, PANEL, sum)[PANEL]))) +
  scale_pattern_manual(values = c(wrong = "circle", ok = "none"), guide = "none") +
  geom_histogram_pattern(binwidth = 0.0025, boundary = 0, aes(y = after_stat(count/tapply(count, PANEL, sum)[PANEL])),
                         pattern_color = NA,
                         pattern_fill = "black",
                         pattern_angle = 45,
                         # pattern_density = 0.5,
                         pattern_spacing = 0.07, # tweak this for dot size
                         # pattern_key_scale_factor = 0.5,
                         pattern_size = 2) +
  # Needed to use geom_richtext to enable 1) bolding part of the label, and adding the label to just one facet
  geom_richtext(data = text_annotation, mapping = aes(x = x, y = y, label = label, hjust = hjust),
                size = 3,
                fill = alpha(c("white"), 0.8),
                # alpha = 0.8,
                label.size = NA) +
  xlab("Risk score") +
  ylab("Population proportion") + 
  geom_vline(xintercept=0.015) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.1, 0.01)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.1, 0.01)) +
  coord_cartesian(xlim = c(0, 0.051), ylim = c(0, 0.075)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.83, 0.93),
        legend.background = element_blank()) +
  scale_fill_manual(values = c("Screened" = "tomato", "Not screened" = "gray"),
                    breaks = c("Screened", "Not screened"),
                    labels = c("Screened", "Not screened"),
                    guide = guide_legend(override.aes = list(pattern = "none")))

ggsave(paste(save_path, "histogram.pdf", sep = ""),
       width = 5.25,
       height = 5)


# =================== Figure 2: Risk distribution facet plot ===================

formula <- diabetes ~ race + ridageyr + bmxbmi

model <- glm(formula,
             data = data,
             family = "binomial",
             weights = round(wtmec8yr/1000))

model_pred <- predict(model, newdata = data, type = "response")

data_with_pred <- data %>%
  mutate(risk_score = model_pred) %>%
  filter(!is.na(risk_score))

marginal_plot_data <- data_with_pred %>% 
  mutate(density = "marginal")

conditional_plot_data <- data_with_pred %>%
  filter(diabetes) %>%
  mutate(density = "conditional")

# The quantile that corresponds to 1.5% threshold on the entire population
quantile_for_thresh_whole_pop <- data_with_pred %>%
  mutate(above_thresh = risk_score >= 0.015) %>%
  summarize(sum(above_thresh) / n()) %>%
  pull()

# The proportion of each group with risk scores above 1.5% (for paper stats)
data_with_pred %>%
  mutate(above_thresh = risk_score >= 0.015) %>%
  group_by(race) %>%
  summarize(sum(above_thresh) / n())

race_group_thresh <- data_with_pred %>%
  group_by(race) %>%
  arrange(desc(risk_score)) %>%
  mutate(cum_proportion = row_number() / n(),
         above_general_pop_quantile = cum_proportion <= quantile_for_thresh_whole_pop) %>%
  summarize(race_group_thresh = min(risk_score[above_general_pop_quantile])) %>%
  mutate(density = "All patients")

fnr_whole_pop <- data_with_pred %>%
  filter(diabetes) %>%
  mutate(screening_decision = risk_score >= 0.015,
         false_negative = !screening_decision) %>%
  summarize(sum(false_negative) / n()) %>%
  pull()

# FNR by race with 1.5% threshold (for paper stats)
data_with_pred %>%
  filter(diabetes) %>%
  mutate(screening_decision = risk_score >= 0.015,
         false_negative = !screening_decision) %>%
  group_by(race) %>%
  summarize(sum(false_negative) / n())

race_group_thresh_equalized_fnr <- data_with_pred %>%
  filter(diabetes) %>%
  group_by(race) %>%
  arrange(risk_score) %>%
  mutate(fnr = row_number() / n(),
         below_fnr_whole_pop = fnr <= fnr_whole_pop) %>%
  summarize(race_group_thresh = max(risk_score[below_fnr_whole_pop])) %>%
  mutate(density = "Patients with diabetes")

plot_data <- bind_rows(marginal_plot_data, conditional_plot_data) %>%
  mutate(density = fct_recode(density, `All patients` = "marginal", `Patients with diabetes` = "conditional"))

text_annotation <- data.frame(
  label = c("Expected <br>net **benefit**<br>from screening", "Expected <br>net **cost**<br>from screening"),
  x = c(0.016, 0.014),
  race = c("Asian", "Asian"),
  y = 12.5,
  density = c("All patients", "All patients"),
  wrong = c("ok", "ok"),
  named_blind_screened = c("Screened", "Screened"),
  hjust = c(0, 1)
)

plot_data %>%
  ggplot(aes(x = risk_score, color = race)) +
  annotate("rect", xmin = 0.015, xmax = 1, ymin = -5, ymax = 20,
           alpha = .075) +
  geom_density(aes(weight = round(wtmec8yr/1000)), bw = 0.01) +
  geom_vline(xintercept = 0.015, show.legend = FALSE) +
  facet_wrap(vars(fct_rev(density))) +
  geom_vline(data = race_group_thresh_equalized_fnr, aes(xintercept = race_group_thresh, color = race),
             linetype = "dashed", show.legend = FALSE) +
  geom_vline(data = race_group_thresh, aes(xintercept = race_group_thresh, color = race),
             linetype = "dashed", show.legend = FALSE) +
  xlab("Probability of having diabetes") +
  ylab("Density") +
  # Needed to use geom_richtext to enable 1) bolding part of the label, and adding the label to just one facet
  geom_richtext(data = text_annotation, mapping = aes(x = x, y = y, label = label, hjust = hjust),
                size = 3,
                fill = alpha(c("white"), 0.8),
                label.size = NA,
                color = "black") +
  scale_x_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y=element_blank(),
        panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(10, 10, 10, 10, "pt")) +
  scale_color_manual(values=group_color_map,
                     breaks = c("White", "Hispanic", "Asian", "Black")) +
  coord_cartesian(xlim = c(0, .05), ylim = c(0, 13.5))


ggsave(paste(save_path, "risk_distribution.pdf", sep = ""),
       width = 8,
       height = 5.5)

# ====================== Figure 3b: Pareto frontier plot =======================

survey_points <- data.frame(
  new_appearances = c(680, 730, 710, 620, 520),
  pct_rides_black_clients = c(0.1, 0.3, 0.5, 0.7, 0.9))

spline_int <- as.data.frame(spline(survey_points$pct_rides_black_clients, survey_points$new_appearances))
true_max <- spline_int %>%
  filter(y == max(y))

equalized_decision_rate <- data.frame(x = 0.5, y = 710)

survey_points %>%
  ggplot(aes(x = pct_rides_black_clients, y = new_appearances)) +
  geom_line(data = spline_int, aes(x=x, y=y), color = "dimgray") +
  geom_segment(aes(x = 0.1, y = 0, xend = 0.1, yend = 680), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(x = 0.3, y = 0, xend = 0.3, yend = 730), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(x = 0.5, y = 0, xend = 0.5, yend = 710), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(x = 0.7, y = 0, xend = 0.7, yend = 620), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(x = 0.9, y = 0, xend = 0.9, yend = 520), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_point(data = true_max, aes(x = x, y = y)) +
  geom_point(data = equalized_decision_rate, aes(x = x, y = y)) +
  annotate("text", x = 0.3 + 0.2, y = 730 + 10, label = "Maximum appearances") + 
  annotate("text", x = 0.5 + 0.18, y = 700 + 15, label = "Equalized decision rate") + 
  ylab("Number of additional appearances") +
  xlab("Percentage of rides offered to Black residents") +
  scale_x_continuous(labels = scales::percent,
                     breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  coord_cartesian(xlim = c(0.1, 0.9),
                  ylim = c(500, 750)) +
  theme(legend.position = c(.84, .7),
        legend.title = element_blank())

ggsave(paste(save_path, "pareto_frontier.pdf", sep = ""),
       width = 5.25,
       height = 5) 

# ==================== Figure 4b: Survey results histogram =====================

dist_by_survey_version_plot_data <- cleaned_survey_results %>%
  count(preference_pct_black, survey_version_long) %>%
  group_by(survey_version_long) %>%
  mutate(proportion = n / sum(n))

mean_pref_by_survey <- cleaned_survey_results %>%
  group_by(survey_version_long) %>%
  summarize(mean_pref = mean(preference_pct_black))

mean_pref_black <- mean_pref_by_survey %>%
  filter(survey_version_long == "Black clients more expensive")

figure_data <- cleaned_survey_results %>%
  filter(survey_version == "black") %>%
  count(preference_pct_black, survey_version) %>%
  group_by(survey_version) %>%
  mutate(proportion = n / sum(n)) %>%
  bind_rows( # Manually adding rows to show that 0 people selected the 10% option in the figure
    data.frame(preference_pct_black = 10,
               survey_version = "black",
               n = 0,
               proportion = 0)
  ) %>%
  mutate(plot_label = "Respondent ride allocation preferences")

figure_data %>%
  ggplot(aes(x = preference_pct_black, y = proportion)) +
  geom_bar(stat='identity', width = 10, show.legend = FALSE, fill = "dimgray") +
  geom_vline(data=mean_pref_black, aes(xintercept=mean_pref, group = survey_version_long), linetype="dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     name = "") +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     breaks = c(10, 30, 50, 70, 90),
                     minor_breaks = c(),
                     name = "Percentage of rides offered to Black clients")

ggsave(paste(save_path, "survey_preferences.pdf", sep = ""),
       width = 5,
       height = 4)


# ==================== Figure 5: Label bias calibration plot ===================

label_bias_data <- data %>%
  rename(doctor_diagnosis = diabetes_diagnosis) %>%
  mutate(blood_test_diagnosis = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), TRUE, FALSE),
         doctor_diagnosis = doctor_diagnosis == 1,
         ground_truth_diabetes = blood_test_diagnosis | doctor_diagnosis)

# Models trained using different labels
doctor_diagnosis_model <- glm(doctor_diagnosis ~ ridageyr + bmxbmi + race,
                              data = label_bias_data,
                              family = "binomial",
                              weights = round(wtmec8yr/1000))

full_model_formula <- diabetes ~ race + ridageyr + bmxbmi + gender +
  whd140 + bmxwt + bmxht + bmxwaist + relatives_had_diabetes + felt_depressed +
  income + health_insurance  + food_security

full_model <- glm(full_model_formula,
                  data = label_bias_data,
                  family = "binomial",
                  weights = round(wtmec8yr/1000))

doctor_diagnosis_model_pred <- predict(doctor_diagnosis_model, newdata = label_bias_data, type = "response")
full_model_pred <- predict(full_model, newdata = label_bias_data, type = "response")

# Label bias calibration plot data
label_bias_plot_data <- label_bias_data %>%
  mutate(risk_score = doctor_diagnosis_model_pred,
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
line_order <- label_bias_plot_data %>%
  # Make sure x-range lines up with what is visualized in plot
  filter(risk_score_bin < risk_score_upper_bound) %>%
  group_by(race) %>%
  summarize(mean_prev = mean(diabetes_prev)) %>%
  arrange(as.character(race)) %>%
  mutate(alph_index = row_number()) %>%
  arrange(desc(mean_prev)) %>%
  pull(alph_index)

# This provides the color map in the right order for the legend
ordered_group_names <- group_names[line_order]

label_bias_plot_data %>%
  ggplot(aes(x=bin_avg_risk_score, y=diabetes_prev, color=race)) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  xlab("Risk score based on biased label") +
  ylab("Diabetes rate") + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0,risk_score_upper_bound), ylim = c(0, 0.08)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.17, 0.83)) +
  scale_color_manual(values=group_color_map,
                     breaks=ordered_group_names)

ggsave(paste(save_path, "label_bias_calibration_plot.pdf", sep = ""),
       width = 5.25,
       height = 5)


