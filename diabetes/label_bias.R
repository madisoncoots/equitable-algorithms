library(tidyverse)
library(RColorBrewer)

save_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/diabetes/figures/"

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

full_model_formula <- diabetes ~ race + ridageyr + bmxbmi + gender +
  whd140 + bmxwt + bmxht + bmxwaist + relatives_had_diabetes + felt_depressed +
  income + health_insurance  + food_security

full_model <- glm(full_model_formula,
                  data = data,
                  family = "binomial",
                  weights = round(wtmec8yr/1000))

doctor_diagnosis_model_pred <- predict(doctor_diagnosis_model, newdata = data, type = "response")
full_model_pred <- predict(full_model, newdata = data, type = "response")

# Label bias calibration plot data
label_bias_plot_data <- data %>%
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

# This is the color palette used for the plot
color_palette <- brewer.pal(n=4,"Set2")
# This maps colors to groups
group_color_map <- c("Asian American" = color_palette[2],
                     "Black American" = color_palette[3],
                     "Hispanic American" = color_palette[4],
                     "White American" = color_palette[1])

# This provides the color map in the right order for the legend
ordered_group_color_map <- group_color_map[line_order]

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
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.25, 0.82)) +
  scale_color_manual(values=ordered_group_color_map)

ggsave(paste(save_path, "label_bias_calibration_plot.pdf", sep = ""),
       width = 4,
       height = 4)


