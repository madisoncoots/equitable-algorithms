library(tidyverse)
library(RColorBrewer)


data <- readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/data.rds")
save_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/diabetes/figures/"

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
  mutate(risk_score_bin = floor(risk_score * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
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

race_blind_calibration_plot_data %>%
  ggplot(aes(x=risk_score_bin, y=diabetes_prev, color=race)) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept=0.015) +
  xlab("Race-blind risk score") +
  ylab("Diabetes rate") + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, 0.12)) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values=ordered_group_color_map)

ggsave(paste(save_path, "race_blind_calibration_plot.pdf", sep = ""),
       width = 5.5,
       height = 4)


