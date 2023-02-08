library(tidyverse)
library(RColorBrewer)
source("/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/diabetes/colors.R")

data <- readRDS("/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/data.rds")
save_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/diabetes/figures/"

formula <- diabetes ~ race + ridageyr + bmxbmi

model <- glm(formula,
             data = data,
             family = "binomial",
             weights = round(wtmec8yr/1000))

model_pred <- predict(model, newdata = data, type = "response")

data_with_pred <- data %>%
  mutate(risk_score = model_pred) %>%
  filter(!is.na(risk_score))

data_with_pred %>%
  group_by(race) %>%
  filter(!is.na(race)) %>%
  summarize(med_risk_score = median(risk_score),
            pctile = quantile(risk_score, 0.25))

line_annotations <- data %>%
  group_by(race) %>%
  filter(!is.na(race)) %>%
  summarize(incidence = mean(diabetes)) %>%
  mutate(density = "marginal") %>%
  mutate(density = fct_recode(density, `All patients` = "marginal"))

make_density_polygon_data <- function(data, facet_name) {
  white_risk_score <- data %>%
    filter(race == "White") %>%
    select(risk_score, wtmec8yr) %>%
    mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))
  
  black_risk_score <- data %>%
    filter(race == "Black") %>%
    select(risk_score, wtmec8yr) %>%
    mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))
  
  hispanic_risk_score <- data %>%
    filter(race == "Hispanic") %>%
    select(risk_score, wtmec8yr) %>%
    mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))
  
  asian_risk_score <- data %>%
    filter(race == "Asian") %>%
    select(risk_score, wtmec8yr) %>%
    mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))
  
  white_density <- density(white_risk_score$risk_score,
                           weights = white_risk_score$normalized_weights,
                           n = 1024)
  
  black_density <- density(black_risk_score$risk_score,
                           weights = black_risk_score$normalized_weights,
                           n = 1024)
  
  hispanic_density <- density(hispanic_risk_score$risk_score,
                              weights = hispanic_risk_score$normalized_weights,
                              n = 1024)
  
  asian_density <- density(asian_risk_score$risk_score,
                           weights = asian_risk_score$normalized_weights,
                           n = 1024)
  
  densities <- bind_rows(
    data.frame(
      x = white_density$x,
      y = white_density$y,
      race = "White"
    ),
    data.frame(
      x = black_density$x,
      y = black_density$y,
      race = "Black"
    ),
    data.frame(
      x = hispanic_density$x,
      y = hispanic_density$y,
      race = "Hispanic"
    ),
    data.frame(
      x = asian_density$x,
      y = asian_density$y,
      race = "Asian"
    )
  ) %>%
    mutate(y = if_else(x > 0.015, y, 0)) %>%
    # These addl rows are necessary to ensure a precise transition
    # from the x-axis to the top of the distribution (otherwise 
    # you get a diagonal line from the last y = 0 point on the x-axis
    # to the next non-zero point where x > 0.015) 
    bind_rows(data.frame(x = 0.015, y = 0, race = "White"),
              data.frame(x = 0.015, y = 0, race = "Black"),
              data.frame(x = 0.015, y = 0, race = "Hispanic"),
              data.frame(x = 0.015, y = 0, race = "Asian")) %>%
    arrange(x, y) %>%
    mutate(density = facet_name)
  return(densities)
}



marginal_plot_data <- data_with_pred %>% 
  mutate(density = "marginal")
conditional_plot_data <- data_with_pred %>%
  filter(diabetes) %>%
  mutate(density = "conditional")

marginal_polygon_data <- make_density_polygon_data(data_with_pred, "marginal")
conditional_polygon_data <- make_density_polygon_data(data_with_pred %>%
                                                        filter(diabetes), "conditional")

polygon_data <- bind_rows(marginal_polygon_data, conditional_polygon_data) %>%
  mutate(density = fct_recode(density, `All patients` = "marginal", `Patients with diabetes` = "conditional"))


# This chunk determines the vertical order of the lines in the plot
# so that we can have the order of the lines in the legend reflect
# the order of the lines in the plot so that it is easier to read
# risk_score_upper_bound <- 0.05
# line_order <- marginal_polygon_data %>%
#   # Make sure x-range lines up with what is visualized in plot
#   filter(x < risk_score_upper_bound) %>%
#   group_by(race) %>%
#   summarize(mean_val = mean(y)) %>%
#   arrange(as.character(race)) %>%
#   mutate(alph_index = row_number()) %>%
#   arrange(desc(mean_val)) %>%
#   pull(alph_index)

# This provides the color map in the right order for the legend
# ordered_group_color_map <- group_color_map[line_order]
# ordered_group_names <- group_names[line_order]

# The quantile that corresponds to 1.5% threshold on the entire population
quantile_for_thresh_whole_pop <- data_with_pred %>%
  mutate(above_thresh = risk_score >= 0.015) %>%
  summarize(sum(above_thresh) / n()) %>%
  pull()

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

race_group_thresh_equalized_fnr <- data_with_pred %>%
  filter(diabetes) %>%
  group_by(race) %>%
  arrange(risk_score) %>%
  mutate(fnr = row_number() / n(),
         below_fnr_whole_pop = fnr <= fnr_whole_pop) %>%
  summarize(race_group_thresh = max(risk_score[below_fnr_whole_pop])) %>%
  mutate(density = "Patients with diabetes")
  
  

# ------------------------------------------------------------------------------


# marginal_plot_data %>%
#   ggplot(aes(x=risk_score)) +
#   annotate("rect", xmin = 0.015, xmax = 1, ymin = -1, ymax = 15,
#            alpha = .075) +
#   geom_density(aes(weight=round(wtmec8yr/1000), color = race)) +
#   geom_vline(xintercept = 0.015) +
#   geom_vline(data = race_group_thresh, aes(xintercept = race_group_thresh, color = race),
#              linetype = "dashed", show.legend = FALSE) +
#   coord_cartesian(xlim = c(0, 0.051), expand = FALSE, y = c(0, 13)) +
#   annotate("text", x = 0.016, y = 12.5, label = "Expected to\nbenefit from screening", hjust = 0, size = 3, vjust = 1) + 
#   annotate("text", x = 0.014, y = 12.5, label = "Not expected to\nbenefit from screening", hjust = 1, size = 3, vjust = 1) +
#   xlab("Probability of having diabetes") +
#   ylab("Density") +
#   scale_x_continuous(labels = scales::percent,
#                      expand = c(0,0)) +
#   theme_bw() +
#   theme(legend.position = "none",
#         legend.title = element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.text.y=element_blank()) +
#   scale_color_manual(values=group_color_map) +
#   scale_fill_manual(values=group_color_map) +
#   guides(colour = guide_legend(override.aes = list(alpha = 0.2)))
# 
# ggsave(paste(save_path, "marginal_risk_distribution.pdf", sep = ""),
#        width = 5.5,
#        height = 4)

# ------------------------------------------------------------------------------

# conditional_plot_data %>%
#   ggplot(aes(x=risk_score, color=race)) +
#   annotate("rect", xmin = 0.015, xmax = 1, ymin = -1, ymax = 15,
#            alpha = .075) +
#   geom_density(aes(weight=round(wtmec8yr/1000))) +
#   geom_vline(xintercept = 0.015) +
#   geom_vline(data = race_group_thresh_equalized_fnr, aes(xintercept = race_group_thresh, color = race),
#              linetype = "dashed", show.legend = FALSE) +
#   xlab("Probability of having diabetes") +
#   ylab("Density") +
#   annotate("text", x = 0.016, y = 2.9, label = "Expected to\nbenefit from screening", hjust = 0, size = 3, vjust = 1) + 
#   annotate("text", x = 0.014, y = 2.9, label = "Not expected to\nbenefit from screening", hjust = 1, size = 3, vjust = 1) +
#   scale_x_continuous(labels = scales::percent,
#                      expand = c(0,0)) +
#   theme_bw() +
#   theme(legend.position = c(.8, .8),
#         legend.title = element_blank(),
#         legend.background = element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.text.y=element_blank()) +
#     scale_color_manual(values=group_color_map) +
#   coord_cartesian(xlim = c(0, 0.051), expand = FALSE, y = c(0, 3))
# 
# ggsave(paste(save_path, "conditional_risk_distribution.pdf", sep = ""),
#        width = 5.5,
#        height = 4)

plot_data <- bind_rows(marginal_plot_data, conditional_plot_data) %>%
  mutate(density = fct_recode(density, `All patients` = "marginal", `Patients with diabetes` = "conditional"))

plot_data %>%
  ggplot(aes(x = risk_score, color = race)) +
  annotate("rect", xmin = 0.015, xmax = 1, ymin = -5, ymax = 20,
           alpha = .075) +
  geom_density(aes(weight = round(wtmec8yr/1000))) +
  geom_vline(xintercept = 0.015, show.legend = FALSE) +
  facet_wrap(vars(fct_rev(density))) +
  geom_vline(data = race_group_thresh_equalized_fnr, aes(xintercept = race_group_thresh, color = race),
             linetype = "dashed", show.legend = FALSE) +
  geom_vline(data = race_group_thresh, aes(xintercept = race_group_thresh, color = race),
             linetype = "dashed", show.legend = FALSE) +
  xlab("Probability of having diabetes") +
  ylab("Density") +
  annotate("label", x = 0.016, y = 12.2, label = "Expected\nto benefit\nfrom screening", hjust = 0, size = 3, vjust = 1, label.size = NA, fill = alpha(c("white"), 0.8)) + 
  annotate("label", x = 0.014, y = 12.2, label = "Not expected\nto benefit\nfrom screening", hjust = 1, size = 3, vjust = 1, label.size = NA, fill = alpha(c("white"), 0.8)) +
  scale_x_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.text.y=element_blank(),
        legend.text = element_text(size = 9), panel.spacing = unit(1.5, "lines"),
        plot.margin = margin(10, 10, 10, 10, "pt")) +
  scale_color_manual(values=group_color_map,
                     breaks = c("White", "Hispanic", "Asian", "Black")) +
  coord_cartesian(xlim = c(0, .05), ylim = c(0, 12))


ggsave(paste(save_path, "risk_distribution.pdf", sep = ""),
       width = 8,
       height = 5.5)

