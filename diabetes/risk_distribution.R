library(tidyverse)
library(RColorBrewer)

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

line_annotations <- data %>%
  group_by(race) %>%
  filter(!is.na(race)) %>%
  summarize(incidence = mean(diabetes)) %>%
  mutate(density = "marginal") %>%
  mutate(density = fct_recode(density, `All patients` = "marginal"))

make_density_polygon_data <- function(data, facet_name) {
  white_risk_score <- data %>%
    filter(race == "White American") %>%
    select(risk_score, wtmec8yr) %>%
    mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))
  
  black_risk_score <- data %>%
    filter(race == "Black American") %>%
    select(risk_score, wtmec8yr) %>%
    mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))
  
  hispanic_risk_score <- data %>%
    filter(race == "Hispanic American") %>%
    select(risk_score, wtmec8yr) %>%
    mutate(normalized_weights = wtmec8yr / sum(wtmec8yr))
  
  asian_risk_score <- data %>%
    filter(race == "Asian American") %>%
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
      race = "White American"
    ),
    data.frame(
      x = black_density$x,
      y = black_density$y,
      race = "Black American"
    ),
    data.frame(
      x = hispanic_density$x,
      y = hispanic_density$y,
      race = "Hispanic American"
    ),
    data.frame(
      x = asian_density$x,
      y = asian_density$y,
      race = "Asian American"
    )
  ) %>%
    mutate(y = if_else(x > 0.015, y, 0)) %>%
    # These addl rows are necessary to ensure a precise transition
    # from the x-axis to the top of the distribution (otherwise 
    # you get a diagonal line from the last y = 0 point on the x-axis
    # to the next non-zero point where x > 0.015) 
    bind_rows(data.frame(x = 0.015, y = 0, race = "White American"),
              data.frame(x = 0.015, y = 0, race = "Black American"),
              data.frame(x = 0.015, y = 0, race = "Hispanic American"),
              data.frame(x = 0.015, y = 0, race = "Asian American")) %>%
    arrange(x, y) %>%
    mutate(density = facet_name)
  return(densities)
}


# This is the color palette used for the plot
color_palette <- brewer.pal(n=4,"Set2")
# This maps colors to groups
group_color_map <- c("Asian American" = color_palette[2],
                     "Black American" = color_palette[3],
                     "Hispanic American" = color_palette[4],
                     "White American" = color_palette[1])

marginal_plot_data <- data_with_pred %>% 
  mutate(density = "marginal")
conditional_plot_data <- data_with_pred %>%
  # filter(race == "Asian American" | race == "White American") %>%
  filter(!diabetes) %>%
  mutate(density = "conditional")

marginal_polygon_data <- make_density_polygon_data(data_with_pred, "marginal")
conditional_polygon_data <- make_density_polygon_data(data_with_pred %>%
                                                        filter(!diabetes), "conditional")

polygon_data <- bind_rows(marginal_polygon_data, conditional_polygon_data) %>%
  mutate(density = fct_recode(density, `All patients` = "marginal", `Patients without diabetes` = "conditional"))


marginal_plot_data %>%
  ggplot(aes(x=risk_score)) +
  geom_polygon(data = marginal_polygon_data, aes(x = x, y = y, fill = race),
               alpha = 0.2) +
  geom_density(aes(weight=round(wtmec8yr/1000), color = race)) +
  geom_vline(xintercept = 0.015) +
  geom_vline(data = line_annotations, aes(xintercept = incidence, color = race),
             linetype = "dashed", show.legend = FALSE) +
  coord_cartesian(xlim = c(0, 0.2), expand = FALSE, y = c(0, 13)) +
  xlab("Probability of having diabetes") +
  ylab("Density") +
  scale_x_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  scale_color_manual(values=group_color_map) +
  scale_fill_manual(values=group_color_map) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.2)))

ggsave(paste(save_path, "marginal_risk_distribution.pdf", sep = ""),
       width = 5.5,
       height = 4)

# ------------------------------------------------------------------------------

conditional_plot_data %>%
  ggplot(aes(x=risk_score, color=race)) +
  geom_polygon(data = conditional_polygon_data, aes(x = x, y = y, fill = race),
                              alpha = 0.2) +
  geom_density(aes(weight=round(wtmec8yr/1000))) +
  geom_vline(xintercept = 0.015) +
  xlab("Probability of having diabetes") +
  ylab("Density") +
  scale_x_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = c(.8, .8),
        legend.title = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
    scale_color_manual(values=group_color_map) +
    scale_fill_manual(values=group_color_map) +
  coord_cartesian(xlim = c(0, 0.2), expand = FALSE, y = c(0, 13)) +
  guides(colour = guide_legend(override.aes = list(alpha = 0.2)))

ggsave(paste(save_path, "conditional_risk_distribution.pdf", sep = ""),
       width = 5.5,
       height = 4)

# plot_data <- bind_rows(marginal_plot_data, conditional_plot_data) %>%
#   mutate(density = fct_recode(density, `All patients` = "marginal", `Patients without diabetes` = "conditional"))

# plot_data %>%
#   ggplot(aes(x = risk_score, color = race)) +
#   geom_density(aes(weight = round(wtmec8yr/1000))) +
#   geom_polygon(data = polygon_data, aes(x = x, y = y, fill = race),
#                alpha = 0.2) +
#   geom_vline(xintercept = 0.015, show.legend = FALSE) +
#   facet_wrap(vars(fct_rev(density))) +
#   geom_vline(data = line_annotations, aes(xintercept = incidence, color = race),
#              linetype = "dashed", show.legend = FALSE) +
#   xlab("Probability of having diabetes") +
#   ylab("Density") +
#   scale_x_continuous(labels = scales::percent,
#                      expand = c(0,0)) +
#   theme_bw() +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         axis.ticks.y = element_blank(),
#         axis.title = element_text(size = 9),
#         axis.text = element_text(size = 8),
#         axis.text.y=element_blank(),
#         legend.text = element_text(size = 9), panel.spacing = unit(1.5, "lines"),
#         plot.margin = margin(10, 10, 10, 10, "pt")) +
#   scale_color_manual(values=group_color_map) +
#   scale_fill_manual(values=group_color_map) +
#   coord_cartesian(xlim = c(0, .2), expand = FALSE, y = c(0, 13)) +
#   guides(colour = guide_legend(override.aes = list(alpha = 0.2)))
#   # guides(color = guide_legend(override.aes = list(fill = "white")))  # removes fill from legend
# 
# 
# ggsave(paste(save_path, "risk_distribution.pdf", sep = ""),
#        width = 5.5,
#        height = 4)

# Attempt at using freq poly instead -------------------------------------------
#  -----------------------------------------------------------------------------


# Freqpoly (density)
# plot_data %>%
#   ggplot(aes(x = risk_score, y = after_stat(density), color = race)) +
#   geom_freqpoly(aes(weight = round(wtmec8yr/1000)), binwidth=0.005) +
#   geom_vline(xintercept = 0.015, show.legend = FALSE) +
#   facet_wrap(vars(fct_rev(density))) +
#   geom_vline(data = line_annotations, aes(xintercept = incidence, color = race),
#              linetype = "dashed", show.legend = FALSE) +
#   xlab("Probability of having diabetes") +
#   ylab("Density") + 
#   scale_x_continuous(labels = scales::percent,
#                      expand = c(0,0)) +
#   theme_bw() +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom", 
#         axis.ticks.y = element_blank(), 
#         axis.title = element_text(size = 9), 
#         axis.text = element_text(size = 8), 
#         axis.text.y=element_blank(),
#         legend.text = element_text(size = 9), panel.spacing = unit(1.5, "lines"), 
#         plot.margin = margin(10, 10, 10, 10, "pt")) +
#   scale_color_brewer(palette = "Set2") +
#   coord_cartesian(xlim = c(0, .2))
# 
# # Histogram with freq poly (counts)
# plot_data %>%
#   ggplot(aes(x = risk_score, color = race)) +
#   geom_histogram(aes(weight = round(wtmec8yr/1000)), binwidth=0.005, fill = "white") +
#   geom_freqpoly(aes(weight = round(wtmec8yr/1000), color = race), binwidth=0.005) +
#   geom_vline(xintercept = 0.015, show.legend = FALSE) +
#   facet_wrap(vars(fct_rev(density))) +
#   geom_vline(data = line_annotations, aes(xintercept = incidence, color = race),
#              linetype = "dashed", show.legend = FALSE) +
#   xlab("Probability of having diabetes") +
#   # ylab("Density") + 
#   scale_x_continuous(labels = scales::percent,
#                      expand = c(0,0)) +
#   theme_bw() +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom", 
#         axis.ticks.y = element_blank(), 
#         axis.title = element_text(size = 9), 
#         axis.text = element_text(size = 8), 
#         axis.text.y=element_blank(),
#         legend.text = element_text(size = 9), panel.spacing = unit(1.5, "lines"), 
#         plot.margin = margin(10, 10, 10, 10, "pt")) +
#   scale_color_brewer(palette = "Set2") +
#   coord_cartesian(xlim = c(0, .2))
