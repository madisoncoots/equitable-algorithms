library(tidyverse)
library(scales)

data <- readRDS("/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/data.rds")

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

marginal_plot_data <- data_with_pred %>% 
  # filter(race == "Asian American" | race == "White American") %>%
  mutate(density = "marginal")

# Experimenting with marginal plot data only
# ------------------------------------------------------------------------------

white_risk_score <- marginal_plot_data %>%
  filter(race == "White American") %>%
  select(risk_score, wtmec8yr)

density()

white_density <- density(white_risk_score$risk_score,
                         weights = white_risk_score$wtmec8yr/sum(white_risk_score$wtmec8yr),
                         n = 1024)

white_density_df <- data.frame(
  x = white_density$x,
  y = white_density$y,
  race = "White American"
) %>%
  mutate(y = if_else(x > 0.015, y, 0)) %>%
  bind_rows(data.frame(x = 0.015, y = 0, race = "White American")) %>%
  arrange(x, y)

marginal_plot_data %>%
  ggplot(aes(x=risk_score)) +
  geom_polygon(data = white_density_df, aes(x = x, y = y, fill = race),
  alpha = 0.2) +
  geom_density(aes(weight=round(wtmec8yr/1000), color = race)) + # !!!! NOTE !!!! bw makes a pretty big diff here, why???
  # geom_density(aes(weight=round(wtmec8yr/1000), fill = race, color = race), alpha = 0.2) +
  # geom_vline(data = line_annotations, aes(xintercept = incidence, color = race),
  #            linetype = "dashed") +
  geom_vline(xintercept = 0.015) +
  # geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(0, 0.2), expand = FALSE, y = c(0, 11)) +
  xlab("Probability of having diabetes") +
  ylab("Density") +
  scale_x_continuous(labels = scales::percent,
                     expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = c(.8, .82),
        legend.title = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank()) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  
  guides(fill = "none")  # removes fill from legend


dpb <- ggplot_build(density_plot)

# ------------------------------------------------------------------------------


 
conditional_plot_data <- data_with_pred %>%
  # filter(race == "Asian American" | race == "White American") %>%
  filter(!diabetes) %>%
  mutate(density = "conditional")
# 
# conditional_plot_data %>%
#   ggplot(aes(x=risk_score, color=race)) +
#   geom_density(aes(weight=round(wtmec8yr/1000))) +
#   geom_vline(xintercept = 0.015) +
#   xlab("Probability of having diabetes") +
#   ylab("Density") + 
#   scale_x_continuous(labels = scales::percent,
#                      expand = c(0,0)) +
#   theme_bw() +
#   theme(legend.position = c(.8, .82),
#         legend.title = element_blank(),
#         axis.ticks.y=element_blank(),
#         axis.text.y=element_blank()) +
#   scale_color_brewer(palette = "Set2") +
#   coord_cartesian(xlim = c(0, 0.2))

plot_data <- bind_rows(marginal_plot_data, conditional_plot_data) %>%
  mutate(density = fct_recode(density, `All patients` = "marginal", `Patients without diabetes` = "conditional"))

plot_data %>%
  ggplot(aes(x = risk_score, color = race)) +
  geom_density(aes(weight = round(wtmec8yr/1000))) +
  geom_vline(xintercept = 0.015, show.legend = FALSE) +
  facet_wrap(vars(fct_rev(density))) +
  geom_vline(data = line_annotations, aes(xintercept = incidence, color = race),
             linetype = "dashed", show.legend = FALSE) +
  xlab("Probability of having diabetes") +
  ylab("Density") + 
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
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(xlim = c(0, .2))

density(plot_data$risk_score)$x

# Attempt at using freq poly instead -------------------------------------------
#  -----------------------------------------------------------------------------


# Freqpoly (density)
plot_data %>%
  ggplot(aes(x = risk_score, y = after_stat(density), color = race)) +
  geom_freqpoly(aes(weight = round(wtmec8yr/1000)), binwidth=0.005) +
  geom_vline(xintercept = 0.015, show.legend = FALSE) +
  facet_wrap(vars(fct_rev(density))) +
  geom_vline(data = line_annotations, aes(xintercept = incidence, color = race),
             linetype = "dashed", show.legend = FALSE) +
  xlab("Probability of having diabetes") +
  ylab("Density") + 
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
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(xlim = c(0, .2))

# Histogram with freq poly (counts)
plot_data %>%
  ggplot(aes(x = risk_score, color = race)) +
  geom_histogram(aes(weight = round(wtmec8yr/1000)), binwidth=0.005, fill = "white") +
  geom_freqpoly(aes(weight = round(wtmec8yr/1000), color = race), binwidth=0.005) +
  geom_vline(xintercept = 0.015, show.legend = FALSE) +
  facet_wrap(vars(fct_rev(density))) +
  geom_vline(data = line_annotations, aes(xintercept = incidence, color = race),
             linetype = "dashed", show.legend = FALSE) +
  xlab("Probability of having diabetes") +
  # ylab("Density") + 
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
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(xlim = c(0, .2))
