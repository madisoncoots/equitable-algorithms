library(tidyverse)
 
data <- readRDS("/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/data.rds")

formula <- diabetes ~ race + ridageyr + bmxbmi +
  I(bmxbmi^2) + race:ridageyr + race:bmxbmi +
  race:I(bmxbmi^2) + ridageyr:bmxbmi + race:ridageyr:bmxbmi

model <- glm(formula,
             data = data,
             family = "binomial",
             weights = round(wtmec8yr/1000))

model_pred <- predict(model, newdata = data, type = "response")

data_with_pred <- data %>%
  mutate(risk_score = model_pred) %>%
  filter(!is.na(risk_score))

diabetes_incidence_white <- data %>% 
  filter(race == "White American") %>% 
  summarize(incidence = mean(diabetes)) %>%
  mutate(race = "White American")

diabetes_incidence_asian <- data %>% 
  filter(race == "Asian American") %>% 
  summarize(incidence = mean(diabetes)) %>%
  mutate(race = "Asian American")

line_annotations <- bind_rows(diabetes_incidence_white,
          diabetes_incidence_asian) %>%
  mutate(density = "marginal") %>%
  mutate(density = fct_recode(density, `All patients` = "marginal"))

# line_annotations <- data.frame(
#   risk_score = c(0.05, 0.05),
#   density = c("marginal", "marginal")) %>%
#   mutate(density = fct_recode(density, `All patients` = "marginal"))

marginal_plot_data <- data_with_pred %>% 
  filter(race == "Asian American" | race == "White American") %>%
  mutate(density = "marginal")

# marginal_plot_data %>%
#   ggplot(aes(x=risk_score, color=race)) +
#   geom_density(aes(weight=round(wtmec8yr/1000))) + # !!!! NOTE !!!! bw makes a pretty big diff here, why???
#   geom_vline(data = diabetes_incidence_asian, aes(xintercept = incidence, 
#                                                   color = race),
#              linetype = "dashed") +
#   geom_vline(data = diabetes_incidence_white, aes(xintercept = incidence, color = race),
#              linetype = "dashed") +
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
# 
conditional_plot_data <- data_with_pred %>%
  filter(race == "Asian American" | race == "White American",
         !diabetes) %>%
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
  coord_cartesian(xlim = c(0, .8))

