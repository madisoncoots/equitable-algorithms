library(tidyverse)
 
data <- readRDS("/Users/madisoncoots/Documents/harvard/research/race-diabetes/data/data.rds") %>%
  filter(sddsrvyr == 7,
         ridageyr >= 35)

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

data_with_pred %>% 
  filter(race == "Asian American" | race == "White American") %>%
  ggplot(aes(x=risk_score, color=race)) +
  geom_density() +
  geom_vline(data = diabetes_incidence_asian, aes(xintercept = incidence, 
                                                  color = race),
             linetype = "dashed") +
  geom_vline(data = diabetes_incidence_white, aes(xintercept = incidence, color = race),
             linetype = "dashed") +
  geom_vline(xintercept = 0.015) +
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
  coord_cartesian(xlim = c(0, 0.2))









test <- regression_data %>%
  filter(ridageyr >= 35,
         ridageyr <= 70,
         sddsrvyr == 7)

diabetes_incidence_white <- test %>% 
  filter(race == "White American") %>% 
  summarize(incidence = mean(diabetes)) %>%
  mutate(race = "White American")
