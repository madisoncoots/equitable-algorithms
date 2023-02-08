library(tidyverse)

# This is not the file you're looking for

read_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/survey.rds"

cleaned_survey_results <- readRDS(read_path)

survey_points <- data.frame(
  new_appearances = c(680, 730, 710, 620, 520),
  pct_rides_black_clients = c(10, 30, 50, 70, 90)) %>%
  mutate(data = "survey_pts",
         plot_label = "Pareto frontier for ride allocation")

spline_points <- as.data.frame(spline(survey_points$pct_rides_black_clients, survey_points$new_appearances)) %>%
  rename(pct_rides_black_clients = x,
         new_appearances = y) %>%
  mutate(data = "spline",
         plot_label = "Pareto frontier for ride allocation")

preferences_data <- cleaned_survey_results %>%
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
  mutate(data = "preferences",
         plot_label = "Ride allocation preferences") %>%
  rename(pct_rides_black_clients = preference_pct_black)

facet_plot_data <- bind_rows(preferences_data, survey_points, spline_points)

ggplot(data = facet_plot_data) +
  facet_wrap(.~plot_label, scales = "free_y",
             ) +
  geom_line(data = facet_plot_data %>% filter(data == "spline"), 
            aes(x=pct_rides_black_clients, y=new_appearances), color = "#00BFC4") + 
  geom_point(data = facet_plot_data %>% filter(data == "survey_pts"),
             aes(x=pct_rides_black_clients, y=new_appearances)) +
  geom_bar(data = facet_plot_data %>% filter(data == "preferences"),
           aes(x = pct_rides_black_clients, y = proportion),
           stat='identity', width = 10, show.legend = FALSE, fill = "#00BFC4") +
  theme_bw() +
  ylab(c("")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     breaks = c(10, 30, 50, 70, 90),
                     minor_breaks = c(),
                     name = "Percentage of rides offered to Black clients")

  