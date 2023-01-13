library(tidyverse)

pareto_data <- data.frame(
  pct_rides_black_clients = c(10, 30, 50, 70, 90),
  num_addl_appearances = c(680, 730, 710, 620, 520)
)
pareto_data %>%
  ggplot(aes(x = pct_rides_black_clients, y = num_addl_appearances)) +
  geom_point() +
  geom_line() +
  theme_bw()
