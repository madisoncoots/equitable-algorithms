library(tidyverse)
library(readr)
library(geosphere)
library(measurements)

save_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/rideshare/figures/"

# data_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/county_samples.csv"
# 
# county_samples <- read_csv(data_path)
# suffolk_court_lat <- 42.35869
# suffolk_court_lon <- -71.06043
# 
# calc_distance <- function(data, pt_lat, pt_lon) {
#   data %>%
#     select(longitude, latitude) %>%
#     distGeo(c(pt_lon, pt_lat)) %>%
#     conv_unit(from = "m", to = "mi")
# }
# 
# set.seed(1)
# county_samples_with_distance = county_samples %>%
#   filter(Race == "White" | Race == "Black") %>%
#   mutate(distance = calc_distance(., suffolk_court_lat, suffolk_court_lon),
#          R = as.integer(Race == "Black")) %>%
#   sample_n(n()) %>% # this line shuffles the rows
#   group_by(R) %>%
#   mutate(index = row_number()) %>%
#   ungroup()
# 
# inv_logit <- function(x) {
#   return (exp(x)/(1+exp(x)))
# }
# 
# gen_book_of_life <- function(
#     pop_size,
#     p_black,
#     beta_0_g2,
#     beta_1_g2,
#     beta_2_g2,
#     beta_0_g1,
#     beta_1_g1,
#     beta_2_g1
# ) {
# 
#   # Appearance potential outcomes
#   p_appearance <- function(r, x, t) {
#     p <- inv_logit(
#       (1-r) * (beta_0_g1 + beta_1_g1 * x + beta_2_g1 * x * t) +
#         r * (beta_0_g2 + beta_1_g2 * x + beta_2_g2 * x * t)
#     )
#     return(p)
#   }
# 
#   # Generate the book of life
#   num_g2 <- round(pop_size * p_g2)
#   book_of_life <- tibble(
#     R = sample(c(rep(0,num_g2), rep(1,pop_size-num_g2))),
#     X = runif(pop_size),
#     Y_0 = p_appearance(R, X, 0),
#     Y_1 = p_appearance(R, X, 1),
#     TE = Y_1 - Y_0
#   ) %>%
#     group_by(R) %>%
#     mutate(index = row_number()) %>%
#     ungroup()
#   return(book_of_life)
# }
# 
# 
# 
# 
# compute_allocations <- function(book_of_life, budget) {
#   n_iters <- 500 # Picked an arbitrarily large n to ensure loop doesn't end before budget is spent
#   all_outputs <- data.frame()
#   for (i in 1:n_iters) {
#     g1_treated <- book %>%
#       filter(R==0) %>%
#       arrange(-TE_per_dollar) %>%
#       mutate(cum_cost = cumsum(cost),
#              group = 1) %>%
#       head(i)
# 
#     cost_to_treat_top_i_g1 <- g1_treated %>%
#       tail(1) %>%
#       pull(cum_cost)
# 
#     if (cost_to_treat_top_i_g1 > budget) {
#       break
#     }
# 
#     remaining_budget <- budget - cost_to_treat_top_i_g1
# 
#     g2_treated <- book %>%
#       filter(R==1) %>%
#       arrange(-TE_per_dollar) %>%
#       mutate(cum_cost = cumsum(cost),
#              group = 2) %>%
#       filter(cum_cost <= remaining_budget)
# 
#     output <- bind_rows(g1_treated, g2_treated) %>%
#       summarize(appearances = sum(Y_1),
#                 new_appearances = sum(TE),
#                 new_g1_appearances = sum(TE %*% (group == 1)),
#                 new_g2_appearances = sum(TE %*% (group == 2)),
#                 num_g1_treated = i,
#                 num_g2_treated = n() - i,
#                 total_spent = sum(cost),
#                 num_treated = n())
# 
#     all_outputs <- rbind(all_outputs, output)
#   }
#   ret_table <- all_outputs %>%
#     mutate(pct_g1_treated = num_g1_treated / num_treated * 100,
#            pct_g2_treated = num_g2_treated / num_treated * 100)
#   return(ret_table)
# }
# 
# 
# # params
# pop_size <- 1e3
# p_g2 <- 0.5
# beta_0 <- -1 # original -1
# beta_1 <- 0.88 # original 1
# beta_2 <- 24 # original 40
# beta_0_g2 <- beta_0
# beta_1_g2 <- beta_1
# beta_2_g2 <- beta_2
# beta_0_g1 <- beta_0
# beta_1_g1 <- beta_1
# beta_2_g1 <- beta_2
# budget <- 1000
# 
# set.seed(5)
# adjusted_county_samples_with_distance <- county_samples_with_distance %>%
#   mutate(distance = if_else(Race == "Black", distance - runif(1, 0, 4.25), distance))
# 
# # call
# set.seed(5)
# book <- gen_book_of_life(
#   pop_size,
#   p_g2,
#   beta_0_g2,
#   beta_1_g2,
#   beta_2_g2,
#   beta_0_g1,
#   beta_1_g1,
#   beta_2_g1) %>%
#   left_join(adjusted_county_samples_with_distance, by = c("index", "R")) %>%
#   mutate(cost = 2 * 5 * distance,
#          TE_per_dollar = TE/cost)
# 
# # Call
# allocations <- compute_allocations(book, budget)
# 
# pareto_data <- allocations %>%
#   select(new_appearances, pct_g2_treated) %>%
#   rename(pct_rides_black_clients = pct_g2_treated) %>%
#   mutate(pct_rides_black_clients = pct_rides_black_clients / 100,
#          new_appearances = new_appearances * 10)

survey_points <- data.frame(
  new_appearances = c(680, 730, 710, 620, 520),
  pct_rides_black_clients = c(0.1, 0.3, 0.5, 0.7, 0.9))

spline_int <- as.data.frame(spline(survey_points$pct_rides_black_clients, survey_points$new_appearances))

survey_points %>%
  ggplot(aes(x = pct_rides_black_clients, y = new_appearances)) +
  # geom_smooth(aes(color = "Smooth"), size = 0.5, se = FALSE) +
  # geom_line(aes(color = "No smoothing")) + # No smoothing
  geom_line(data = spline_int, aes(x=x, y=y), color = "dimgray") +
  geom_segment(aes(x = 0.1, y = 0, xend = 0.1, yend = 680), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(x = 0.3, y = 0, xend = 0.3, yend = 730), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(x = 0.5, y = 0, xend = 0.5, yend = 710), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(x = 0.7, y = 0, xend = 0.7, yend = 620), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_segment(aes(x = 0.9, y = 0, xend = 0.9, yend = 520), color = "gray", linetype = "dashed", linewidth = 0.3) +
  geom_point(data = survey_points, aes(x = pct_rides_black_clients, y = new_appearances)) +
  annotate("text", x = 0.3 + 0.23, y = 730 + 10, label = "Maximum appearances (approx.)") + 
  annotate("text", x = 0.5 + 0.15, y = 700 + 15, label = "Demographic parity") + 
  ylab("Number of additional appearances") +
  xlab("Percentage of rides offered to Black clients") +
  scale_x_continuous(labels = scales::percent,
                     breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  coord_cartesian(xlim = c(0.1, 0.9),
                  ylim = c(500, 750)) +
  theme_bw() + 
  theme(legend.position = c(.84, .7),
        legend.title = element_blank())

ggsave(paste(save_path, "pareto_frontier.pdf", sep = ""),
       width = 5,
       height = 5) 

