# This file contains the code that builds the semi-synthetic population from
# the Suffolk County census data that was used for the ride allocation survey.
# The survey points were selected from the allocations table that gets created
# at the end of this script. Of note, we multiplied all of the numbers by 10
# to get bigger magnitudes to use in the survey. That means that the simulated
# population roughly represents 10,000 residents, of which 50% are White and 50%
# are Black. Lastly, we did a bit of rounding of the percentages (percent treated
# in group 2) in the allocations table to get cleaner, round numbers for the 
# survey.

library(tidyverse)
library(readr)
library(geosphere)
library(measurements)

county_samples <- read_csv("/home/mcoots/ride_allocation_lab/county_samples.csv")

suffolk_court_lat <- 42.35869
suffolk_court_lon <- -71.06043

calc_distance <- function(data, pt_lat, pt_lon) {
  data %>% 
    select(longitude, latitude) %>% 
    distGeo(c(pt_lon, pt_lat)) %>% 
    conv_unit(from = "m", to = "mi")
}

set.seed(1)
county_samples_with_distance = county_samples %>%
  filter(Race == "White" | Race == "Black") %>%
  mutate(distance = calc_distance(., suffolk_court_lat, suffolk_court_lon),
         R = as.integer(Race == "Black")) %>%
  sample_n(n()) %>% # this line shuffles the rows
  group_by(R) %>%
  mutate(index = row_number()) %>%
  ungroup()

inv_logit <- function(x) {
  return (exp(x)/(1+exp(x)))
}

gen_book_of_life <- function(
    pop_size,
    p_black,
    beta_0_g2,
    beta_1_g2,
    beta_2_g2,
    beta_0_g1,
    beta_1_g1,
    beta_2_g1
) {
  
  # Appearance potential outcomes
  p_appearance <- function(r, x, t) {
    p <- inv_logit(
      (1-r) * (beta_0_g1 + beta_1_g1 * x + beta_2_g1 * x * t) + 
        r * (beta_0_g2 + beta_1_g2 * x + beta_2_g2 * x * t)
    )
    return(p)
  }
  
  # Generate the book of life
  num_g2 <- round(pop_size * p_g2)
  book_of_life <- tibble(
    R = sample(c(rep(0,num_g2), rep(1,pop_size-num_g2))),
    X = runif(pop_size),
    Y_0 = p_appearance(R, X, 0),
    Y_1 = p_appearance(R, X, 1),
    TE = Y_1 - Y_0
  ) %>%
    group_by(R) %>%
    mutate(index = row_number()) %>%
    ungroup()
  return(book_of_life)
}




compute_allocations <- function(book_of_life, budget) {
  n_iters <- 500 # Picked an arbitrarily large n to ensure loop doesn't end before budget is spent
  all_outputs <- data.frame()
  for (i in 1:n_iters) {
    g1_treated <- book %>% 
      filter(R==0) %>%
      arrange(-TE_per_dollar) %>%
      mutate(cum_cost = cumsum(cost),
             group = 1) %>%
      head(i)
    
    cost_to_treat_top_i_g1 <- g1_treated %>% 
      tail(1) %>%
      pull(cum_cost)
    
    if (cost_to_treat_top_i_g1 > budget) {
      break
    }
    
    remaining_budget <- budget - cost_to_treat_top_i_g1
    
    g2_treated <- book %>%
      filter(R==1) %>%
      arrange(-TE_per_dollar) %>%
      mutate(cum_cost = cumsum(cost),
             group = 2) %>%
      filter(cum_cost <= remaining_budget)
    
    output <- bind_rows(g1_treated, g2_treated) %>%
      summarize(appearances = sum(Y_1),
                new_appearances = sum(TE),
                new_g1_appearances = sum(TE %*% (group == 1)),
                new_g2_appearances = sum(TE %*% (group == 2)),
                num_g1_treated = i,
                num_g2_treated = n() - i,
                total_spent = sum(cost),
                num_treated = n())
    
    all_outputs <- rbind(all_outputs, output)  
  }
  ret_table <- all_outputs %>%
    mutate(pct_g1_treated = num_g1_treated / num_treated * 100,
           pct_g2_treated = num_g2_treated / num_treated * 100)
  return(ret_table)
}



# Run me to get a table of survey points!
get_survey_points <- function(allocations, num_points) {
  survey_points <- allocations %>%
    mutate(row_number = row_number())
  
  parity_point <- survey_points %>% 
    mutate(delta = abs(pct_g1_treated - pct_g2_treated)) %>%
    filter(delta == min(delta)) %>% 
    select(-delta)
  
  parity_point_idx <- parity_point$row_number
  
  max_efficiency_point <- survey_points %>%
    filter(new_appearances == max(new_appearances))
  
  max_efficiency_point_idx <- max_efficiency_point$row_number
  
  range <- max_efficiency_point_idx - parity_point_idx + 1
  step_size <- floor(range / (num_points - 1))
  
  # Pre-populating first value
  first_row <- survey_points %>% filter(row_number == parity_point_idx)
  points_list <- rbind(parity_point)
  
  row_idx <- parity_point_idx
  for (i in 1:(num_points-2)) {
    row_idx <- row_idx + step_size
    next_row <- survey_points %>% filter(row_number == row_idx) 
    points_list <- rbind(points_list, next_row)
  }
  # Adding last value manually
  last_row <- survey_points %>% filter(row_number == max_efficiency_point_idx)
  points_list <- rbind(points_list, last_row)
  return(points_list)
}

# params
pop_size <- 1e3
p_g2 <- 0.5
beta_0 <- -1 # original -1 
beta_1 <- 0.88 # original 1
beta_2 <- 24 # original 40
beta_0_g2 <- beta_0
beta_1_g2 <- beta_1
beta_2_g2 <- beta_2
beta_0_g1 <- beta_0
beta_1_g1 <- beta_1
beta_2_g1 <- beta_2
budget <- 1000

# Remove later
set.seed(5)
adjusted_county_samples_with_distance <- county_samples_with_distance %>%
  mutate(distance = if_else(Race == "Black", distance - runif(1, 0, 4.25), distance))

# call
set.seed(5)
book <- gen_book_of_life(
  pop_size, 
  p_g2, 
  beta_0_g2, 
  beta_1_g2,
  beta_2_g2, 
  beta_0_g1, 
  beta_1_g1, 
  beta_2_g1) %>%
  left_join(adjusted_county_samples_with_distance, by = c("index", "R")) %>%
  mutate(cost = 2 * 5 * distance,
         TE_per_dollar = TE/cost)

# Checking treatment effect size
book %>% group_by(R) %>% summarize(mean_TE = mean(TE))

# Sanity check
book %>% ggplot(aes(x = distance, color = Race)) + geom_density()
book %>% group_by(Race) %>% summarize(mean(distance))


# Call
allocations <- compute_allocations(book, budget)



