library(readr)
library(janitor)
library(tidyverse)
library(ggplot2)
library(lubridate)

data_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/Nudge_Preferences_Expanded_Survey_December_12_2022_10.07.csv"

write_path <- "/Users/madisoncoots/Documents/harvard/research/equitable-algorithms/data/"

final_survey_date <- mdy("12/6/22") # this is when we started the final survey

raw_survey_results <- read_csv(survey_data_path, col_types = cols(EndDate = col_datetime(format = "%Y-%m-%d %H:%M:%S"))) %>%
  clean_names()

cleaned_survey_results <- raw_survey_results %>%
  # Removing first two rows which contain more metadata and question statements
  filter(row_number() != 1,
         row_number() != 2) %>%
  # Filter out results from the pilot 
  filter(as.Date(end_date) >= final_survey_date) %>%
  # Filter out results from the preview 
  filter(status != "Survey Preview") %>%
  # Drop metadata and feedback question
  select(-start_date,
         -end_date,
         -status,
         -ip_address,
         -progress,
         -duration_in_seconds,
         -finished,
         -recorded_date,
         -response_id,
         -recipient_last_name,
         -recipient_first_name,
         -recipient_email,
         -external_reference,
         -location_latitude,
         -location_longitude,
         -distribution_channel,
         -user_language) %>%
  rename(comprehension_q1 = x18,
         comprehension_q2 = x19) %>%
  # Determining survey version
  mutate(survey_version = if_else(!is.na(pref_white_asc) | !is.na(pref_white_desc), "white", "black")) %>%
  # Translating options to percentages for both versions of preferences question
  mutate(
    preference = 
      case_when(
        pref_white_asc == "Option A" | pref_black_asc == "Option A" ~ 10,
        pref_white_asc == "Option B" | pref_black_asc == "Option B" ~ 30,
        pref_white_asc == "Option C" | pref_black_asc == "Option C" ~ 50,
        pref_white_asc == "Option D" | pref_black_asc == "Option D" ~ 70,
        pref_white_asc == "Option E" | pref_black_asc == "Option E" ~ 90,
        pref_white_desc == "Option A" | pref_black_desc == "Option A" ~ 90,
        pref_white_desc == "Option B" | pref_black_desc == "Option B" ~ 70,
        pref_white_desc == "Option C" | pref_black_desc == "Option C" ~ 50,
        pref_white_desc == "Option D" | pref_black_desc == "Option D" ~ 30,
        pref_white_desc == "Option E" | pref_black_desc == "Option E" ~ 10
      )
  ) %>%
  # Determining figure version
  mutate(figure_version = if_else(!is.na(pref_white_asc) | !is.na(pref_black_asc), "asc", "desc")) %>%
  relocate(preference, .after = pref_black_desc) %>%
  relocate(survey_version, .after = preference) %>%
  relocate(figure_version, .after = survey_version) %>%
  # Filter to those who correctly answered comprehension questions
  filter(comprehension_q1 == "5 people who live far from court" &
           (
             (comprehension_q2 == "5 Black people" & survey_version == "black") |
               (comprehension_q2 == "5 White people" & survey_version == "white")
           )
  ) %>%
  mutate(
    survey_version_long = case_when(
      survey_version == "black" ~ "Black clients more expensive",
      survey_version == "white" ~ "Black clients less expensive"),
    preference_pct_black = case_when(
      survey_version == "white" & preference == 10 ~ 90,
      survey_version == "white" & preference == 30 ~ 70,
      survey_version == "white" & preference == 70 ~ 30,
      survey_version == "white" & preference == 90 ~ 10,
      TRUE ~ preference),
    binary_political_aff = case_when(
      political_aff == "No preference" | political_aff == "Other" ~ as.character(NA),
      !is.na(independent_q)  ~ independent_q,
      TRUE ~ political_aff),
    race_plus_hispanic = if_else(hispanic == "Yes", "Hispanic", race)
  )

saveRDS(cleaned_survey_results, paste(write_path, "survey.rds", sep = ""))

