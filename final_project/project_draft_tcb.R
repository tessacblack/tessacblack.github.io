###### TCB Final Project Spring 2024 ######

library(tidyverse)
library(janitor)
library(patchwork)
library(skimr)
library(readxl)


# Loading and looking at data ####
hammies <- read_csv("IU106_Social Behavior First and Last Five Minutes_Formatted_6-6-23.csv")
hammies <- clean_names(hammies)
names(hammies)
str(hammies)


# Clean 
#fuck off
# Remove paw because we didn't score it, remove avoid because idek why it's there,
  # remove id bc it's a repeat col of juvenile_id, remove aggression_score bc 
  # idk what it is, remove receive_aggression duration bc there was no duration 
columns_removed <- c("id", "aggression_number_assigned", "comp1", "comp2", "comp3", 
                     "aggression_score_freq", "aggression_score_duration")
hammies <- select(hammies, -columns_removed)
names(hammies)

tidy_hammies <- hammies %>% 
  mutate(trial_timing = as.factor(first_or_last_five_minutes)) %>% 
  mutate(maternal_treatment = as.factor(maternal_treatment)) %>% 
  mutate(juvenile_id = as.factor(juvenile_id)) %>% 
  select(-first_or_last_five_minutes)
  
str(tidy_hammies)

# Subsetting data so that I have first and last as their own df's
behavior_last <- tidy_hammies %>% 
  filter(trial_timing == "Last",)
behavior_first <- tidy_hammies %>% 
  filter(trial_timing == "First",)

# From here, I'm going to model the effect of maternal treatment and sex on the frequencies of and durations of each of the behaviors 