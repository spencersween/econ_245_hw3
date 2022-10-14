rm(list = ls())

##### Import Libraries #####
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)
library(janitor)

##### Start Homework #####

# Question 1
uof = read_csv("uof_louisville.csv")

# Question 2a
uof = uof %>% mutate(hour = lubridate::hour(time_of_occurrence))
frequent_hour = uof %>% select(hour) %>% 
  group_by(hour) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  pull(hour)

# Question 2b
uof = uof %>% mutate(month = lubridate::month(date_of_occurrence))
least_frequent_month = uof %>% select(month) %>% 
  group_by(month) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(-n)) %>% 
  slice(1) %>% 
  pull(month)

# Question 2c
uof = uof %>% mutate(day = lubridate::wday(date_of_occurrence, label = T))
most_frequent_day = uof %>% 
  mutate(day = lubridate::wday(date_of_occurrence, label = T)) %>% 
  select(day) %>% 
  group_by(day) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  pull(day)

# Question 2d
day_distribution = uof %>% mutate(day = lubridate::day(date_of_occurrence)) %>% 
  select(day) %>% 
  group_by(day) %>% 
  count() %>% 
  mutate(fraction = n/1000) %>% 
  arrange(desc(n)) %>% 
  adorn_totals() %>% 
  tibble()

# Question 3a
force_used_1 = unique(uof$force_used_1)

# Question 3b
force_used_2 = unique(uof$force_used_2)

# Question 3c
all_force = uof %>%
  select(force_used_1, 
         force_used_2, 
         force_used_3, 
         force_used_4, 
         force_used_4, 
         force_used_5, 
         force_used_6, 
         force_used_7, 
         force_used_8) %>% 
  distinct() %>% ## edit this line
  t() %>% ## do not edit this line
  c() %>% 
  unique()

# Question 3d
violent_force <- c("take down", "hobble", "ecw cartridge deployed", 
                   "knee strike(s)", "12 ga. sock round", "take-down", 
                   "impact weapon","kick", "deadly force used")

# Question 3e
uof$violent_uof_1 = ifelse(uof$force_used_1 %in% violent_force, 1, 0)
uof$violent_uof_1 = ifelse(is.na(uof$force_used_1), NA, uof$violent_uof_1)

# Question 3f
violent_force_service_table = uof %>% filter(violent_uof_1 == 1) %>% 
  tabyl(service_rendered, ) %>% 
  arrange(desc(n)) %>% 
  select(service_rendered, n, percent) %>% 
  adorn_totals()

# Question 4a
uof_filtered = uof %>% 
  filter( (citizen_gender == "female" | citizen_gender == "male") &
                  (is.na(citizen_race) == F)) %>% 
   mutate(force_used_1_effective_binary = 
            ifelse(force_used_1_effective == "yes", 1, 0))

# Question 4b
uof_filtered_table = uof_filtered %>% 
  group_by(citizen_gender, citizen_race) %>% 
  summarize(effective_1 = sum(force_used_1_effective_binary, na.rm = T), 
            counts = n()) %>% 
  adorn_totals() %>% 
  mutate(fraction_effective = effective_1/counts)
