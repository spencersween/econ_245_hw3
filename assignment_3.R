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

# Question 2
uof = uof %>% mutate(hour = lubridate::hour(time_of_occurrence))
frequent_hour = uof %>% select(hour) %>% 
  group_by(hour) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  pull(hour)


uof = uof %>% mutate(month = lubridate::month(date_of_occurrence))
least_frequent_month = uof %>% select(month) %>% 
  group_by(month) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(-n)) %>% 
  slice(1) %>% 
  pull(month)

uof = uof %>% mutate(day = lubridate::wday(date_of_occurrence))
most_frequent_day = uof %>% select(day) %>% 
  group_by(day) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  pull(day)

day_distribution = uof %>% mutate(day = lubridate::day(date_of_occurrence)) %>% 
  select(day) %>% 
  group_by(day) %>% 
  count() %>% 
  mutate(fraction = n/1000) %>% 
  arrange(desc(n)) %>% 
  adorn_totals() %>% 
  tibble()

# Question 3
force_used_1 = unique(uof$force_used_1)
force_used_2 = unique(uof$force_used_2)

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

violent_force <- c("take down", "hobble", "ecw cartridge deployed", "knee strike(s)",
                   "12 ga. sock round", "take-down", "impact weapon",
                   "kick", "deadly force used")

uof = uof %>% mutate(violent_uof_1 = ifelse((force_used_1 %in% violent_force) == T, 1, 0))

violent_force_service_table = uof %>% filter(violent_uof_1 == 1) %>% 
  tabyl(service_rendered)  %>% 
  arrange(desc(n)) %>% 
  select(service_rendered, n, percent) %>% 
  rename(fraction = percent) %>% 
  adorn_totals()


violent_force_service_table
