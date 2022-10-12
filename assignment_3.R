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
uof = uof %>% mutate(hour = hour(time_of_occurrence))
x = uof %>% 
  select(hour) %>% 
  group_by(hour) %>% 
  summarise(n()) %>%
  ungroup() %>% top_n(1) %>% 
  select(hour)
frequent_hour = c(x[1])

uof = uof %>% mutate(month = month(date_of_occurrence))
x = uof %>% 
  select(month) %>% 
  group_by(month) %>% 
  summarise(n()) %>%
  ungroup() %>% top_n(-1) %>% 
  select(month)
least_frequent_month = c(x[1])

uof = uof %>% mutate(day = wday(date_of_occurrence))
x = uof %>% 
  select(day) %>% 
  group_by(day) %>% 
  summarise(n()) %>%
  ungroup() %>% top_n(1) %>% 
  select(day)
most_frequent_day = c(x[1])


day_distribution = uof %>% 
  mutate(day = day(date_of_occurrence)) %>% 
  group_by(day) %>% 
  summarize(n = n()) %>%
  adorn_totals() %>% 
  mutate(fraction = n/1000)


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

uof = uof %>% mutate(violent_uof_1 = force_used_1 %in% violent_force)

violent_force_service_table = uof %>% 
  filter(violent_uof_1 == 1) %>% 
  group_by(service_rendered) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  adorn_totals() %>% 
  mutate(fraction = n/224)
violent_force_service_table
