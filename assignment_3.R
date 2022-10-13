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
uof %>% mutate(hour = hour(time_of_occurrence)) %>% tabyl(hour) %>% arrange(desc(n))
frequent_hour = c(23)
is.vector(frequent_hour)

uof = uof %>% mutate(month = month(time_of_occurrence))
uof %>% mutate(month = month(time_of_occurrence)) %>% tabyl(month) %>% arrange(desc(n))
least_frequent_month = c(NA)
is.vector(least_frequent_month)

uof = uof %>% mutate(day = wday(time_of_occurrence))
uof %>% mutate(day = wday(time_of_occurrence)) %>% tabyl(day) %>% arrange(desc(n))
most_frequent_day = c(NA)


day_distribution = uof %>% 
  mutate(day = day(time_of_occurrence)) %>% tabyl(day) %>% arrange(desc(n)) %>% adorn_totals()

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

uof = uof %>% mutate(violent_uof_1 = ifelse(force_used_1 %in% violent_force, 1, 0))
                     
violent_force_service_table = uof %>% filter(violent_uof_1 == 1) %>% 
  tabyl(service_rendered)  %>% 
  arrange(desc(n)) %>% 
  select(service_rendered, n, percent) %>% 
  rename(fraction = percent) %>% 
  adorn_totals()
