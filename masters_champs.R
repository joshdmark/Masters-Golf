library(tidyverse)
library(data.table)
library(sqldf)
library(RCurl)

## get master data 
masters_data_url <- getURL('https://raw.githubusercontent.com/joshdmark/Masters-Golf/main/masters_scores_raw.csv')
masters_data <- read.csv(text = masters_data_url, stringsAsFactors = FALSE) %>% data.frame()
rm(masters_data_url) ## remove url 

## rename column 1
names(masters_data)[1] <- 'year'

## tidy data: convert from WIDE to LONG data
masters_data <- masters_data %>% 
  # mutate(r3_score = ifelse(finish == 'CUT', NA, r3_score), 
  #        r4_score = ifelse(finish == 'CUT', NA, r4_score))
  gather(key = 'round', value = 'score', r1_score:r4_score) %>% 
  group_by(year, player) %>% 
  mutate(rows_per_year = n()) %>% 
  ungroup() %>% 
  data.frame() %>% 
  mutate(prize_money = str_replace_all(string = prize_money, pattern = ',', replacement = ''), 
         prize_money = str_replace_all(string = prize_money, pattern = '\\$', replacement = '')) %>% 
  mutate(prize_money = as.numeric(prize_money)) %>% 
  mutate(winner_ind = ifelse(finish == 1, 1, 0), 
         prize_money_small = prize_money / rows_per_year) %>% 
  data.table() %>%
  .[finish == 'CUT' & round %in% c('r4_score', 'r3_score'), score := NA] %>% 
  data.frame() %>% 
  arrange(desc(year), desc(finish))

## get winners for past 20 years 
masters_winners <- masters_data %>% 
  filter(winner_ind == 1 & year >= 2001) %>% 
  distinct(player)

## filter masters_data to only winners since 2001
masters_data <- masters_data %>% 
  filter(player %in% masters_winners$player)

## output final file
fwrite(masters_data, 'C:/Users/joshua.mark/OneDrive - Accenture/Desktop/Sports/masters_data.csv')
