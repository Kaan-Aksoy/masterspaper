# Load libraries ----
library(tidyverse)
library(haven)
library(labelled)
library(countrycode)

# Reading in the data and harmonising ----
mydata1 <- read_delim("~/Documents/Data/GurievTreisman2019Data/Replication/masskillings.txt") %>% 
  filter(., year >= 1975 & year <= 2013) %>% 
  drop_na(., ccode)
  # naniar::replace_with_na(., replace = list(yrdied = 9999))
mydata2 <- read_dta("~/Documents/Data/GurievTreisman2019Data/Replication/DPI2017.dta", skip = 2) %>% 
  filter(., ifs != 0 & year >= 1975 & year <= 2013) %>% 
  mutate_if(., is.labelled, to_factor) %>%
  naniar::replace_with_na_all(., condition = ~.x == -999) %>% 
  mutate(., across(ifs,
                   ~ifelse(.x %in% c('CSK', 'DDR', 'ROM', 'SUN', 'TMP', 'YMD', 'YSR', 'ZAR'),
                           c('CZE', 'GDR', 'ROM', 'RUS', 'ETM', 'YPR', 'YUG', 'DRC'), .x))) %>%
  mutate(., ccode = countrycode(.$ifs,
                               origin = 'iso3c',
                               destination = 'cown'))
mydata3 <- read_delim("~/Documents/Data/GurievTreisman2019Data/Replication/ideology.txt") %>% 
  filter(., year >= 1975 & year <= 2013) %>% 
  select(., -c('Notes', 'Which other'))  # ccode = Correlates of War
mydata4 <- read_csv("~/Documents/Data/V_Dem_v12.csv") %>% 
  filter(., year >= 1975 & year <= 2013) %>% 
  mutate(., ccode = COWcode)

# Create a larger dataset.

df1 <- list(mydata1, mydata2, mydata3, mydata4) %>%
  reduce(left_join, by = c('ccode', 'year')) %>% 
  mutate(., country = country.x,
         .before = "country.x") %>% 
  select(., -c("country.x", "country.y", "sftgcode", "countryname", "ifs"))
