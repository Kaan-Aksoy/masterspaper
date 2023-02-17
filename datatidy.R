library(tidyverse)
library(haven)
library(labelled)
library(countrycode) # The problem with this one is that it doesn't seem to accept tibbles.

# Reading in the data.
mydata1 <- read_delim("~/Documents/Data/GurievTreisman2019Data/Replication/masskillings.txt") %>% 
  filter(., year >= 1975) %>% 
  drop_na(., ccode)
mydata2 <- read_dta("~/Documents/Data/GurievTreisman2019Data/Replication/DPI2017.dta", skip = 2) %>% 
  filter(., ifs != 0) %>% 
  mutate_if(., is.labelled, to_factor) %>% 
  naniar::replace_with_na_all(., condition = ~.x == -999)
mydata3 <- read_delim("~/Documents/Data/GurievTreisman2019Data/Replication/ideology.txt") %>% 
  filter(., year >= 1975) %>% 
  select(., -c(Notes, `Which other`))
mydata4 <- read_csv("~/Documents/Data/V_Dem_v12.csv") %>% 
  filter(., year >= 1975)

# Harmonise across countries.

