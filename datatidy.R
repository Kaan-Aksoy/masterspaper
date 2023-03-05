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
  mutate(., country = countryname) %>% 
  naniar::replace_with_na_all(., condition = ~.x == -999) %>% 
  mutate(., across(ifs,
                   ~ifelse(.x %in% c('CSK', 'DDR', 'ROM', 'SUN', 'TMP', 'YMD', 'YSR', 'ZAR'),
                           c('CZE', 'GDR', 'ROM', 'RUS', 'ETM', 'YPR', 'YUG', 'DRC'), .x))) %>%
  mutate(., ccode = countrycode(.$ifs,
                               origin = 'iso3c',
                               destination = 'cown')) %>% 
  select(., -c("countryname"))
mydata3 <- read_delim("~/Documents/Data/GurievTreisman2019Data/Replication/ideology.txt") %>% 
  filter(., year >= 1975 & year <= 2013) %>% 
  select(., -c('Notes', 'Which other'))  # ccode = Correlates of War
mydata4 <- read_csv("~/Documents/Data/V_Dem_v12.csv") %>% 
  filter(., year >= 1975 & year <= 2013) %>% 
  mutate(., ccode = COWcode) %>% 
  mutate(., country = country_name) %>% 
  select(., -c("country_name"))
mydata5 <- read_csv("~/Documents/Data/PTS-2022.csv") %>% # Political Terror Scale
  filter(., Year >= 1975 & Year <= 2013) %>% 
  mutate(., year = Year) %>% 
  mutate(., ccode = COW_Code_N) %>% 
  select(., -c("Country_OLD", "COW_Code_A", "WordBank_Code_A", "UN_Code_N", "Year", "COW_Code_N"))
mydata6 <- read_csv("~/Documents/Data/worldbank_gdppercapita.csv") %>% 
  pivot_longer(., cols = `1960`:`2021`,
               names_to = "year",
               values_to = "gdppc",
               values_drop_na = TRUE) %>% 
  filter(., year >= 1975 & year <= 2013) %>% 
  mutate(., ccode = countrycode(.$`Country Code`,
                                origin = 'iso3c',
                                destination = 'cown'),
         country = `Country Name`) %>% 
  mutate_at(., "year", as.numeric) %>% 
  select(., -c("Indicator Name", "Indicator Code", "Country Code", "Country Name")) %>% 
  na.omit(.)

# Create a larger dataset ----

df1 <- list(mydata1, mydata2, mydata3, mydata4, mydata5, mydata6) %>%
  reduce(left_join, by = c('ccode', 'year')) %>% 
  mutate(., country = country.x,
         .before = "country.x") %>% 
  select(., -c("country.x", "country.y", "sftgcode", "ifs", "country.x.x", "country.y.y", "Country")) %>% 
  select(., c("country", "year", "v2x_polyarchy", # Electoral democracy index
              "v2x_libdem", # Liberal democracy index
              "v2xel_frefair", # Elections free and fair (aggregate)
              "v2elintim", # Election government intimidation
              "v2elpeace", # Election violence
              "v2elfrfair", # Election free and fair
              "v2elaccept", # Election losers accept results
              "v2eltrnout", # Turnout
              "v2elsrgel", # Local government elected
              "v2ellocgov", # Local government exists check
              "v2psbars", # Barriers to parties
              "v2psparban", # Parties banned
              "v2psoppaut", # Opp. party autonomy
              "v2exbribe", # Executive bribery and corrupt exchange (0 high 4 low, reverse)
              "v2exembez", # Executive embezzlement and theft (same)
              "v2excrptps", # Public sector bribery and corrupt exchange (s)
              "v2exthftps", # Public sector embezzlement and theft (s)
              "v2regdur", # Regime duration in days
              "v2regsupgroupssize", # Regime support group size
              "v2juhcind", # High court independence
              "v2juncind", # Lower courts independence
              "v2juhccomp", # Compliance with high court
              "v2jucomp", # Compliance with lower courts
              "v2cltort", # Freedom from torture
              "v2clkill", # Freedom from political killings
              "v2clstown", # State ownership of economy
              "v2stcritrecadm", # Bureaucrat appointment criteria (0 nepotism, 4 merit)
              "v2csreprss", # Civil society repression (0 high 4 low)
              "v2mecenefm", # Media censorship effort
              "v2mecenefi", # Internet censorship effort
              "v2exl_legitideol", # Government ideological in nature
              "v2exl_legitlead", # Cult of personality around leader
              "v2exl_legitperf", # Performance legitimation—this one is pretty good
              "v2cacamps", # Polarisation
              "v2caviol", # Non-state political violence
              "v2castate", # State-administered mass mobilisation
              "polity2", # Polity2 score
              "PTS_A", # Political Terror Scale: Amnesty International
              "PTS_H", # Political Terror Scale: Human Rights Watch
              "PTS_S", # Political Terror Scale: State Department
              "gdppc" # GDP per capita
              ))

# Some variables need to have their scales reversed in order to be more intuitive.
reversr <- function (x, na.rm = T) {
  min(x, na.rm = T) - x + max(x, na.rm = T)
}

df1 <- df1 %>% 
  mutate(., v2elpeace_rev = scale(reversr(v2elpeace), center = TRUE, scale = FALSE),
         v2psbars_rev = scale(reversr(v2psbars), center = TRUE, scale = FALSE),
         v2psparban_rev = scale(reversr(v2psparban), center = TRUE, scale = FALSE),
         v2exbribe_rev = scale(reversr(v2exbribe), center = TRUE, scale = FALSE),
         v2exembez_rev = scale(reversr(v2exembez), center = TRUE, scale = FALSE),
         v2excrptps_rev = scale(reversr(v2excrptps), center = TRUE, scale = FALSE),
         v2exthftps_rev = scale(reversr(v2exthftps), center = TRUE, scale = FALSE),
         v2clstown_rev = scale(reversr(v2clstown), center = TRUE, scale = FALSE),
         v2csreprss_rev = scale(reversr(v2csreprss), center = TRUE, scale = FALSE),
         v2mecenefm_rev = scale(reversr(v2mecenefm), center = TRUE, scale = FALSE),
         v2mecenefi_rev = scale(reversr(v2mecenefi), center = TRUE, scale = FALSE)
  )

# Starting to run some analyses ----
library(estimatr) # In order to cluster standard errors.

# Our first hypothesis: as political violence increases, low level bribery will increase.
# Clustering standard errors by country in order to get a clearer picture.

lm1 <- lm_robust(v2excrptps_rev ~ v2cltort * v2clkill + scale(v2exl_legitperf, center = T, scale = F),
                clusters = country,
                data = df1)
lm2 <- lm_robust(v2exbribe_rev ~ v2cltort * v2clkill + scale(v2exl_legitperf, center = T, scale = F),
                 clusters = country,
                 data = df1)
lm3 <- lm_robust(v2exthftps_rev ~ v2cltort * v2clkill + scale(v2exl_legitperf, center = T, scale = F),
                 clusters = country,
                 data = df1)
lm4 <- lm_robust(v2exembez_rev ~ v2cltort * v2clkill + scale(v2exl_legitperf, center = T, scale = F),
                 clusters = country,
                 data = df1)

# Report them ----
library(modelsummary)
library(kableExtra)

modelsummary(list(lm1, lm2, lm3, lm4),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             gof_omit = 'AIC|BIC|Log.Lik.|RMSE')

ggplot(df1,
       aes(x = (v2cltort * v2clkill),
           y = v2excrptps)) +
  geom_point() +
  geom_smooth(method = 'lm')
