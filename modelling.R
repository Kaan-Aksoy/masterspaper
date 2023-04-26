# Load libraries ----
library(tidyverse)
library(haven)
library(estimatr)     # To cluster standard errors
library(modelsummary) # For neat reporting tables
library(kableExtra)   # For additional table formatting
library(countrycode)  # Converting country coding schemes
library(sandwich)
library(lmtest)

# Load and clean data ----
mydata1 <- read_csv("~/Documents/Data/V_Dem_v13.csv") %>% 
  mutate(., region = as_factor(e_regionpol)) %>% # Recoding regions as factors
  select(., c("country_name", "country_text_id", "year", "v2x_polyarchy", "v2x_clphy", 
              "v2excrptps", "v2exl_legitperf", "e_gdppc", "e_total_resources_income_pc", "region")) %>% 
  mutate(., democracy = as_factor(if_else(v2x_polyarchy <= 0.42, 0, 1)))

mydata2 <- read_delim("~/Documents/Data/masskillings.txt") %>% 
  mutate(., masskilling = as_factor(lag(ongoing, n = 3)),
         country_name = country,
         country_text_id = sftgcode) %>% 
  select(., c("country_text_id", "year", "masskilling")) %>% 
  na.omit(.)

# So, it's problematic to try and expand the years. Recreating the dataset is actually easier, done below.
# mydata3 <- read_dta("~/Documents/Data/marx.dta") %>%
#   drop_na(., cowcode) %>% # Some unrecognised countries (e.g., Abkhazia) cause issues here.
#   mutate(., country_text_id = countrycode(cowcode, origin = 'cown', destination = 'cowc')) %>%
#   select(., c("country_text_id", "year", "marx"))

# This is the solution I hacked together. Not really proud of it, but it gets the job done.
mydata3 <- expand_grid(year = c(1789:2022),
                       country_text_id = unique(mydata1$country_text_id)) %>% 
  mutate(., marx = case_when(
    country_text_id == "CHN" & year > 1949 ~ 1,
    country_text_id == "LAO" & year > 1975 ~ 1,
    country_text_id == "VNM" & year > 1959 ~ 1,
    country_text_id == "CUB" & year > 1959 ~ 1,
    # country_text_id == "PRK" & year > 1945 ~ 1, # Uncomment for North Korea as communist today
    country_text_id == "RUS" & year > 1918 & year <= 1991 ~ 1,
    country_text_id == "AFG" & year > 1978 & year <= 1992 ~ 1,
    country_text_id == "UKR" & year > 1919 & year <= 1991 ~ 1,
    country_text_id == "BLR" & year > 1919 & year <= 1991 ~ 1,
    country_text_id == "DDR" & year > 1949 & year <= 1990 ~ 1,
    country_text_id == "EST" & year > 1918 & year <= 1991 ~ 1,
    country_text_id == "LVA" & year > 1918 & year <= 1991 ~ 1,
    country_text_id == "LTU" & year > 1918 & year <= 1991 ~ 1,
    country_text_id == "HUN" & year > 1944 & year <= 1989 ~ 1,
    country_text_id == "AZE" & year > 1920 & year <= 1991 ~ 1,
    country_text_id == "CZE" & year > 1944 & year <= 1990 ~ 1,
    country_text_id == "TJK" & year > 1920 & year <= 1991 ~ 1,
    country_text_id == "TKM" & year > 1920 & year <= 1991 ~ 1,
    country_text_id == "UZB" & year > 1920 & year <= 1991 ~ 1,
    country_text_id == "POL" & year > 1945 & year <= 1991 ~ 1,
    country_text_id == "GEO" & year > 1921 & year <= 1991 ~ 1,
    country_text_id == "ARM" & year > 1920 & year <= 1991 ~ 1,
    country_text_id == "MNG" & year > 1921 & year <= 1991 ~ 1,
    country_text_id == "KAZ" & year > 1936 & year <= 1991 ~ 1,
    country_text_id == "KGZ" & year > 1936 & year <= 1991 ~ 1,
    country_text_id == "ROU" & year > 1944 & year <= 1989 ~ 1,
    country_text_id == "MDA" & year > 1940 & year <= 1991 ~ 1,
    country_text_id == "ALB" & year > 1944 & year <= 1992 ~ 1,
    country_text_id == "BGR" & year > 1944 & year <= 1990 ~ 1,
    country_text_id == "RUS" & year > 1918 & year <= 1991 ~ 1,
    country_text_id == "PRK" & year > 1945 & year <= 2009 ~ 1, # Comment out for PRK communist today
    country_text_id == "YMD" & year > 1967 & year <= 1990 ~ 1,
    country_text_id == "SDN" & year > 1969 & year <= 1985 ~ 1,
    country_text_id == "SOM" & year > 1969 & year <= 1991 ~ 1,
    country_text_id == "COG" & year > 1969 & year <= 1992 ~ 1,
    country_text_id == "ETH" & year > 1974 & year <= 1991 ~ 1,
    country_text_id == "MOZ" & year > 1975 & year <= 1990 ~ 1,
    country_text_id == "AGO" & year > 1975 & year <= 1992 ~ 1,
    country_text_id == "BEN" & year > 1975 & year <= 1990 ~ 1,
    country_text_id == "KHM" & year > 1976 & year <= 1992 ~ 1,
    country_text_id == "BFA" & year > 1984 & year <= 1987 ~ 1,
    country_text_id == "SYC" & year > 1979 & year <= 1991 ~ 1,
    TRUE ~ 0
  )
  ) %>% 
  arrange(., country_text_id)

mydata4 <- read_csv("~/Documents/Data/businessopen.csv", skip = 3) %>% 
  mutate(., country_text_id = `Country Code`,
         country_name = `Country Name`) %>% 
  select(., -c("Indicator Name", "Indicator Code", "Country Name", "Country Code", "country_name")) %>% 
  pivot_longer(cols = 1:63,
               names_to = "year",
               values_to = "businessdays") %>% 
  mutate(., year = as.numeric(year)) %>% 
  filter(., country_text_id %in% mydata1$country_text_id) %>% 
  na.omit()

mydata5 <- read_csv("~/Documents/Data/aidpercgni.csv", skip = 3) %>% 
  mutate(., country_text_id = `Country Code`,
         country_name = `Country Name`) %>% 
  select(., -c("Indicator Name", "Indicator Code", "Country Name", "Country Code", "country_name")) %>% 
  pivot_longer(cols = 1:63,
               names_to = "year",
               values_to = "aidgni") %>% 
  mutate(., year = as.numeric(year)) %>% 
  filter(., country_text_id %in% mydata1$country_text_id) %>% 
  na.omit()

mydata6 <- read_csv("~/Documents/Data/natresrents.csv", skip = 4) %>% 
  mutate(., country_text_id = `Country Code`,
         country_name = `Country Name`) %>% 
  select(., -c("Indicator Name", "Indicator Code", "Country Name", "Country Code", "country_name")) %>% 
  pivot_longer(cols = 1:63,
               names_to = "year",
               values_to = "naturalresourcerents") %>% 
  mutate(., year = as.numeric(year), # Imports as character
         naturalresourcerents = naturalresourcerents/100) %>% # These are coded as percentages, decimalising
  filter(., country_text_id %in% mydata1$country_text_id) %>% 
  na.omit()

# Merge as necessary ----
df1 <- list(mydata1, mydata2, mydata3, mydata4, mydata5, mydata6) %>% 
  reduce(left_join, by = c("country_text_id", "year")) %>% 
  distinct(., country_text_id, year, .keep_all = TRUE) # Eliminate duplicates if there are any

# Save the master data to a file so as to be able to provide a replication dataset.
setwd("~/GitHub/masterspaper")
write_csv(df1, "kaksoy_masterpaperdata.csv")

# Write a function to reverse the scales of some unintuitively coded variables.
reversr <- function (x, na.rm = T) {
  min(x, na.rm = T) - x + max(x, na.rm = T)
}

# Create models 1 to 4 ----
lm1 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy),
                 data = df1,
                 clusters = country_text_id)
lm2 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf,
                 data = df1,
                 clusters = country_text_id)

lm3 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc),
                 data = df1,
                 clusters = country_text_id)

lm4 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy,
                 data = df1,
                 clusters = country_text_id)

# Report models 1 to 4 ----
modelsummary(list(lm1, lm2, lm3, lm4),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
                          'aidgni' = 'Aid as % of GNI',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             notes = list('Standard errors clustered by country.'),
             title = "Models 1-4") %>%
  add_header_above(c(" " = 1, "Low-level bribery" = 4)) %>% 
  kable_styling(bootstrap_options = "condensed", latex_options = "HOLD_position") # Only for knitting

# Create models 5 through 7 ----
lm5 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy + relevel(region, ref = 5),
                 data = df1,
                 subset = (democracy == 0),
                 clusters = country_text_id)

lm6 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy + relevel(region, ref = 5) +
                   marx, 
                 data = df1,
                 subset = (democracy == 0),
                 clusters = country_text_id)
lm7 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy + relevel(region, ref = 5) + 
                   marx + aidgni + naturalresourcerents, 
                 data = df1,
                 subset = democracy == 0,
                 clusters = country_text_id)

# Report models 5 through 7 ----
modelsummary(list(lm5, lm6, lm7),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
                          'marx' = 'Communist',
                          'aidgni' = 'Aid as % of GNI',
                          'naturalresourcerents' = 'Natural resource rents as % of GDP',
                          'relevel(region, ref = 5)1' = 'Eastern Europe and post-Soviet',
                          'relevel(region, ref = 5)2' = 'Latin America',
                          'relevel(region, ref = 5)3' = 'North Africa and Middle East',
                          'relevel(region, ref = 5)4' = 'Sub-Saharan Africa',
                          'relevel(region, ref = 5)6' = 'Eastern Asia',
                          'relevel(region, ref = 5)7' = 'Southeastern Asia',
                          'relevel(region, ref = 5)8' = 'Southern Asia',
                          'relevel(region, ref = 5)9' = 'Pacific',
                          'relevel(region, ref = 5)10' = 'Caribbean',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             notes = list('Standard errors clustered by country.'),
             title = "Extended models with regional controls") %>%
  add_header_above(c(" " = 1, "Low-level bribery" = 3)) %>% 
  kable_styling(bootstrap_options = "condensed", latex_options = "HOLD_position") %>% 
  pack_rows("Region", start_row = 13, end_row = 29, bold = FALSE)

# Change dependent variable to time it takes to open a business ----
lm8 <- lm_robust(businessdays ~ reversr(v2x_clphy),
                 data = df1,
                 subset = (democracy == 0),
                 clusters = country_text_id)

lm9 <- lm_robust(businessdays ~ reversr(v2x_clphy) + v2exl_legitperf,
                  data = df1,
                  subset = (democracy == 0),
                  clusters = country_text_id)

lm10 <- lm_robust(businessdays ~ reversr(v2x_clphy) + v2exl_legitperf +
                    log(e_gdppc),
                  data = df1,
                  subset = (democracy == 0),
                  clusters = country_text_id)

lm11 <- lm_robust(businessdays ~ reversr(v2x_clphy) + v2exl_legitperf +
                    log(e_gdppc) + v2x_polyarchy,
                  data = df1,
                  subset = (democracy == 0),
                  clusters = country_text_id)
lm12 <- lm_robust(businessdays ~ reversr(v2x_clphy) + v2exl_legitperf +
                    log(e_gdppc) + v2x_polyarchy + aidgni + naturalresourcerents,
                  data = df1,
                  subset = (democracy == 0),
                  clusters = country_text_id)

# Report the models with business days ----
modelsummary(list(lm8, lm9, lm10, lm11, lm12),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
                          'aidgni' = 'Aid as % of GNI',
                          'naturalresourcerents' = 'Natural resource rents as % of GDP',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             notes = list('Standard errors clustered by country.'),
             title = "Basic models with alternative dependent variable") %>%
  add_header_above(c(" " = 1, "Time to open business (days)" = 5)) %>% 
  kable_styling(bootstrap_options = "condensed", latex_options = "HOLD_position")

# The boxplot clearly shows to us that there really isn't a difference between
# states which have had mass killings in the last three years and those which
# have not, at least in how Treisman and Guriev (2019) have implemented the
# measure.

# # Load the DPI data ----
# dpi <- read_dta("~/Documents/Data/DPI2015/DPI2015.dta") %>% 
#   naniar::replace_with_na(replace = list(liec = c(-999),
#                                          eiec = c(-999))) %>% 
#   filter(., liec <= 5 &
#            ifs != 0) %>%  # Non-democracies. Can also be tried with "7" for dominant parties.
#   mutate(., country_text_id = ifs) %>% 
#   select(., c("country_text_id", "year", "liec", "eiec"))
lm4_1 <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
              log(e_gdppc) + v2x_polyarchy,
            data = df1)

ggplot(lm4_1, aes(x = .fitted,
                  y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted",
       y = "Residual",
       title = "Non-clustered S.E.")

tmp <- tibble(XS1 = reversr(df1$v2excrptps))
tmp$residuals <- (lm4$fitted.values - tmp$XS1)

ggplot(tmp, aes(x = XS1,
                y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted",
       y = "Residual",
       title = "Clustered S.E.")
