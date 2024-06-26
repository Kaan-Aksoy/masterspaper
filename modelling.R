# Load libraries ----
library(tidyverse)
library(haven)
library(estimatr)     # To cluster standard errors
library(modelsummary) # For neat reporting tables
library(kableExtra)   # For additional table formatting
library(countrycode)  # Converting country coding schemes
library(sandwich)     # Newey-West standard errors
library(lmtest)       # To test for autocorrelation and heteroskedasticity

# Load and clean data ----
mydata1 <- read_csv("~/Documents/Data/V_Dem_v13.csv") %>% 
  mutate(., region = as_factor(e_regionpol)) %>% # Recoding regions as factors
  select(., c("country_name", "country_text_id", "year", "v2x_polyarchy", "v2x_clphy",
              "v2excrptps", "v2exbribe", "v2exembez", "v2exthftps", "v2exl_legitperf",
              "e_gdppc", "e_total_resources_income_pc", "region")) %>% 
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

mydata5 <- read_csv("~/Documents/Data/natresrents.csv", skip = 4) %>% 
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
df1 <- list(mydata1, mydata2, mydata3, mydata4, mydata5) %>% 
  reduce(left_join, by = c("country_text_id", "year")) %>% 
  distinct(., country_text_id, year, .keep_all = TRUE) # Eliminate duplicates if there are any

# Save the master data to a file so as to be able to provide a replication dataset.
setwd("~/GitHub/masterspaper")
write_csv(df1, "kaksoy_masterpaperdata.csv")

# Load cleaned data ----
# Load the new data file to ensure the analyses can run independently from the piecemeal data.
rm(list = ls())
df1 <- read_csv("~/GitHub/masterspaper/kaksoy_masterpaperdata.csv") %>% 
  mutate(., region = as_factor(region), # Doesn't import as factor, so this step is necessary.
         democracy = as_factor(democracy),
         marx = as_factor(marx),
         masskilling = as_factor(masskilling)) 

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
                   marx + naturalresourcerents, 
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
                    log(e_gdppc) + v2x_polyarchy + naturalresourcerents,
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


# Figure 1, boxplot among regions ----
df1 %>% 
  filter(., democracy == 0) %>% 
  ggplot(., aes(x = region,
                y = reversr(v2x_clphy))) +
  geom_boxplot() +
  labs(x = "Geopolitical region",
       y = "Physical violence index") +
  scale_x_discrete(labels = c('Eastern Europe and post-Soviet',
                              'Latin America',
                              'North Africa and Middle East',
                              'Sub-Saharan Africa',
                              'Western Europe and North America',
                              'Eastern Asia',
                              'Southeastern Asia',
                              'Southern Asia',
                              'Pacific',
                              'Caribbean')) +
  coord_flip() +
  theme_bw()

# Figure 2, linear model ----
df1 %>% 
  filter(., democracy == 0) %>% 
  ggplot(., aes(x = reversr(v2x_clphy),
                y = reversr(v2excrptps))) +
  geom_point(alpha = 0.01) +
  geom_smooth(col = "firebrick", method = 'lm') +
  labs(x = "Physical violence index",
       y = "Low-level bribery") +
  theme_bw()

# Figure 3, business days model ----
df1 %>% 
  filter(., democracy == 0) %>% 
  ggplot(., aes(x = reversr(v2x_clphy),
                y = businessdays)) +
  geom_point(alpha = 0.50) +
  geom_smooth(col = "firebrick", method = 'lm') +
  labs(x = "Physical violence index",
       y = "Time needed to open business (days)") +
  theme_bw()

# Appendix A, correlation matrix ----
df1 %>% 
  select(., c("v2x_polyarchy", "v2x_clphy", "v2excrptps", "v2exl_legitperf")) %>% 
  mutate(., `Electoral democracy index` = `v2x_polyarchy`,
         `Physical violence index` = `v2x_clphy`,
         `Low-level bribery` = `v2excrptps`,
         `Performance legitimation` = `v2exl_legitperf`) %>% 
  select(., -c("v2x_polyarchy", "v2x_clphy", "v2excrptps", "v2exl_legitperf")) %>% 
  datasummary_correlation(output = "kableExtra") %>% 
  kable_styling(bootstrap_options = "condensed", latex_options = c("HOLD_position", "scale_down"),
                full_width = FALSE)

# Appendix B, mass killing model ----
lm13 <- lm_robust(reversr(v2excrptps) ~ masskilling + v2exl_legitperf +
                    log(e_gdppc) + v2x_polyarchy + relevel(region, ref = 5),
                  data = df1,
                  subset = (democracy == 0),
                  clusters = country_text_id)

# Reporting model.
modelsummary(lm13,
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('masskilling1' = 'Mass killing',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
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
             title = "Model using mass killings proxy measure for political violence") %>%
  add_header_above(c(" " = 1, "Low-level bribery" = 1)) %>% 
  kable_styling(bootstrap_options = "condensed", latex_options = "HOLD_position") %>% 
  pack_rows("Region", start_row = 9, end_row = 25, bold = FALSE)

# Appendix C, Newey-West (heteroskedasticity and autocorrelation consistent, HAC) errors ----
lmA <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy),
          subset = (democracy == 0),
          data = df1)

lmB <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf,
          subset = (democracy == 0),
          data = df1)

lmC <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
            log(e_gdppc),
          subset = (democracy == 0),
          data = df1)

lmD <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
            log(e_gdppc) + v2x_polyarchy,
          subset = (democracy == 0),
          data = df1)

modelplot(list("Model 1" = lmA, "Model 2" = lmB,
               "Model 3" = lmC, "Model 4" = lmD),
          coef_omit = "Intercept",
          coef_map = c('v2exl_legitperf' = 'Performance legitimation',
                       'log(e_gdppc)' = 'Logged GDP per capita',
                       'v2x_polyarchy' = 'Electoral democracy index',
                       'reversr(v2x_clphy)' = 'Physical violence index'),
          conf_level = .99) +
  scale_colour_brewer(palette = "Dark2")

# Reporting models.
modelsummary(list(lmA, lmB, lmC, lmD),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             vcov = sandwich::NeweyWest,
             title = "Basic models with Newey-West standard errors") %>% 
  add_header_above(c(" " = 1, "Low-level bribery" = 4)) %>% 
  kable_styling(bootstrap_options = "condensed", latex_options = "HOLD_position")


## Models with all dimensions of corruption. ----
lm_alt1 <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy),
              data = df1,
              subset = democracy == 0)

lm_alt2 <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf,
              data = df1,
              subset = democracy == 0)

lm_alt3 <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc),
              data = df1,
              subset = democracy == 0)

lm_alt4 <- lm(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy,
              data = df1,
              subset = democracy == 0)

lm_alt5 <- lm(reversr(v2exbribe) ~ reversr(v2x_clphy),
              data = df1,
              subset = democracy == 0)

lm_alt6 <- lm(reversr(v2exbribe) ~ reversr(v2x_clphy) + v2exl_legitperf,
              data = df1,
              subset = democracy == 0)

lm_alt7 <- lm(reversr(v2exbribe) ~ reversr(v2x_clphy) + v2exl_legitperf +
                log(e_gdppc),
              data = df1,
              subset = democracy == 0)

lm_alt8 <- lm(reversr(v2exbribe) ~ reversr(v2x_clphy) + v2exl_legitperf +
                log(e_gdppc) + v2x_polyarchy,
              data = df1,
              subset = democracy == 0)

lm_alt9 <- lm(reversr(v2exembez) ~ reversr(v2x_clphy),
              data = df1,
              subset = democracy == 0)

lm_alt10 <- lm(reversr(v2exembez) ~ reversr(v2x_clphy) + v2exl_legitperf,
              data = df1,
              subset = democracy == 0)

lm_alt11 <- lm(reversr(v2exembez) ~ reversr(v2x_clphy) + v2exl_legitperf +
                log(e_gdppc),
              data = df1,
              subset = democracy == 0)

lm_alt12 <- lm(reversr(v2exembez) ~ reversr(v2x_clphy) + v2exl_legitperf +
                log(e_gdppc) + v2x_polyarchy,
              data = df1,
              subset = democracy == 0)

lm_alt13 <- lm(reversr(v2exthftps) ~ reversr(v2x_clphy),
              data = df1,
              subset = democracy == 0)

lm_alt14 <- lm(reversr(v2exthftps) ~ reversr(v2x_clphy) + v2exl_legitperf,
               data = df1,
               subset = democracy == 0)

lm_alt15 <- lm(reversr(v2exthftps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                 log(e_gdppc),
               data = df1,
               subset = democracy == 0)

lm_alt16 <- lm(reversr(v2exthftps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                 log(e_gdppc) + v2x_polyarchy,
               data = df1,
               subset = democracy == 0)

# Report models with Newey-West standard errors (HAC) ----
modelsummary(list(lm_alt1, lm_alt2, lm_alt3, lm_alt4,
                  lm_alt5, lm_alt6, lm_alt7, lm_alt8,
                  lm_alt9, lm_alt10, lm_alt11, lm_alt12,
                  lm_alt13, lm_alt14, lm_alt15, lm_alt16),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             vcov = sandwich::NeweyWest,
             title = "Corruption and political violence",
             notes = list("Newey-West standard errors")) %>% 
  add_header_above(c(" " = 1, "Low-level bribery" = 4, "High-level bribery" = 4,
                     "High-level theft" = 4, "Low-level theft" = 4)) %>% 
  kable_styling(bootstrap_options = "condensed", latex_options = "HOLD_position")