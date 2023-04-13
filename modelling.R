# Load libraries ----
library(tidyverse)
library(haven)
library(estimatr)     # To cluster standard errors
library(modelsummary) # For neat reporting tables
library(kableExtra)   # For additional table formatting
library(countrycode)  # Converting country coding schemes

# Load data ----
mydata1 <- read_csv("~/Documents/Data/V_Dem_v13.csv") %>% 
  mutate(., region = as_factor(e_regionpol)) %>% # Recoding regions as factors
  filter(., v2x_polyarchy <= 0.42) # Filtering non-democratic countries

mydata2 <- read_delim("~/Documents/Data/masskillings.txt") %>% 
  mutate(., masskilling = as_factor(lag(ongoing, n = 3)),
         country_name = country,
         country_text_id = sftgcode) %>% 
  select(., c("country_text_id", "year", "masskilling")) %>% 
  na.omit(.)

mydata3 <- read_dta("~/Documents/Data/marx.dta") %>% 
  drop_na(., cowcode) %>% 
  mutate(., country_text_id = countrycode(cowcode, origin = 'cown', destination = 'cowc')) %>% 
  select(., c(country_text_id, year, marx))

df1 <- list(mydata1, mydata2, mydata3) %>% 
  reduce(left_join, by = c("country_text_id", "year")) %>% 
  distinct(., country_text_id, year, .keep_all = TRUE) # Eliminate duplicates if there are any

# Write a function to reverse the scales of some unintuitively coded variables.
reversr <- function (x, na.rm = T) {
  min(x, na.rm = T) - x + max(x, na.rm = T)
}

# Create models ----
lm1 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy),
                 data = df1,
                 clusters = country_id)

lm2 <- lm_robust(reversr(v2excrptps) ~ v2exl_legitperf,
                 data = df1,
                 clusters = country_id)

lm3 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf,
                 data = df1,
                 clusters = country_id)

lm4 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc),
                 data = df1,
                 clusters = country_id)

lm5 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy,
                 data = df1,
                 clusters = country_id)

lm6 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy + relevel(region, ref = 5),
                 data = df1,
                 clusters = country_id)

lm7 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy + relevel(region, ref = 5) +
                   marx, 
                 data = df1,
                 clusters = country_id)

# Report the models ----
modelsummary(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
                          'relevel(region, ref = 5)1' = 'Region: Eastern Europe and Post-Soviet Union',
                          'relevel(region, ref = 5)2' = 'Region: Latin America',
                          'relevel(region, ref = 5)3' = 'Region: North Africa and Middle East',
                          'relevel(region, ref = 5)4' = 'Region: Sub-Saharan Africa',
                          'relevel(region, ref = 5)6' = 'Region: Eastern Asia',
                          'relevel(region, ref = 5)7' = 'Region: Southeastern Asia',
                          'relevel(region, ref = 5)8' = 'Region: South Asia',
                          'relevel(region, ref = 5)9' = 'Region: Pacific',
                          'relevel(region, ref = 5)10' = 'Region: Caribbean',
                          'marx' = 'Communist',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             notes = list('Standard errors clustered by country.')
             ) %>%
  add_header_above(c(" " = 1,
                     "Low-level bribery" = 7))

# Add some visualisation ----
ggplot(data = df1,
       mapping = aes(x = reversr(v2x_clphy),
                     y = reversr(v2excrptps))) +
  geom_point(alpha = .05) +
  geom_smooth(method = 'lm') +
  labs(title = "Relationship between physical violence and low-level corruption",
       x = "Physical violence index",
       y = "Low-level corruption") +
  theme_linedraw()

# Try something new ----
masskilling <- read_delim("~/Documents/Data/masskillings.txt") %>% 
  mutate(., masskilling = as_factor(lag(ongoing, n = 3)),
         country_name = country,
         country_text_id = sftgcode) %>% 
  select(., c("country_text_id", "year", "masskilling")) %>% 
  na.omit(.)

df1 <- list(df1, masskilling) %>% 
  reduce(left_join, by = c("country_text_id", "year"))

lm8 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy + masskilling,
                 data = df1,
                 clusters = country_id)

lm9 <- lm_robust(reversr(v2excrptps) ~ v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy + masskilling,
                 data = df1,
                 clusters = country_id)

# Report the new model ----
modelsummary(list(lm1, lm2, lm3, lm4, lm5, lm6, lm7),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
                          'masskilling1' = 'Mass killing (3 years)',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             notes = list('Standard errors clustered by country.')) %>%
  add_header_above(c(" " = 1,
                     "Low-level bribery" = 7))

# So, the standard errors skyrocket when we include the masskillings, presumably
# because we decrease the number of observations. Perhaps trying a boxplot might
# give us a picture of some kind.

df1 %>% 
  select(., c("v2excrptps", "masskilling")) %>% 
  na.omit(.) %>% 
  ggplot(.,
         aes(x = masskilling,
             y = reversr(v2excrptps))) +
  geom_boxplot() +
  scale_x_discrete(breaks = c("1", "0"),
                   labels = c("Yes", "No")) +
  labs(title = "Difference between mass killing countries and non-mass killing autocracies",
       x = "Mass killing within last three years?",
       y = "Low-level corruption") +
  theme_linedraw()

# The boxplot clearly shows to us that there really isn't a difference between
# states which have had mass killings in the last three years and those which
# have not, at least in how Treisman and Guriev (2019) have implemented the
# measure.

# Load the DPI data ----
dpi <- read_dta("~/Documents/Data/DPI2015/DPI2015.dta") %>% 
  naniar::replace_with_na(replace = list(liec = c(-999),
                                         eiec = c(-999))) %>% 
  filter(., liec <= 5 &
           ifs != 0) %>%  # Non-democracies. Can also be tried with "7" for dominant parties.
  mutate(., country_text_id = ifs) %>% 
  select(., c("country_text_id", "year", "liec", "eiec"))

df2 <- list(dpi, df1) %>% 
  reduce(left_join, by = c("country_text_id", "year"))

# Now try it with the DPI data.

lm8 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy),
                 data = df2,
                 clusters = country_id)

lm9 <- lm_robust(reversr(v2excrptps) ~ v2exl_legitperf,
                 data = df2,
                 clusters = country_id)

lm10 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf,
                 data = df2,
                 clusters = country_id)

lm11 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc),
                 data = df2,
                 clusters = country_id)

lm12 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + liec,
                 data = df2,
                 clusters = country_id)

lm13 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                    log(e_gdppc) + liec + eiec,
                  data = df2,
                  clusters = country_id)

lm14 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                    log(e_gdppc) + liec + eiec + masskilling,
                  data = df2,
                  clusters = country_id)

# Report them.

modelsummary(list(lm7, lm8, lm9, lm10, lm11, lm12, lm13),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'liec' = 'Legislative electoral competitiveness',
                          'eiec' = 'Executive electoral competitiveness',
                          'masskilling1' = 'Mass killing',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             notes = list('Standard errors clustered by country.')) %>% 
  add_header_above(c(" " = 1,
                   "Low-level bribery" = 7))

# Try using another proxy for corruption, such as the time it takes to open a business in days.
businesstime <- read_csv("~/Documents/Data/businessopen.csv", skip = 3) %>% 
  mutate(., country_text_id = `Country Code`,
         country_name = `Country Name`) %>% 
  select(., -c("Indicator Name", "Indicator Code", "Country Name", "Country Code")) %>% 
  pivot_longer(cols = 1:63,
               names_to = "year",
               values_to = "businessdays") %>% 
  mutate(., year = as.numeric(year)) %>% 
  filter(., country_text_id %in% df1$country_text_id) %>% 
  na.omit()

df3 <- list(businesstime, df1) %>% 
  reduce(left_join, by = c("country_text_id", "year"))

# More models with this new dependent variable.

lm15 <- lm_robust(businessdays ~ reversr(v2x_clphy),
                 data = df3,
                 clusters = country_id)

lm16 <- lm_robust(businessdays ~ v2exl_legitperf,
                 data = df3,
                 clusters = country_id)

lm17 <- lm_robust(businessdays ~ reversr(v2x_clphy) + v2exl_legitperf,
                 data = df3,
                 clusters = country_id)

lm18 <- lm_robust(businessdays ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc),
                 data = df3,
                 clusters = country_id)

lm19 <- lm_robust(businessdays ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy,
                 data = df3,
                 clusters = country_id)

modelsummary(list(lm15, lm16, lm17, lm18, lm19),
             output = 'kableExtra',
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_map = c('reversr(v2x_clphy)' = 'Physical violence index',
                          'v2exl_legitperf' = 'Performance legitimation',
                          'log(e_gdppc)' = 'Logged GDP per capita',
                          'v2x_polyarchy' = 'Electoral democracy index',
                          '(Intercept)' = 'Intercept'),
             gof_omit = 'BIC|Log.Lik.|RMSE|Std.Errors',
             notes = list('Standard errors clustered by country.')) %>% 
  add_header_above(c(" " = 1,
                     "Time taken to open business (days)" = 5))
