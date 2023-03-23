# Load libraries ----
library(tidyverse)
library(haven)
library(estimatr) # To cluster standard errors
library(modelsummary) # For neat reporting tables

# Load data ----
mydata1 <- read_csv("~/Documents/Data/V_Dem_v12.csv") %>% 
  filter(., v2x_polyarchy <= 0.42)

# Write a function to reverse the scales of some unintuitively coded variables
reversr <- function (x, na.rm = T) {
  min(x, na.rm = T) - x + max(x, na.rm = T)
}

# Create models ----
lm1 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy),
                 data = mydata1,
                 clusters = country_id)

lm2 <- lm_robust(reversr(v2excrptps) ~ v2exl_legitperf,
                 data = mydata1,
                 clusters = country_id)

lm3 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf,
                 data = mydata1,
                 clusters = country_id)

lm4 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc),
                 data = mydata1,
                 clusters = country_id)

lm5 <- lm_robust(reversr(v2excrptps) ~ reversr(v2x_clphy) + v2exl_legitperf +
                   log(e_gdppc) + v2x_polyarchy,
                 data = mydata1,
                 clusters = country_id)

# Report the models ----

modelsummary(list(lm1, lm2, lm3, lm4, lm5),
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
                     "Low-level bribery" = 5))

# Add some visualisation ----

ggplot(data = mydata1,
       mapping = aes(x = reversr(v2x_clphy),
                     y = reversr(v2excrptps))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_linedraw()
