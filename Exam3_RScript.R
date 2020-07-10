#clear environment
rm(list = ls(all=TRUE))

# load tidycensus 
library(tidycensus)
census_api_key("0979f51018f9aad11498289530215de0e9dd4c4f", install = TRUE,
               overwrite = TRUE)

gini_2010 <- load_variables(year = 2010, "acs5")
gini_2015 <- load_variables(year = 2015, "acs5")

View(gini_2010)
View(gini_2015)

# filter gini inequality
gini_2010 <- get_acs(geography = "county", 
                 variables = c(gini = c("B19083_001")), 
                 year = 2010)
gini_2015 <- get_acs(geography = "county", 
                     variables = c(gini = c("B19083_001")), 
                     year = 2015)

# add years
gini_2010$year = 2010
gini_2015$year = 2015

# combine the datasets into a panel data
library(data.table)
library(dplyr)
inequality_panel = left_join(x = gini_2010,
                          y = gini_2015,
                          by = "NAME")

# rename NAME variable to `state`
setnames(inequality_panel, "NAME", "state")

# rename other variables too, make the irrelevant ones null
inequality_panel <- subset(inequality_panel, select = -c(variable.y))
inequality_panel <- subset(inequality_panel, select = -c(GEOID.y))
inequality_panel$year.x <- NULL
inequality_panel$year.y <- NULL

# rename variables to specify the year
setnames(inequality_panel, "moe.x", "moe 2010")
setnames(inequality_panel, "moe.y", "moe 2015")
setnames(inequality_panel, "variable.x", "variable")
setnames(inequality_panel, "estimate.x", "estimate 2010")
setnames(inequality_panel, "estimate.y", "estimate 2015")
setnames(inequality_panel, "GEOID.x", "GEOID")




# reshape panel to wide format
library(tidyverse)
inequality_wide <-
  inequality_panel %>%
  pivot_wider(id_cols = c("state", "GEOID", "estimate 2010", "estimate 2015"),
              names_from = "state",
              values_from = c("estimate 2010", "estimate 2015"))



# reshape panel to long format
inequality_long <- # reshape panel to wide format
  inequality_wide %>%
  pivot_longer(cols = starts_with('estimate'),
               names_to = c("state", "GEOID"),
               names_sep = " ",
               values_to = c("estimate 2010", "estimate 2015"),
               values_drop_na = TRUE)

# NUMER 5

# collapse the data
inequality_collapsed <- inequality_long %>%
  group_by(country_code, year, country_name) %>%
  summarize(across(where(is.numeric), sum)) %>%
  select(-c("transaction_id"))




# WDI #8 and on

library(WDI)
gdp_current = WDI(country = "all", indicator = "NY.GDP.MKTP.CD",
                    start = 2006, end = 2007, extra = FALSE, cache = NULL)

#deflate data with USD 2015
deflator_data = WDI(country = "all", indicator = "NY.GDP.DEFL.ZS",
                    start = 2006, end = 2007, extra = FALSE, cache = NULL)

# rename deflator variable
library(data.table)
setnames(deflator_data, "NY.GDP.DEFL.ZS", "deflator")

#subset to get data frame with only USD, remove deflator table
usd_deflator <- subset(deflator_data, country == "United States")
rm(deflator_data)

#drop unnecessary variables
usd_deflator$iso2c <- NULL
usd_deflator$country <- NULL

#merge to match years + deflation rates by USD
gdp_deflated = left_join(x = gdp_current,
                          y = usd_deflator,
                          by = "year")
setnames(gdp_deflated, "NY.GDP.MKTP.CD", "GDP")

# ACTUALLY FUNCTION OF DEFLATING IT
gdp_deflated$deflated_amount = gdp_deflated$GDP/
  (gdp_deflated$deflator/100)