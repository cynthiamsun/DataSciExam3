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

# rename other variables too
inequality_panel <- subset(inequality_panel, select = -c(variable.y))
inequality_panel <- subset(inequality_panel, select = -c(GEOID.y))

# rename variables to specify the year
setnames(inequality_panel, "moe.x", "moe 2010")
