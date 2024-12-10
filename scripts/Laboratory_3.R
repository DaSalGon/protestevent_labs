# Data Analysis R Lab 3, 25.11.2024
# Saldivia Gonzatti and Hutter

# Set-up #######################################################################

options(scipen = 999) # Turn-off scientific numeric notation.

library(dplyr)      # Upload R package to handle and manage data.
library(ggplot2)    # Upload R package to create plots.
library(tidyverse)  # Upload R package to handle and manage data.

# Load the data ################################################################

# Download the SCAD Latin America data originally from here:
  # https://www.strausscenter.org/ccaps-research-areas/social-conflict/database/

# Or download it from the Laboratory 2 folder in the course's GitHub repository.

scad <- read.csv("../Lab 2/SCAD2018LatinAmerica_Final.csv") # Upload the data

# Let's use the data by the Mass Mobillization Data project (MMALL; https://massmobilization.github.io) 
  # to compare the protest data from SCAD with another source. The data is available here:
  # https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HTTWYL

mmall <- read.csv("mmALL_073120_csv.csv") # Upload the data


# EXTRA:
 # Now, let's load the ACLED data set; download the data from the Lab 3 folder
  # in the course's GitHub repository.
# acled <- read.csv("LatinAmerica_2018-2024_Nov15.csv") # Upload the data


# Re-code data #################################################################


# Let's have a cleaner version of the protest forms:

scad <- scad %>%
  mutate(protest_form = case_when(
    etype == 1 ~ "Organized Demonstration",
    etype == 2 ~ "Spontaneous Demonstration",
    etype == 3 ~ "Organized Violent Riot",
    etype == 4 ~ "Spontaneous Violent Riot",
    etype == 5 ~ "General Strike",
    etype == 6 ~ "Limited Strike",
    etype == 7 ~ "Pro-Government Violence (Repression)",
    etype == 8 ~ "Anti-Government Violence",
    etype == 9 ~ "Extra-government Violence",
    etype == 10 ~ "Intra-government Violence",
    TRUE ~ "Unknown"  # This will catch any unmatched cases
  ))

scad <- scad %>% 
  dplyr::filter(! protest_form %in%
                  c("Pro-Government Violence (Repression)",
                    "Extra-government Violence",
                    "Intra-government Violence"))

scad_aggr <- scad %>% group_by(countryname, eyr) %>%
  summarise(events_scad = n()) %>% ungroup()


mmall_aggr <- mmall %>% 
  dplyr::filter(protest!=0) %>% 
  group_by(country, year) %>%
  summarise(events_mmal = n()) %>% ungroup()


# Which countries are in both data sets? #######################################

table(scad$countryname %in% mmall$country)
table(scad$countryname[!scad$countryname %in% mmall$country])

# Merge the data ###############################################################

data <- scad_aggr %>% left_join(mmall_aggr, 
                                by = c("countryname" = "country", "eyr" = "year"))
data <- data %>% dplyr::filter(!countryname=="Trinidad and Tobago")

rm(mmall_aggr, mmall, scad_aggr, scad) # Clean


# Analysis #####################################################################


# Are both data sets related?

cor.test(data$events_mmal, data$events_scad)

lm(data = data, events_scad ~ events_mmal) %>%
  summary()

lm(data = data, events_scad ~ events_mmal + countryname) %>%
  summary()


# Visualize the number of events by country and source

data_longer <- data %>% 
  pivot_longer(cols = c(events_scad, events_mmal), names_to = "source", values_to = "events")

ggplot(data_longer, aes(x = eyr, y = events, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~countryname) +
  theme_bw()



