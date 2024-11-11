# Data Analysis R Lab 2, 11.11.2024
# Saldivia Gonzatti and Hutter

# Set-up #######################################################################

options(scipen = 999) # Turn-off scientific numeric notation.

library(dplyr)      # Upload R package to handle and manage data.
library(ggplot2)    # Upload R package to create plots.
library(lme4)       # Upload R package to estimate linear mixed-effects models. 

# Load the data ################################################################

# Download the SCAD Latin America data originally from here:
  # https://www.strausscenter.org/ccaps-research-areas/social-conflict/database/

# Or download it from the Laboratory 2 folder in the course's GitHub repository.

scad <- read.csv("SCAD2018LatinAmerica_Final.csv") # Upload the data

# Check the codebook here!

# Link: https://www.strausscenter.org/wp-content/uploads/SCAD_33_Codebook.pdf

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

# Let's reduce and abstract specific protest forms to three overarching categories:

scad <- scad %>% 
  mutate(radical_action = case_when(
    etype %in% c(3, 4, 7, 8, 9, 10) ~ "violent",
    etype %in% c(1, 2) ~ "demonstration",
    etype %in% c(5, 6) ~ "strike",
    TRUE ~ "unknown"
  ))


# Visualize the protest form distribution across countries #####################

# Plot the frequency of the different protest forms by country 
# in one code batch:

scad %>% 
  ggplot(aes(x = radical_action)) +
  geom_bar(stat="count") +
  theme_bw() +
  xlab("Country") + ylab("Number of events") +
  facet_wrap(~countryname, scales = "free_y") +
  ggtitle("Protest forms by country") 

# Get democracy data to merge with protest data ################################
  
# Note: The V-Dem data is available in the vdemdata package. However, you  need
  # the devtools package to install it from GitHub. To make life easier, I pre-
  # pared the data in the Github folder for you. If you wish to download the data
  # directly using the vdemdata package and filtering and transforming it, you can
  # un-comment the following code and run it in your R console.

# Check the V-DEM project, the data and codebooks here!
# Link: https://v-dem.net/


  # install.packages("devtools")
  # library(devtools)
  # devtools::install_github("vdeminstitute/vdemdata")  
  # library(vdemdata)
  # vdem_data <- vdemdata::vdem %>% 
  #  dplyr::filter(country_name %in% c("Nicaragua", "Costa Rica", "Haiti", "Cuba",
  #                                  "Dominican Republic", "Guatemala",
  #                                 "El Salvador", "Jamaica", "Mexico",
  #                                "Honduras", "Panama", "Trinidad and Tobago")) %>% 
  # dplyr::select(year, country_name, v2x_polyarchy) %>%  # Select the variables of interest
  # dplyr::rename(countryname = country_name) # Rename the country_name variable
  # save(vdem_data, file = "vdem_data_selection.RData") # Save the data for future use
  

load("vdem_data_selection.RData") # Load the data
  
# Code the population data of each country to benchmark the number of events based
  # on numbers for the year 2000 - a harmonisation approach:
  
  vdem_data <- vdem_data %>%
    mutate(population_2000 = case_when(
      countryname == "Nicaragua" ~ 5000000,
      countryname == "Costa Rica" ~ 3700000,
      countryname == "Haiti" ~ 8000000,
      countryname == "Cuba" ~ 11200000,
      countryname == "Dominican Republic" ~ 8000000,
      countryname == "Guatemala" ~ 12000000,
      countryname == "El Salvador" ~ 6000000,
      countryname == "Jamaica" ~ 2700000,
      countryname == "Mexico" ~ 97000000,
      countryname == "Honduras" ~ 7000000,
      countryname == "Panama" ~ 3000000,
      countryname == "Trinidad and Tobago" ~ 1300000,
      TRUE ~ NA_real_  # Default case
    ))
  
# Filter the demcracy data from V-DEM to the years we have protest data for from
  # the SCAD Latin America data... but include two previous years (1988 and 1989); 
  # you'll see why in a second.
  
  vdem_data <- vdem_data %>%
    dplyr::filter(year >= 1988 & year <= 2017) 
  
# We have information for 12 countries covering 30 years (1988-2017)
  # We have 336 observations in total (12 x 30 = 360).

# Aggregate SCAD data and merge it #############################################
  
# Now, let's aggregate the SCAD data only considering demonstrations by year and
  # country (as we learned in Laboratory 1).

  scad_demos <- scad %>% 
    dplyr::filter(radical_action == "demonstration") %>% 
    group_by(countryname, eyr) %>%
    summarise(events = n()) %>% ungroup() 
  
# Now, let's combine the protest data from SCAD Latin America with the democracy
  # data from V-DEM (polyarchy indicator).
  
  # Note: The original protest SCAD data is structured at the event-level. That means,
  # when we aggreated it by country and year in the code above, we do not get year-observations
  # for which there were no protests (since the data is not built as time-series cross-sectional data, TSCS).
  # Therefore, we will merge the data to the panel (or TSCS) data structure of V-DEM and 
  # and fill the NAs with zeros.
  # Otherwise, look at how the function expand.grid() works to create a full panel data structure.
  
  data_democracy_demos <- vdem_data %>% 
      left_join(scad_demos, by = c("year" = "eyr", "countryname" = "countryname"))  
  
  rm(scad_demos, vdem_data, scad) # Let's clean the environment a little
  

# Last transformations before analysis #########################################

  # Let's calculate the number of events per million inhabitants
  data_democracy_demos <- data_democracy_demos %>%
    mutate(events = ifelse(is.na(events) & year!=1989 & year!=1988, 0, events),
           events_per_million = events / population_2000 * 1000000,
           ) # Replace NA with 0, except for 1988/1989, for which we have no protest observations
  
  # Let's create a lagged variable of the polyarchy indicator to use in the analysis.
  # Then, we will create a simple democratization indicator by taking the difference
  # of the last years and the year before that.
  
  data_democracy_demos <- data_democracy_demos %>%
    arrange(countryname, year) %>% # Order the data
    group_by(countryname) %>% # Group it by country to get lag values (!)
    mutate(v2x_polyarchy_lag  = lag(v2x_polyarchy, 1)) %>% 
    mutate(v2x_polyarchy_2lags = lag(v2x_polyarchy, 2)) %>%
    ungroup() %>% 
    # Let's create a variable of change that should represent "DEMOCRATIZATION"
      # trends. We will use the difference between the previous year and the
      # two previous years.
    mutate(democratization_2lags = v2x_polyarchy_lag - v2x_polyarchy_2lags)
  
  
# Analysis #####################################################################
  
  # First simple visualization using a linear model estimator
  ggplot(data_democracy_demos, aes(y = events_per_million, x = democratization_2lags)) +
    geom_point() +
    stat_smooth(method = "lm", formula = 'y ~ x', se=FALSE, fullrange = TRUE) +
    # lm specification for the visual analysis; look at the effect when setting
    # se and full range to TRUE and FALSE interchangeably.
    facet_wrap(~countryname)
  
  
  # Analysis 1: Run a simple linear model
  
  lm(data = data_democracy_demos, 
     events_per_million ~ v2x_polyarchy_lag + countryname) %>% 
    summary()
  
    # Result: no association!
  
  # Analysis 2:Fixed effects model (manually using lm())
  
  lm(data = data_democracy_demos, 
     events_per_million ~ democratization_2lags + countryname) %>% 
    summary()
  
    # Result: negative association.
  
  
  # Analysis 3: Random slope model
  lmer(events_per_million ~ democratization_2lags + ( 0 + democratization_2lags | countryname),
         data = data_democracy_demos) %>% 
    summary() # Similar results if we include year fix effects.
  
    # Result: no association.
  
  
  # Interpretation: In these preliminary analysis, we find no association in the simple linear model.
  # In the fix effects models, when we account for country specific characteristics and "control them out", 
  # we find a negative association between democratization and protests.
  # In the random slope model, we find no association - here, we assume that each country has 
  # unique responses.

  

