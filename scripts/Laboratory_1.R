# Data Analysis R Lab 1, 04.11.2024
# Saldivia Gonzatti and Hutter

# Set-up #######################################################################

options(scipen = 999) # Turn-off scientific numeric notation.

library(dplyr)      # Upload R package to handle and manage data.
library(ggplot2)    # Upload R package to create plots.
library(tidyverse)  # Upload R package to handle and manage data.
library(patchwork)  # Upload R package to layout plots together.

# Download the data from https://poldem.eui.eu/download/ and check the codebook!

data <- read.csv("poldem-protest_30.csv") # Upload the data


# Short descriptions ###########################################################

# Let's inspect part of the data

table(data$action_form, exclude = NULL)
table(data$radical_action, exclude = NULL)

summary(data$part_all)

# Let's analyze number of events by protest form ('radical action')

country_comparison <- data %>% 
  group_by(country_name, radical_action) %>% 
  summarise(events_weighted = sum(weighted_event, na.rm = TRUE)) %>% 
  group_by(country_name) %>%
  mutate(total_events = sum(events_weighted, na.rm = TRUE),
         percentage = events_weighted / total_events * 100) 

# What are the most and the least confrontational countries?

country_comparison %>%
  dplyr::filter(radical_action == "confrontational") %>% 
  arrange(desc(percentage)) 

country_comparison %>%
  dplyr::filter(radical_action == "confrontational") %>% 
  arrange(percentage) 


# What are the most and the least violent countries?

country_comparison %>%
  dplyr::filter(radical_action == "violent") %>% 
  arrange(desc(percentage)) %>% 
  as.data.frame()

country_comparison %>%
  dplyr::filter(radical_action == "violent") %>% 
  arrange(percentage)


# Antara's homework: How to identify quantile groups?

percentile_distribution <- country_comparison %>%
  dplyr::filter(radical_action == "violent") %>% 
  ungroup()

  # Define the quantile cut-off points

quantiles <- quantile(percentile_distribution$percentage, 
                      probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  # Based on that, assign them to the dataframe again

percentile_distribution$percentage_quantile <- cut(percentile_distribution$percentage, 
                                                   breaks = quantiles, 
                                                   include.lowest = TRUE, 
                                                   labels = c(0, 0.25, 0.5, 0.75),
                                                   right = TRUE)

# Correlation ##################################################################

# Let's analyze the intensity of protest events in Germany and Spain

country_selection <- data %>% 
  dplyr::filter(country_name %in% c("germany", "spain")) %>% 
  group_by(country_name, year) %>%
  summarise(events = n(),
            events_weighted = sum(weighted_event)) 

  ## You can transform the data in wide format for pairwise analysis, 
  ## e.g. for correlation

country_selection.wide <-
  country_selection %>%
  pivot_wider(names_from = country_name, 
              values_from = c(events, events_weighted))

cor.test(country_selection.wide$events_weighted_spain, 
         country_selection.wide$events_weighted_germany)

cor.test(country_selection.wide$events_spain, 
         country_selection.wide$events_germany)

rm(country_selection.wide) # Let's clean the work space a little.


# Visualization ################################################################

# Now, let's see the patterns of mobilization in Germany and Spain

  ## First, by number of events:

  ggplot(country_selection, aes(x = year, y = events, color = country_name)) +
  geom_line() +
  # Define the color lines as the country names
  scale_color_manual(values = c("germany" = "red", "spain" = "blue")) +
  theme_bw() + 
  xlab("Year") + ylab("Number of events, absolute") +
  # Define the legend names and labels
  labs(color = "Country") 

  ## Second, by the weighted number of events:

country_selection %>% 
  ggplot(aes(x = year, y = events_weighted, color = country_name)) +
  geom_line() +
  # Define the color lines as the country names
  scale_color_manual(values = c("germany" = "red", "spain" = "blue")) +
  theme_bw() +
  xlab("Year") + ylab("Number of events, weighted") +
  # Define the legend names and labels
  labs(color = "Country") 

# Now, let's inspect both cases in terms of number of participants

data <- data %>% 
  mutate(pe_size = case_when(
       weighted_part_all < 99 ~ "small",
       weighted_part_all >= 99 & weighted_part_all <= 9999 ~ "moderate/large",
       weighted_part_all >= 10000 ~ "mass"
       )) # Operationalization following Soule and Earl (2005).

  ## Aggregate the data


country_selection.part <- data %>% 
  dplyr::filter(country_name %in% c("germany", "spain")) %>% 
  group_by(country_name, year, pe_size) %>%
  summarise(events = n(),
            events_weighted = sum(weighted_event),
            participation_weighted = sum(weighted_part_all, na.rm = TRUE)) 

  ## Plot the data

spain_participation <- country_selection.part %>% 
  dplyr::filter(country_name=="spain") %>% 
  ggplot(aes(x = year, y = events_weighted, color = pe_size)) +
  geom_line() +
  # Define the color lines as the country names
  scale_color_manual(values = c("small" = "blue", "moderate/large" = "black",
                                "mass" = "red")) +
  theme_bw() +
  xlab("Year") + ylab("Mobilization size, weighted - Spain") +
  # Define the legend names and labels
  labs(color = "Protest size") + ylim(0,135) +
  ggtitle("Spain")

germany_participation <- country_selection.part %>% 
  dplyr::filter(country_name=="germany") %>% 
  ggplot(aes(x = year, y = events_weighted, color = pe_size)) +
  geom_line() +
  # Define the color lines as the country names
  scale_color_manual(values = c("small" = "blue", "moderate/large" = "black",
                                "mass" = "red")) +
  theme_bw() +
  xlab("Year") + ylab("Mobilization size, weighted - Germany") +
  # Define the legend names and labels
  labs(color = "Protest size") + ylim(0,135) +
  ggtitle("Germany")

# Let's combine the plots
spain_participation + germany_participation
