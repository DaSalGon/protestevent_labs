# Statistical test homework, 28.10.2024

# This script offers one solution to the tasks requested for the statistical test.
# There are of course very different ways to solve the tasks, this is just one of them.

options(scipen = 999) # Turn-off scientific numeric notation.
load("data/statistical test/ebm_selection.RData")    # Load the data based on its location.

library(dplyr)    # Upload R package to handle and manage data.
library(ggplot2)  # Upload R package to create plots.

# TASK 1 ===============================================

# Get the mean of trust_parliament and trust_government for each 
# country per survey year.

# Here, we first group the data by country and year of the survey
# and then proceed to get the mean of trust in parliament and government
# excluding NA values. Otherwise, we would get only a final NA value

ebm_selection %>% 
  dplyr::group_by(country_cl, year) %>% 
  dplyr::summarise(mean_trust_parliament = mean(trust_parliament, na.rm = TRUE),
                   mean_trust_government = mean(trust_government, na.rm = TRUE))

# TASK 2 ===============================================

# Show the distribution of trust in parliament in France in 2020 using ggplot
# Note that this is not a nice visualization, since our variable of interest is binary.

ggplot(ebm_selection %>% 
         dplyr::filter(country_cl=="France" & year==2020), # Here, we subset the data.
       
       aes(x=trust_parliament)) + # Here, we define the variable of interest.
  
  geom_histogram(binwidth = 0.5, fill="blue", color="black") + 
  # Here, we set the type of graph we want.
  
  # Now, we define some labelling for the visualization.
  labs(title="Distribution of trust in parliament in France in 2020",
       x="Trust in parliament",
       y="Frequency")


ebm_selection %>% dplyr::filter(!is.na(trust_parliament)) %>%
  ggplot( aes(x=age, )) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080"))


# Alternatively, we can look at the age distribution by the two different 
# trust groups (0 = no trust, 1 = trust). This might help us to have a preliminary
# idea about the regression task later:


ebm_selection %>% dplyr::filter(!is.na(trust_parliament)) %>% # Subset the data.
  
  ggplot(aes(age, fill=as.factor(trust_parliament), # Define the variables.
             
             colour = as.factor(trust_parliament))) + # And colors.
  
  geom_density(alpha = 0.1) + # Define the visualization setting for density distributions.
  
  theme_bw() # Let's simplify the aesthetics.

# --> Does this graph help us get a better picture about the relationship
#     between age and trust in parliament?


# TASK 3 ===============================================

# Assume we want to study the linear relationship between age and 
# trust in government including country fix effects. 
# Implement a linear OLS regression, although logit would be the one correct approach.

# You can use the pipes by dplyr:: to conduct the full analysis in one shot:

ebm_selection <- ebm_selection %>% 
  mutate(country_factor = as.factor(country_cl)) 

# Let's transfor the country variable to a factor variable,
# although in this case it won't change the results.

ebm_selection %>% 
  dplyr::filter(!is.na(trust_government)) %>% # Subset NAs
  lm(trust_government ~ age + country_factor, data = .) %>% # Specify the model.
  # Including the country variable as a IV serves as a manually set fix effect.
  # There are many alternatives about how to do this.
  
  summary() # Summarize the results.


# Now, let's run the logistic regression through separate steps:

data_clean <- ebm_selection %>% 
  dplyr::filter(!is.na(trust_government)) # Subset the data beforehand.

logistic_regression <- glm(trust_government ~ age + country_factor, 
                           # Use the regular glm function from R.
                           
                           # Define that the regression should be logistic:
                           data = data_clean, family = "binomial")

# Summarize the results.
summary(logistic_regression)

