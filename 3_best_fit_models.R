######################## BEST-FITTING MODELS FOR EACH VARIABLE

#loading packages
library(data.table)
library(tidyverse)
library(nnet)
library(emmeans)

#reading in observations
obs <- fread("/blue/guralnick/gradyerin/thesis/final_dataset/SE_inat_obs_12.31.23")

#filtering to include just urban observations for certain models
obs_urban <- obs %>% 
  filter(urban == "urban")

####################### QUALITY GRADE

#multinomial logistic regression including participation group, traveler status, and their interaction as predictors
m1_qu <- multinom(quality_grade ~ participation_group * traveler_status,
                  data = obs_clean)

#table with results
qu_table <- tbl_regression(m1_qu, exponentiate = TRUE, intercept = TRUE) %>% 
  bold_p()

#plotting effects of participation group and traveler group
plot_model(m1_qu, type = "eff", terms = c("participation_group", "traveler_status"))


####################### LAND COVER

#multinomial logistic regression including participation group, traveler status, and their interaction as predictors
m1_lc <- multinom(nlcd_group ~ participation_group * traveler_status, 
                  data = obs_clean)

#table with results
lc_table <- tbl_regression(m1_lc, exponentiate = TRUE, intercept = TRUE) %>% 
  bold_p()

#plotting the effects of participation group and traveler group
plot_model(m1_lc, type = "eff", terms = c("participation_group", "traveler_status"))


####################### PROTECTED LAND

#binomial logistic regression including participation group, traveler status, and their interaction as predictors
m1_prot <- glm(protected == "protected" ~ participation_group * traveler_status, 
               family = binomial, 
               data = obs_clean)

#table with results
prot_table <- tbl_regression(m1_prot, exponentiate = TRUE, intercept = TRUE) %>% 
  bold_p()

#plotting the effects of participation group and traveler group
plot_model(m1_prot, type = "eff", terms = c("participation_group", "traveler_status"))


####################### URBAN AREAS

#binomial logistic regression including participation group, traveler status, and their interaction as predictors
m1_urban <- glm(urban == "urban" ~ participation_group * traveler_status, 
                family = binomial, 
                data = obs_clean)

#table with results
urban_table <- tbl_regression(m1_urban, exponentiate = TRUE, intercept = TRUE) %>% 
  bold_p()

#plotting the effects of participation group and traveler group
plot_model(m1_urban, type = "eff", terms = c("participation_group", "traveler_status"))


####################### URBAN PARKS

#M1
#binomial logistic regression including participation group and traveler status as predictors (no interaction)
m1_park <- glm(park == "park" ~ participation_group + traveler_status, 
               family = binomial, 
               data = obs_urban)

#table with results
park1_table <- tbl_regression(m1_park, exponentiate = TRUE, intercept = TRUE) %>% 
  bold_p()

#plotting the effects of participation group and traveler group
plot_model(m1_park, type = "eff", terms = c("participation_group", "traveler_status"))

#M2
#binomial logistic regression including participation group, traveler status, and their interaction as predictors
m2_park <- glm(park == "park" ~ participation_group * traveler_status, 
               family = binomial, 
               data = obs_urban)

#table with results
park2_table <- tbl_regression(m2_park, exponentiate = TRUE, intercept = TRUE) %>% 
  bold_p()

#plotting the effects of participation group and traveler group
plot_model(m12_park, type = "eff", terms = c("participation_group", "traveler_status"))


####################### URBAN LOW-INCOME

#binomial logistic regression including participation group, traveler status, and their interaction as predictors
m1_inc <- glm(low_income_cen == "low_income" ~ participation_group * traveler_status,
              family = binomial,
              data = obs_urban)

#table with results
inc_table <- tbl_regression(m1_inc, exponentiate = TRUE, intercept = TRUE) %>% 
  bold_p()

#plotting the effects of participation group and traveler group
plot_model(m1_inc, type = "eff", terms = c("participation_group", "traveler_status"))


####################### WEEKEND VS. WEEKDAY

#binomial logistic regression including participation group, traveler status, and their interaction as predictors
m1_day <- glm(day_type == "weekend" ~ participation_group * traveler_status, 
              family = binomial, 
              data = obs_clean)

#table with results
day_table <- tbl_regression(m1_day, exponentiate = TRUE, intercept = TRUE) %>% 
  bold_p()

#plotting the effects of participation group and traveler group
plot_model(m1_day, type = "eff", terms = c("participation_group", "traveler_status"))
