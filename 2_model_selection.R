######################## EXPLORATORY MODEL SELECTION (4 MODELS FOR EACH VARIABLE including a combination of participation group, traveler status, and their interaction)

#loading packages
library(data.table)
library(tidyverse)
library(nnet)

#reading in observations
obs <- fread("/blue/guralnick/gradyerin/thesis/final_dataset/SE_inat_obs_12.31.23")

#filtering to include just urban observations for certain models
obs_urban <- obs %>% 
  filter(urban == "urban")


####################### QUALITY GRADE

#MODEL 1: including participation group, traveler status, and their interaction
m1_qu <- multinom(quality_grade ~ participation_group * traveler_status,
                  data = obs_clean)
#MODEL 2: including participation group and traveler status
m2_qu <- multinom(quality_grade ~ participation_group + traveler_status,
                  data = obs_clean)
#MODEL 3: including participation group
m3_qu <- multinom(quality_grade ~ participation_group,
                  data = obs_clean)
#MODEL 4: including traveler status
m4_qu <- multinom(quality_grade ~ traveler_status,
                  data = obs_clean)
#Comparing AICs
qu_aic <- c(m1 = AIC(m1_qu), 
            m2 = AIC(m2_qu), 
            m3 = AIC(m3_qu), 
            m4 = AIC(m4_qu))
akaike.weights(qu_aic)


####################### LAND COVER

#MODEL 1: including participation group, traveler status, and their interaction
m1_lc <-multinom(nlcd_group ~ participation_group * traveler_status, 
                 data = obs_clean)
#MODEL 2: including participation group and traveler status
m2_lc <- multinom(nlcd_group ~ participation_group + traveler_status, 
                  data = obs_clean)
#MODEL 3: including participation group
m3_lc <- multinom(nlcd_group ~ participation_group, 
                  data = obs_clean)
#MODEL 4: including traveler status
m4_lc <- multinom(nlcd_group ~ traveler_status, 
                  data = obs_clean)
#Comparing AICs
lc_aic <- c(m1 = AIC(m1_lc), 
            m2 = AIC(m2_lc), 
            m3 = AIC(m3_lc), 
            m4 = AIC(m4_lc))
akaike.weights(lc_aic) 


####################### PROTECTED LAND

#MODEL 1: including participation group, traveler status, and their interaction
m1_prot <- glm(protected == "protected" ~ participation_group * traveler_status, 
               family = binomial, 
               data = obs_clean)
#MODEL 2: including participation group and traveler status
m2_prot <- glm(protected == "protected" ~ participation_group + traveler_status, 
               family = binomial, 
               data = obs_clean)
#MODEL 3: including participation group
m3_prot <- glm(protected == "protected" ~ participation_group, 
               family = binomial, 
               data = obs_clean)
#MODEL 4: including traveler status
m4_prot <- glm(protected == "protected" ~ traveler_status, 
               family = binomial, 
               data = obs_clean)
#Comparing AICs
prot_aic <- c(m1 = AIC(m1_prot), 
              m2 = AIC(m2_prot), 
              m3 = AIC(m3_prot), 
              m4 = AIC(m4_prot))
akaike.weights(prot_aic) 


####################### URBAN AREAS

#MODEL 1: including participation group, traveler status, and their interaction
m1_urban <- glm(urban == "urban" ~ participation_group * traveler_status, 
                family = binomial, 
                data = obs_clean)
#MODEL 2: including participation group and traveler status
m2_urban <- glm(urban == "urban" ~ participation_group + traveler_status, 
                family = binomial, 
                data = obs_clean)
#MODEL 3: including participation group
m3_urban <- glm(urban == "urban" ~ participation_group, 
                family = binomial, 
                data = obs_clean)
#MODEL 4: including traveler status
m4_urban <- glm(urban == "urban" ~ traveler_status, 
                family = binomial, 
                data = obs_clean)
#Comparing AICs
urban_aic <- c(m1 = AIC(m1_urban), 
               m2 = AIC(m2_urban), 
               m3 = AIC(m3_urban), 
               m4 = AIC(m4_urban))
akaike.weights(urban_aic) 


####################### URBAN PARKS

#MODEL 1: including participation group, traveler status, and their interaction
m1_park <- glm(park == "park" ~ participation_group * traveler_status, 
               family = binomial, 
               data = obs_urban)
#MODEL 2: including participation group and traveler status
m2_park <- glm(park == "park" ~ participation_group + traveler_status, 
               family = binomial, 
               data = obs_urban)
#MODEL 3: including participation group
m3_park <- glm(park == "park" ~ participation_group, 
               family = binomial, 
               data = obs_urban)
#MODEL 4: including traveler status
m4_park <- glm(park == "park" ~ traveler_status, 
               family = binomial, 
               data = obs_urban)
#Comparing AICs
park_aic <- c(m1 = AIC(m1_park), 
              m2 = AIC(m2_park), 
              m3 = AIC(m3_park), 
              m4 = AIC(m4_park))
akaike.weights(park_aic) 



####################### URBAN LOW-INCOME

#MODEL 1: including participation group, traveler status, and their interaction
m1_inc <- glm(low_income_cen == "low_income" ~ participation_group * traveler_status,
              family = binomial,
              data = obs_urban)
#MODEL 2: including participation group and traveler status
m2_inc <- glm(low_income_cen == "low_income" ~ participation_group + traveler_status, 
              family = binomial, 
              data = obs_urban)
#MODEL 3: including participation group
m3_inc <- glm(low_income_cen == "low_income" ~ participation_group, 
              family = binomial, 
              data = obs_urban)
#MODEL 4: including traveler status
m4_inc <- glm(low_income_cen == "low_income" ~ traveler_status, 
              family = binomial, 
              data = obs_urban)
#Comparing AICs
inc_aic <- c(m1 = AIC(m1_inc), 
             m2 = AIC(m2_inc), 
             m3 = AIC(m3_inc), 
             m4 = AIC(m4_inc))
akaike.weights(inc_aic) #


####################### WEEKEND VS. WEEKDAY

#MODEL 1: including participation group, traveler status, and their interaction
m1_day <- glm(day_type == "weekend" ~ participation_group * traveler_status, 
              family = binomial, 
              data = obs_clean)
#MODEL 2: including participation group and traveler status
m2_day <- glm(day_type == "weekend" ~ participation_group + traveler_status, 
              family = binomial, 
              data = obs_clean)
#MODEL 3: including participation group
m3_day <- glm(day_type == "weekend" ~ participation_group, 
              family = binomial, 
              data = obs_clean)
#MODEL 4: including traveler status
m4_day <- glm(day_type == "weekend" ~ traveler_status, 
              family = binomial, 
              data = obs_clean)
#Comparing AICs
day_aic <- c(m1 = AIC(m1_day), 
             m2 = AIC(m2_day), 
             m3 = AIC(m3_day), 
             m4 = AIC(m4_day))
akaike.weights(day_aic) 


