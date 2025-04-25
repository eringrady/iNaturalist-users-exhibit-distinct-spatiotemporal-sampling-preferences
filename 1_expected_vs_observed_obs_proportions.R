######################## OVERALL AND USER GROUP EXPECTED VS. OBSERVED OBSERVATION PROPORTIONS FOR EACH VARIABLE

#loading packages
library(data.table)
library(tidyverse)

#reading in observations
obs <- fread("/blue/guralnick/gradyerin/thesis/final_dataset/SE_inat_obs_12.31.23")

#extracting expected proportions for each variable
expected_props <- obs %>% 
  select(nlcd_group, protected, urban, low_income, day_type, prot_exp_prop, lc_exp_prop, urban_exp_prop, park, park_exp_prop, lowinc_exp_prop) %>% 
  mutate(day_exp_prop = ifelse(day_type == "week", 0.714, 0.2857)) %>% 
  pivot_longer(cols = contains("exp"),
               names_to = "exp_variable",
               values_to = "exp_proportion") %>% 
  distinct() %>% 
  pivot_longer(
    cols = -contains("exp"),
    names_to = "variable",
    values_to = "variable_specific") %>% 
  distinct() %>% 
  mutate(exp_variable = ifelse(exp_variable == "lc_exp_prop", "nlcd_group", 
                             ifelse(exp_variable == "prot_exp_prop", "protected",
                                    ifelse(exp_variable == "urban_exp_prop", "urban",
                                           ifelse(exp_variable == "park_exp_prop", "park",
                                                  ifelse(exp_variable == "lowinc_exp_prop", "low_income",
                                                         ifelse(exp_variable == "day_exp_prop", "day_type", "NA"))))))) %>% 
  filter(exp_variable == variable) %>% 
  select(variable_specific, exp_proportion) %>% 
  rename(category = variable_specific,
         expected_proportion = exp_proportion) 


#Summarizing actual counts for focal region (not user-group specific) and comparing to expected
lc_summary_all <- obs %>% 
  group_by(nlcd_group) %>% 
  summarize(count = n()) %>% 
  rename(category = nlcd_group) %>% 
  ungroup() %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
prot_summary_all <- obs %>% 
  group_by(protected) %>% 
  summarize(count = n()) %>% 
  rename(category = protected)%>% 
  ungroup() %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
urban_summary_all <- obs %>% 
  group_by(urban) %>% 
  summarize(count = n()) %>% 
  rename(category = urban)%>% 
  ungroup() %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
park_summary_all <- obs %>% 
  group_by(park) %>% 
  summarize(count = n()) %>% 
  rename(category = park)%>% 
  ungroup() %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
low_inc_summary_all <- obs %>% 
  group_by(low_income) %>% 
  summarize(count = n()) %>% 
  rename(category = low_income)%>% 
  ungroup() %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
day_summary_all <- obs %>% 
  group_by(day_type) %>% 
  summarize(count = n()) %>% 
  rename(category = day_type) %>% 
  ungroup() %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
compare_props_all <- rbind(lc_summary_all, prot_summary_all, urban_summary_all, park_summary_all, low_inc_summary_all, day_summary_all) %>% 
  left_join(expected_props, by = "category") %>% 
  mutate(across(c('proportion', `expected_proportion`), ~round(.x, 3)))


#Summarizing actual counts for each user group and comparing to expected
lc_summary <- obs %>% 
  group_by(combined_group, nlcd_group) %>% 
  summarize(count = n()) %>% 
  rename(category = nlcd_group) %>% 
  ungroup() %>% 
  group_by(combined_group) %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
prot_summary <- obs %>% 
  group_by(combined_group, protected) %>% 
  summarize(count = n()) %>% 
  rename(category = protected)%>% 
  ungroup() %>% 
  group_by(combined_group) %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
urban_summary <- obs %>% 
  group_by(combined_group, urban) %>% 
  summarize(count = n()) %>% 
  rename(category = urban)%>% 
  ungroup() %>% 
  group_by(combined_group) %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
park_summary <- obs %>% 
  group_by(combined_group, park) %>% 
  summarize(count = n()) %>% 
  rename(category = park)%>% 
  ungroup() %>% 
  group_by(combined_group) %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
low_inc_summary <- obs %>% 
  group_by(combined_group, low_income) %>% 
  summarize(count = n()) %>% 
  rename(category = low_income)%>% 
  ungroup() %>% 
  group_by(combined_group) %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
day_summary <- obs %>% 
  group_by(combined_group, day_type) %>% 
  summarize(count = n()) %>% 
  rename(category = day_type) %>% 
  ungroup() %>% 
  group_by(combined_group) %>% 
  mutate(proportion = count / sum(count)) %>% 
  select(-count)
compare_props_groups <- rbind(lc_summary, prot_summary, urban_summary, park_summary, low_inc_summary, day_summary) %>% 
  left_join(expected_props, by = "category") %>% 
  pivot_wider(names_from = combined_group,
              values_from = proportion) %>% 
  mutate(across(c(`expected_proportion`, `casual,local`, `casual,traveler`, `super,local`, `super,traveler`), ~round(.x, 3)))