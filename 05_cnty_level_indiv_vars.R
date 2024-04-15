# title: "05_cnty_level_indiv_vars"
# author: "Taylor Mobley"

library(tidyverse)
library(psych)
library(epiDisplay)
options(scipen = 10)
options(digits = 10)

# 25+ education -------------------------------------------------------

## load 25 plus data for educ indicats
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/clean-data-2/",
                       "ipums1940_indiv25plus_dat.RDS")) %>%
  mutate(gt_grade8 = ifelse(educ>=3,1,0))

gc()
# check
# table(data$gt_grade8, data$educ, exclude=NULL)

educ_cnty_data <- data %>%
  group_by(statefip, countyicp) %>%
  count(gt_grade8) %>%
  mutate(prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = gt_grade8, values_from=prop) %>%
  rename(educ_gt8_cnty_prop = `1`,
         educ_le8_cnty_prop = `0`,
         educ_miss_cnty_prop = `NA`) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

blk_educ_cnty_data <- data %>%
  filter(race==2) %>%
  group_by(statefip, countyicp) %>%
  count(gt_grade8) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  ungroup() %>% 
  pivot_wider(names_from = gt_grade8, values_from=c(blk_prop,n)) %>% 
  rename(educ_gt8_cnty_blk_prop = `blk_prop_1`,
         educ_le8_cnty_blk_prop = `blk_prop_0`,
         educ_miss_cnty_blk_prop = `blk_prop_NA`,
         educ_le8_cnty_blk_n = `n_0`) %>%
  dplyr::select(-c(n_1, n_NA)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# 8/15/2023 TMM ungrouping

wht_educ_cnty_data <- data %>%
  filter(race==1) %>%
  group_by(statefip, countyicp) %>%
  count(gt_grade8) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  ungroup() %>% 
  pivot_wider(names_from = gt_grade8, values_from=c(wht_prop,n)) %>% 
  rename(educ_gt8_cnty_wht_prop = `wht_prop_1`,
         educ_le8_cnty_wht_prop = `wht_prop_0`,
         educ_miss_cnty_wht_prop = `wht_prop_NA`,
         educ_gt8_cnty_wht_n = `n_1`) %>%
  dplyr::select(-c(n_0, n_NA)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) # 8/15/2023 TMM ungrouping

educ_cnty_data <- educ_cnty_data %>%
  left_join(blk_educ_cnty_data) %>%
  left_join(wht_educ_cnty_data)

remove(blk_educ_cnty_data)
remove(wht_educ_cnty_data)
remove(data)
gc()

# 16+ employment and income -------------------------------------------

# TMM -- for Paloma, noting employemnt variables are calculated in the same way
# as those in the 04_ed_level_indiv_vars.R script

## Load 16 plus data set for empstat and incwage indicats
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/clean-data-2/",
                       "ipums1940_indiv16plus_dat.RDS"))

## Employment status
emp_cnty_data <- data %>%
  filter(empstat_factor!="Not in labor force") %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=prop) %>%
  rename(emp16_cnty_prop = Employed,
         unemp16_cnty_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) 

blk_emp_cnty_data <- data %>%
  filter(empstat_factor!="Not in labor force" & 
           race==2) %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=blk_prop) %>% 
  rename(emp16_cnty_blk_prop = Employed,
         unemp16_cnty_blk_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) 

wht_emp_cnty_data <- data %>%
  filter(empstat_factor!="Not in labor force" & 
           race==1) %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=wht_prop) %>% 
  rename(emp16_cnty_wht_prop = Employed,
         unemp16_cnty_wht_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

notlf_cnty_data <- data %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=prop) %>% 
  rename(notlf16_cnty_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) 

notlf_cnty_wht_data <- data %>%
  filter(race==1) %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=wht_prop) %>% 
  rename(notlf16_cnty_wht_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed))

notlf_cnty_blk_data <- data %>%
  filter(race==2) %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=blk_prop) %>% 
  rename(notlf16_cnty_blk_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed))

## Number of employed and unemployed White residents  
test <- data %>% 
  filter(empstat_factor!="Not in labor force" & race==1) %>%
  count(statefip, countyicp, empstat_factor) %>%
  pivot_wider(names_from = empstat_factor, values_from=n) %>% 
  rename(emp16_cnty_wht_n = `Employed`,
         unemp16_cnty_wht_n = `Unemployed`) 

## Number of employed and unemployed Black residents 
test2 <- data %>% 
  filter(empstat_factor!="Not in labor force" & race==2) %>%
  count(statefip, countyicp, empstat_factor) %>%
  pivot_wider(names_from = empstat_factor, values_from=n) %>% 
  rename(emp16_cnty_blk_n = `Employed`,
         unemp16_cnty_blk_n = `Unemployed`) 

emp_cnty_data <- emp_cnty_data %>%
  left_join(blk_emp_cnty_data) %>%
  left_join(wht_emp_cnty_data) %>%
  left_join(notlf_cnty_data) %>%
  left_join(notlf_cnty_wht_data) %>%
  left_join(notlf_cnty_blk_data) %>%
  left_join(test) %>%
  left_join(test2)

remove(wht_emp_cnty_data)
remove(blk_emp_cnty_data)
remove(notlf_cnty_data)
remove(notlf_cnty_wht_data)
remove(notlf_cnty_blk_data)
remove(test)
remove(test2)
gc()

## Wages
wage_cnty_data <- data %>%
  group_by(statefip, countyicp) %>%
  summarize(wage16_cnty_median = median(incwage, na.rm=TRUE),
            wage16_cnty_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage16_cnty_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

blk_wage_cnty_data <- data %>%
  filter(race==2) %>%
  group_by(statefip, countyicp) %>%
  summarize(wage16_cnty_blk_median = median(incwage, na.rm=TRUE),
            wage16_cnty_blk_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage16_cnty_blk_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup()

wht_wage_cnty_data <- data %>%
  filter(race==1) %>%
  group_by(statefip, countyicp) %>%
  summarize(wage16_cnty_wht_median = median(incwage, na.rm=TRUE),
            wage16_cnty_wht_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage16_cnty_wht_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

wage_cnty_data <- wage_cnty_data %>%
  left_join(blk_wage_cnty_data) %>%
  left_join(wht_wage_cnty_data)

remove(wht_wage_cnty_data)
remove(blk_wage_cnty_data)

remove(data)
gc()

# 14+ employment and income ----------------------------------------

## Load 14 plus data set for empstat and incwage indicats
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/clean-data-2/",
                       "ipums1940_indiv14plus_dat.RDS"))
gc()

## Employment status
emp14_cnty_data <- data %>%
  filter(empstat_factor!="Not in labor force") %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=prop) %>%
  rename(emp14_cnty_prop = Employed,
         unemp14_cnty_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) 

blk_emp14_cnty_data <- data %>%
  filter(empstat_factor!="Not in labor force" & 
          race==2) %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=blk_prop) %>% 
  rename(emp14_cnty_blk_prop = Employed,
         unemp14_cnty_blk_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

wht_emp14_cnty_data <- data %>%
  filter(empstat_factor!="Not in labor force" & 
           race==1) %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=wht_prop) %>% 
  rename(emp14_cnty_wht_prop = Employed,
         unemp14_cnty_wht_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

notlf14_cnty_data <- data %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=prop) %>% 
  rename(notlf14_cnty_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) 

notlf14_cnty_wht_data <- data %>%
  filter(race==1) %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=wht_prop) %>% 
  rename(notlf14_cnty_wht_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) 

notlf14_cnty_blk_data <- data %>%
  filter(race==2) %>%
  group_by(statefip, countyicp) %>%
  count(empstat_factor) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=blk_prop) %>% 
  rename(notlf14_cnty_blk_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) 

## Number of employed and unemployed White residents  
test <- data %>% 
  filter(empstat_factor!="Not in labor force" & race==1) %>%
  count(statefip, countyicp, empstat_factor) %>%
  pivot_wider(names_from = empstat_factor, values_from=n) %>% 
  rename(emp14_cnty_wht_n = `Employed`,
         unemp14_cnty_wht_n = `Unemployed`) 

## Number of employed and unemployed Black residents 
test2 <- data %>% 
  filter(empstat_factor!="Not in labor force" & race==2) %>%
  count(statefip, countyicp, empstat_factor) %>%
  pivot_wider(names_from = empstat_factor, values_from=n) %>% 
  rename(emp14_cnty_blk_n = `Employed`,
         unemp14_cnty_blk_n = `Unemployed`) 

emp14_cnty_data <- emp14_cnty_data %>%
  full_join(blk_emp14_cnty_data) %>%
  full_join(wht_emp14_cnty_data) %>%
  full_join(notlf14_cnty_data) %>%
  full_join(notlf14_cnty_wht_data) %>%
  full_join(notlf14_cnty_blk_data) %>%
  full_join(test) %>%
  full_join(test2)

remove(wht_emp14_cnty_data)
remove(blk_emp14_cnty_data)
remove(notlf14_cnty_data)
remove(notlf14_cnty_wht_data)
remove(notlf14_cnty_blk_data)
remove(test)
remove(test2)
gc()

## Wages
wage14_cnty_data <- data %>%
  group_by(statefip, countyicp) %>%
  summarize(wage14_cnty_median = median(incwage, na.rm=TRUE),
            wage14_cnty_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage14_cnty_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

gc()

blk_wage14_cnty_data <- data %>%
  filter(race==2) %>%
  group_by(statefip, countyicp) %>%
  summarize(wage14_cnty_blk_median = median(incwage, na.rm=TRUE),
            wage14_cnty_blk_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage14_cnty_blk_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

wht_wage14_cnty_data <- data %>%
  filter(race==1) %>%
  group_by(statefip, countyicp) %>%
  summarize(wage14_cnty_wht_median = median(incwage, na.rm=TRUE),
            wage14_cnty_wht_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage14_cnty_wht_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

wage14_cnty_data <- wage14_cnty_data %>%
  left_join(blk_wage14_cnty_data) %>%
  left_join(wht_wage14_cnty_data)

remove(wht_wage14_cnty_data)
remove(blk_wage14_cnty_data)
remove(data)
gc()

# merge and save --------------------------------------------------

# merge all to ed_hh_indiv_data
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/clean-data-2/",
                       "ipums1940_ed_hh_indiv_vars.RDS"))

data <- data %>% 
  left_join(educ_cnty_data) %>%
  left_join(emp_cnty_data) %>%
  left_join(wage_cnty_data) %>%
  left_join(emp14_cnty_data) %>%
  left_join(wage14_cnty_data)

# check -- 8/15/2023 TMM 
# sapply(data, function(x) sum(is.na(x)))
# check <- data %>% filter(is.na(educ_le8_cnty_prop))
# check2 <- data %>% filter(is.na(educ_le8_cnty_blk_n))
# indiv_data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
#                        "comm-level-social-vars/data/clean-data-2/",
#                        "ipums1940_indiv25plus_dat.RDS")) %>%
#   mutate(gt_grade8 = ifelse(educ>=3,1,0))

# 8/15/2023 TMM notes:
# missingness could be due to areas with 0 people in appropriate age group or
# 0 people in racial group for race-stratified variables. Making the counts 0
# but leaving the continous variables missing in these cases since 0 would be 
# an informative value

data <- data %>%
  mutate_at(vars(contains("_prop")), replace_na, 0) %>%
  mutate_at(vars(contains("_n")), replace_na, 0)

# check -- 8/15/2023 TMM 
# sapply(data, function(x) sum(is.na(x)))

saveRDS(data, paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                     "comm-level-social-vars/data/clean-data-2/",
                     "ipums1940_arealvl_hh_indiv_vars.RDS"))
