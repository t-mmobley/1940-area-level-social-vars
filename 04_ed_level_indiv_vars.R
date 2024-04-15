# title: "04_area_level_indiv_vars"
# author: "Taylor Mobley"

library(tidyverse)
library(psych)
library(epiDisplay)
options(scipen = 10)
options(digits = 10)

# Individual-level descriptive stats -----------------------------------
# indiv_data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
#                              "comm-level-social-vars/data/clean-data-2/",
#                              "ipums1940_indiv_dat.RDS"))
# 
# ## 1. Age distribution
# 
# table(indiv_data$age, exclude=NULL)
# hist(indiv_data$age, breaks=30)
# 
# ## 1. Race [no missingness]
# 
# table(indiv_data$race, exclude=NULL)
# 
# ## 2. Educational attainment
# 
# ### Desired var: pop >=25 years old with completed edu greater than 8th grade
# 
# with(indiv_data %>% filter(pop25plus==1), 
#      tab1(educ_factor, cum.percent = TRUE))
# 
# ## 3. Nativity
# 
# ### Note: not planning to use because there is a lot of missingness. 
# 
# ## 4. Employment status, general 
# 
# ### Desired var: Population >=16 years old or >=14 yo, unemployed in civilian labor force
# 
# with(indiv_data %>% filter(pop16plus==1),
#      tab1(empstat_factor, cum.percent = TRUE))
# with(indiv_data %>% filter(pop14plus==1),
#      tab1(empstat_factor, cum.percent = TRUE))
# 
# ## 5. Wages summary [note: top-coded at 5,001 in 1940]
# 
# ### Summary stats first among pop aged 16 +
#   
# indiv_data %>% filter(pop16plus==1) %>%
#   group_by(empstat_factor) %>%
#   summarize(n_incwage = n(),
#             min_incwage = min(incwage, na.rm=TRUE),
#             iqr_incwage = IQR(incwage, na.rm=TRUE),
#             median_incwage = median(incwage, na.rm=TRUE),
#             max_incwage = max(incwage, na.rm=TRUE),
#             mean_incwage = mean(incwage, na.rm=TRUE),
#             sd_incwage = sd(incwage, na.rm=TRUE))
# 
# ### Then among pop aged 14+
# 
# indiv_data %>% filter(pop14plus==1) %>%
#   group_by(empstat_factor) %>%
#   summarize(n_incwage = n(),
#             min_incwage = min(incwage, na.rm=TRUE),
#             iqr_incwage = IQR(incwage, na.rm=TRUE),
#             median_incwage = median(incwage, na.rm=TRUE),
#             max_incwage = max(incwage, na.rm=TRUE),
#             mean_incwage = mean(incwage, na.rm=TRUE),
#             sd_incwage = sd(incwage, na.rm=TRUE))
# 
# #### Individual-level summary measures
# 
# # remove all indiv data
# remove(indiv_data)
# gc()

# 25+ education indicators --------------------------------------------
## load 25 plus data for educ indicats
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/clean-data-2/",
                       "ipums1940_indiv25plus_dat.RDS"))

data <- data %>%  
  mutate(gt_grade8 = ifelse(educ>=3,1,0))

gc()
# check
# table(data$gt_grade8, data$educ, exclude=NULL)

educ_ed_data <- data %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(gt_grade8) %>%
  mutate(prop = (n/sum(n))) %>%
  ungroup() %>% 
  dplyr::select(-n) %>%
  pivot_wider(names_from = gt_grade8, values_from=prop) %>%
  rename(educ_gt8_ed_prop = `1`,
         educ_le8_ed_prop = `0`,
         educ_miss_ed_prop = `NA`) %>% 
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

blk_educ_ed_data <- data %>%
  filter(race == 2) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(gt_grade8) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  pivot_wider(names_from = gt_grade8, values_from=c(blk_prop,n)) %>% 
  rename(educ_gt8_ed_blk_prop = `blk_prop_1`,
         educ_le8_ed_blk_prop = `blk_prop_0`,
         educ_miss_ed_blk_prop = `blk_prop_NA`,
         educ_le8_ed_blk_n = `n_0`) %>%
  dplyr::select(-c(n_1, n_NA)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup()  

gc()

wht_educ_ed_data <- data %>%
  filter(race == 1) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(gt_grade8) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  pivot_wider(names_from = gt_grade8, values_from=c(wht_prop,n)) %>% 
  rename(educ_gt8_ed_wht_prop = `wht_prop_1`,
         educ_le8_ed_wht_prop = `wht_prop_0`,
         educ_miss_ed_wht_prop = `wht_prop_NA`,
         educ_gt8_ed_wht_n = `n_1`) %>%
  dplyr::select(-c(n_0, n_NA)) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() 

## merge education data sets and remove intermediate data
educ_ed_data <- educ_ed_data %>%
  full_join(blk_educ_ed_data) %>%
  full_join(wht_educ_ed_data)

remove(wht_educ_ed_data)
remove(blk_educ_ed_data)
remove(data)
gc()

# 16+ employment and income ------------------------------------------

# TMM -- for Paloma, I calculate prop employed and prop unemployed with the intention of
# having a denominator that equals the number of people in the labor force (which I think shouldn't include 
# people counted as "not in the labor force"). I think this makes sense based on other documentation I've read
# but let me know if you disagree! 
# Additionally, prop "not in labor force" variables are just the proportion of all people in a given pop
# (16+ or 14+). Again, I don't think we will use this variable, but flagging that it is a different/
# not comparable with prop employed and prop unemployed 

## Load 16 plus data set for empstat and incwage indicats
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/clean-data-2/",
                       "ipums1940_indiv16plus_dat.RDS"))

## Employment status
emp_ed_data <- data %>%
  filter(empstat_factor!="Not in labor force") %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=prop) %>%
  rename(emp16_ed_prop = Employed,
         unemp16_ed_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() 

blk_emp_ed_data <- data %>%
  filter(empstat_factor!="Not in labor force" & 
           race == 2) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=blk_prop) %>% 
  rename(emp16_ed_blk_prop = Employed,
         unemp16_ed_blk_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() 

wht_emp_ed_data <- data %>%
  filter(empstat_factor!="Not in labor force" & 
            race == 1) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=wht_prop) %>% 
  rename(emp16_ed_wht_prop = Employed,
         unemp16_ed_wht_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup()

notlf_ed_data <- data %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=prop) %>% 
  rename(notlf16_ed_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) %>%
  ungroup() 

notlf_ed_wht_data <- data %>%
  filter(race == 1) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=wht_prop) %>% 
  rename(notlf16_ed_wht_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) %>%
  ungroup() 

notlf_ed_blk_data <- data %>%
  filter(race == 2) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=blk_prop) %>% 
  rename(notlf16_ed_blk_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) %>%
  ungroup() 

## Number of employed and unemployed White residents  
test <- data %>% 
  filter(empstat_factor!="Not in labor force" & race==1) %>%
  count(statefip, countyicp, enumdist, empstat_factor) %>%
  pivot_wider(names_from = empstat_factor, values_from=n) %>% 
  rename(emp16_ed_wht_n = `Employed`,
         unemp16_ed_wht_n = `Unemployed`) 

## Number of employed and unemployed Black residents 
test2 <- data %>% 
  filter(empstat_factor!="Not in labor force" & race==2) %>%
  count(statefip, countyicp, enumdist, empstat_factor) %>%
  pivot_wider(names_from = empstat_factor, values_from=n) %>% 
  rename(emp16_ed_blk_n = `Employed`,
         unemp16_ed_blk_n = `Unemployed`) 

emp_ed_data <- emp_ed_data %>%
  full_join(blk_emp_ed_data) %>%
  full_join(wht_emp_ed_data) %>%
  full_join(notlf_ed_data) %>%
  full_join(notlf_ed_wht_data) %>%
  full_join(notlf_ed_blk_data) %>%
  full_join(test) %>%
  full_join(test2)

remove(wht_emp_ed_data)
remove(blk_emp_ed_data)
remove(notlf_ed_data)
remove(notlf_ed_wht_data)
remove(notlf_ed_blk_data)
remove(test)
remove(test2)
gc()

## Wages
wage_ed_data <- data %>%
  group_by(statefip, countyicp, enumdist) %>%
  summarize(wage16_ed_median = median(incwage, na.rm=TRUE),
            wage16_ed_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage16_ed_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

blk_wage_ed_data <- data %>%
  filter(race == 2) %>%
  group_by(statefip, countyicp, enumdist) %>%
  summarize(wage16_ed_blk_median = median(incwage, na.rm=TRUE),
            wage16_ed_blk_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage16_ed_blk_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

wht_wage_ed_data <- data %>%
  filter(race == 1) %>%
  group_by(statefip, countyicp, enumdist) %>%
  summarize(wage16_ed_wht_median = median(incwage, na.rm=TRUE),
            wage16_ed_wht_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage16_ed_wht_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

wage_ed_data <- wage_ed_data %>%
  full_join(blk_wage_ed_data) %>%
  full_join(wht_wage_ed_data)

remove(wht_wage_ed_data)
remove(blk_wage_ed_data)
remove(data)
gc()

# 14+ employment and income ---------------------------------------

## Load 14 plus data set for empstat and incwage indicats
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/clean-data-2/",
                       "ipums1940_indiv14plus_dat.RDS"))

gc()

## Employment status
emp14_ed_data <- data %>%
  filter(empstat_factor!="Not in labor force") %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=prop) %>%
  rename(emp14_ed_prop = Employed,
         unemp14_ed_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() 

blk_emp14_ed_data <- data %>%
  filter(empstat_factor!="Not in labor force" & 
           race == 2) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=blk_prop) %>% 
  rename(emp14_ed_blk_prop = Employed,
         unemp14_ed_blk_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() 

gc()

wht_emp14_ed_data <- data %>%
  filter(empstat_factor!="Not in labor force" & 
           race == 1) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=wht_prop) %>% 
  rename(emp14_ed_wht_prop = Employed,
         unemp14_ed_wht_prop = Unemployed) %>%
  mutate_if(is.numeric, round, 2) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  ungroup() 

## Prop not in labor force

notlf14_ed_data <- data %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=prop) %>% 
  rename(notlf14_ed_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) %>%
  ungroup()  

notlf14_ed_wht_data <- data %>%
  filter(race == 1) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(wht_prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=wht_prop) %>% 
  rename(notlf14_ed_wht_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) %>%
  ungroup()  

notlf14_ed_blk_data <- data %>%
  filter(race == 2) %>%
  group_by(statefip, countyicp, enumdist) %>%
  count(empstat_factor) %>%
  mutate(blk_prop = (n/sum(n))) %>%
  dplyr::select(-n) %>%
  pivot_wider(names_from = empstat_factor, values_from=blk_prop) %>% 
  rename(notlf14_ed_blk_prop = `Not in labor force`) %>%
  dplyr::select(-c(Employed, Unemployed)) %>%
  ungroup() 

## Number of employed and unemployed White residents  
test <- data %>% 
  filter(empstat_factor!="Not in labor force" & race==1) %>%
  count(statefip, countyicp, enumdist, empstat_factor) %>%
  pivot_wider(names_from = empstat_factor, values_from=n) %>% 
  rename(emp14_ed_wht_n = `Employed`,
         unemp14_ed_wht_n = `Unemployed`) 

## Number of employed and unemployed Black residents 
test2 <- data %>% 
  filter(empstat_factor!="Not in labor force" & race==2) %>%
  count(statefip, countyicp, enumdist, empstat_factor) %>%
  pivot_wider(names_from = empstat_factor, values_from=n) %>% 
  rename(emp14_ed_blk_n = `Employed`,
         unemp14_ed_blk_n = `Unemployed`) 

emp14_ed_data <- emp14_ed_data %>%
  full_join(blk_emp14_ed_data) %>%
  full_join(wht_emp14_ed_data) %>%
  full_join(notlf14_ed_data) %>%
  full_join(notlf14_ed_wht_data) %>%
  full_join(notlf14_ed_blk_data) %>%
  full_join(test) %>%
  full_join(test2)

remove(wht_emp14_ed_data)
remove(blk_emp14_ed_data)
remove(notlf14_ed_data)
remove(notlf14_ed_wht_data)
remove(notlf14_ed_blk_data)
remove(test)
remove(test2)
gc()

## Wages
wage14_ed_data <- data %>%
  group_by(statefip, countyicp, enumdist) %>%
  summarize(wage14_ed_median = median(incwage, na.rm=TRUE),
            wage14_ed_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage14_ed_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup()  

blk_wage14_ed_data <- data %>%
  filter(race == 2) %>%
  group_by(statefip, countyicp, enumdist) %>%
  summarize(wage14_ed_blk_median = median(incwage, na.rm=TRUE),
            wage14_ed_blk_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage14_ed_blk_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

wht_wage14_ed_data <- data %>%
  filter(race == 1) %>%
  group_by(statefip, countyicp, enumdist) %>%
  summarize(wage14_ed_wht_median = median(incwage, na.rm=TRUE),
            wage14_ed_wht_25pct = quantile(incwage, c(0.25), na.rm=TRUE),
            wage14_ed_wht_75pct = quantile(incwage, c(0.75), na.rm=TRUE)) %>%
  ungroup() 

wage14_ed_data <- wage14_ed_data %>%
  full_join(blk_wage14_ed_data) %>%
  full_join(wht_wage14_ed_data)

remove(wht_wage14_ed_data)
remove(blk_wage14_ed_data)
remove(data)
gc()

# merge and save ----------------------------------------------------
# merge all to ed_hh_data
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/clean-data-2/",
                       "ipums1940_area_hh_vars.RDS"))

ed_data <- data %>% 
  left_join(educ_ed_data) %>%
  left_join(emp_ed_data) %>%
  left_join(wage_ed_data) %>%
  left_join(emp14_ed_data) %>%
  left_join(wage14_ed_data)

# check -- 8/15/2023 TMM 
# sapply(ed_data, function(x) sum(is.na(x)))
# check <- ed_data %>% filter(is.na(educ_le8_ed_prop))
# check2 <- ed_data %>% filter(is.na(educ_le8_ed_blk_n))
# indiv_data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
#                        "comm-level-social-vars/data/clean-data-2/",
#                        "ipums1940_indiv25plus_dat.RDS")) %>% 
#   mutate(gt_grade8 = ifelse(educ>=3,1,0))
# check3 <- indiv_data %>% filter(statefip==47 & countyicp==70 & enumdist==400090)
# check3 <- indiv_data %>% filter(statefip==39 & countyicp==450 & enumdist==2300130)
# remove(indiv_data)

# 8/15/2023 TMM notes:
# missingness could be due to areas with 0 people in appropriate age group or
# 0 people in racial group for race-stratified variables. Making the counts 0
# but leaving the continous variables missing in these cases since 0 would be 
# an informative value

ed_data <- ed_data %>%
  mutate_at(vars(contains("_prop")), replace_na, 0) %>%
  mutate_at(vars(contains("_n")), replace_na, 0)

# check -- 8/15/2023 TMM 
# sapply(ed_data, function(x) sum(is.na(x)))

saveRDS(ed_data, paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                        "comm-level-social-vars/data/clean-data-2/",
                        "ipums1940_ed_hh_indiv_vars.RDS"))
