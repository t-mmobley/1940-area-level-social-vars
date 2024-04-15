# Libraries --------------------------------------------------------
library(tidyverse)
library(ipumsr)
library(psych)
# Read -------------------------------------------------------------
ddi <- read_ipums_ddi(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                             "comm-level-social-vars/data/",
                             "raw-data/usa_00023.dat/usa_00023.xml"))
data <- read_ipums_micro(ddi)

# Format data ------------------------------------------------------

# lower case colnames
colnames(data) <- tolower(colnames(data))
gc()

# set missing and create factor vars
# unique(data$educ) # 0-11, 99
# unique(data$ownershp) # 0,1,2 / 0 = NA in IPUMS
# unique(data$rent) # seems continuous
# range(data$rent) # range 0-9999 / 9997 top code in 1940 per IPUMS
# unique(data$valueh) # seems continuous / per IPUMS, it is in 1940
# range(data$valueh) # range 1-9999999 / 9999998 = NA in IPUMS
# unique(data$race) # 1,2,3,4,5,6
# unique(data$hispan) # 0,1,2,3,4
# unique(data$nativity) # 0,1,2,3,4,5
# unique(data$empstat) # 0,1,2,3 / 0 = NA per IPUMS
# unique(data$incwage) # seems continuous
# range(data$incwage) # 0-999999 / 9999998, 9999999 NA or missing in IPUMS
# unique(data$sei) # seems continuous
# range(data$sei) # 0-96
# unique(data$age) # seems continuous
# range(data$age) # range 0-120
# unique(data$relate) # 1-13 / 1 = HoH

# 8/14/2023 TMM adding gq code check
# table(data$gq, data$gq_factor, exclude=NULL) 
# GQ codes: 1 = HHs under 1970 def, 2 = HHs under 1990 def, 3 = GQ institutions
# 4 = other group quarters, 6 = fragments

# data <- data %>%
#   dplyr::select(-c(raced, hispand, ownershpd, educd, empstatd, related))

data <- data %>%
  mutate(educ_factor = as_factor(lbl_clean(educ)),
         relate_factor = as_factor(lbl_clean(relate))) %>% ## from int + lbl to fct
  mutate(educ = ifelse(educ %in% c(99),NA,educ), ## int
         educ_factor = ifelse(educ_factor %in% c('99'),NA,educ_factor), ## this stops being a factor -> int
         rent = ifelse(rent %in% c(9998,9999),NA,rent), ## from int + lbl to int, follow documentation rules, labels are wrong
         valueh = ifelse(valueh %in% c(9999998, 9999999),NA,valueh), ## from int + lbl to int
         incwage = ifelse(incwage %in% c(999998, 999999), NA, incwage)) %>% ## from dbl + lbl, to dbl
  mutate(ownershp_factor = as_factor(lbl_clean(ownershp)), ## int+lbl to fct, has a N/A category
         race_factor = as_factor(race),## int+lbl to fct
         hispan_factor = as_factor(hispan), ## int+lbl to fct
         empstat_factor = as_factor(empstat), ## int+lbl to fct, has a N/a category
         gq_factor = as_factor(gq))

remove(ddi)
gc()

# checking relationship betwenn metarea and metaread codes
# metarea vs metaread -- all 1:1 matches
# metareas <- data %>% count(metarea, metaread) #138 obs
# length(unique(metareas$metarea)) #138, including 0
# length(unique(metareas$metaread)) #138, including 0

colnames(data)

data <- data %>%
  dplyr::select(-c(ends_with("d"))) %>%
  mutate(pop25plus = ifelse(!is.na(age) & age>=25,1,0),
         pop16plus = ifelse(!is.na(age) & age>=16,1,0),
         pop14plus = ifelse(!is.na(age) & age>=14,1,0))

gc()

# Race variable labels:
# 
# 1     White
# 2     Black/African American
# 3     American Indian or Alaska Native
# 4     Chinese
# 5     Japanese
# 6     Other Asian or Pacific Islander
# 
# Ethnicity variable labels:
# 
# 0     Not Hispanic
# 1     Mexican
# 2     Puerto Rican
# 3     Cuban
# 4     Other

# table(data$race, data$hispan, exclude=NULL)

data$raceth = ifelse(data$hispan!=0,5, #hispanic 1.6% of the sample
               ifelse(data$hispan==0 & data$race==1,1, #non hispanic white
                  ifelse(data$hispan==0 & data$race==2,2, #non hispanic black
                     ifelse(data$hispan==0 & data$race==3,3, #non hispanic AIAN
                        ifelse(data$hispan==0 & data$race %in% c(4,5,6),4,NA)))))

gc()

# create unique hh data set ------------------------------------------
colnames(data)

# serial = household identifier and relate = relationship to head of HH
# I am keeping relate and race so that I can create race-stratified HH vars
# arranging by relate ideally keeps head of HH (although not every HH has relate=1)
hh_data <- data %>% 
  dplyr::select(sample, serial, statefip, countyicp, metarea, enumdist, #metaread, PRS 08/17/23
           gq, gq_factor, rent, valueh, ownershp, ownershp_factor, 
           relate, relate_factor, race) %>% 
  arrange(sample, serial, relate) %>%
  distinct(sample, serial, .keep_all=TRUE)

hh_data <- as_tibble(hh_data)

# LPRS - check that there is only one row per sample/household
# hh_data %>% count(sample, serial) %>% dim()
# hh_data %>% count(sample, serial) %>% summarize(sum(n))
# Testing another way of doing this: 
# test <- data %>% 
#   select(c(sample, serial, statefip, countyicp, metarea, enumdist, #metaread, PRS 08/17/23
#            gq, gq_factor, rent, valueh, ownershp, ownershp_factor, 
#            relate, relate_factor, raceth)) %>% 
#   group_by(sample, serial) %>%
#   arrange(relate) %>% 
#   slice(1) %>% 
#   ungroup()

# identical(test, hh_data) TRUE, YOOHOO

# hh_data %>% count(relate)
# data %>% count(relate) same number of Head/householder

# check -- 8/14/2023 TMM adding a check of relate
# table(hh_data$relate, exclude = NULL)
# table(hh_data$relate, hh_data$gq, exclude = NULL)

# Save
saveRDS(hh_data, file=paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                             "comm-level-social-vars/data/",
                             "clean-data-2/ipums1940_hh_dat.RDS"))

# then remove
remove(hh_data)
gc()

# filter and save individual-level data set --------------------------
data <- data %>% 
  dplyr::select(-c(sample, gq, gq_factor, rent, valueh, ownershp, 
                  ownershp_factor, race_factor))
# Save
saveRDS(data, file=paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                             "comm-level-social-vars/data/",
                             "clean-data-2/ipums1940_indiv_dat.RDS"))

# greater than or equal to 16
data16plus <- data %>%
  filter(age>=16)

gc()

# Save
saveRDS(data16plus, file=paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                          "comm-level-social-vars/data/",
                          "clean-data-2/ipums1940_indiv16plus_dat.RDS"))

remove(data16plus)
gc()

# greater than or equal to 14
data14plus <- data %>%
  filter(age>=14)

# Save
saveRDS(data14plus, file=paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                                "comm-level-social-vars/data/",
                                "clean-data-2/ipums1940_indiv14plus_dat.RDS"))

remove(data14plus)
gc()

# greater than or equal to 25
data25plus <- data %>%
  filter(age>=25)

# Save
saveRDS(data25plus, file=paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                                "comm-level-social-vars/data/",
                                "clean-data-2/ipums1940_indiv25plus_dat.RDS"))
