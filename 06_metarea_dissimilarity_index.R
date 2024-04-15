# title: "06_metarea_dissimilarity_index"
# Code authored by: Misbath Daouda
# Code adapted by: Taylor Mobley

library(tidyverse)
library(psych)
library(sf)
library(numform)
library(viridis)
library(stringr)
options(scipen = 10)
options(digits = 10)

# Read ---------------------------------------------------------------
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/",
                       "clean-data-2/ipums1940_arealvl_hh_indiv_vars.RDS"))

# Enumeration district pop stats -------------------------------------

# 8/15/2023 TMM added dplyr to select function
data %>% 
  dplyr::select(ed_pop_tot, ed_pop_bwrace, ed_wht_pop, ed_nwht_pop, ed_blk_pop, 
         ed_am_in_pop, ed_api_pop, ed_chinese_pop, ed_japanese_pop) %>%
  psych::describe() %>% 
  dplyr::select(n, median, min, max) %>% 
  knitr::kable()

hist(data$ed_pop_tot)

# 8/15/2023 TMM checking missingness -- looks good
# check <- data %>%
#   dplyr::select(ed_pop_tot, ed_pop_bwrace, ed_wht_pop, ed_nwht_pop, ed_blk_pop,
#                 ed_am_in_pop, ed_api_pop, ed_chinese_pop, ed_japanese_pop,
#                 met_pop_tot, met_pop_bwrace, met_wht_pop, met_nwht_pop,
#                 met_blk_pop, met_am_in_pop, met_api_pop, met_chinese_pop,
#                 met_japanese_pop)
# 
# sapply(check, function(x) sum(is.na(x)))
# 
### ED pops restricted to metro areas

temp <- data %>% 
  filter(metarea !=0) %>%
  dplyr::select(ed_pop_tot, ed_pop_bwrace, ed_wht_pop, ed_nwht_pop, ed_blk_pop, 
                ed_am_in_pop, ed_api_pop, ed_chinese_pop, ed_japanese_pop)%>%
  mutate(ed_pop_tot = as.numeric(ed_pop_tot))

temp %>%
  psych::describe() %>% 
  dplyr::select(n, median, min, max)

hist(temp$ed_pop_tot)

### Metro area pop stats

data %>% filter(metarea > 0) %>%
  dplyr::select(c(metarea, met_pop_tot, met_pop_bwrace, met_wht_pop, met_nwht_pop,
           met_blk_pop, met_am_in_pop, met_api_pop, 
           met_chinese_pop, met_japanese_pop)) %>%
  distinct() %>%
  dplyr::select(met_pop_tot, met_wht_pop, met_blk_pop, met_am_in_pop,
         met_api_pop, met_chinese_pop, met_japanese_pop) %>%
  psych::describe() %>% 
  dplyr::select(n, median, min, max) %>% 
  knitr::kable()

# Note: 0 met areas have white pop < 1000. 

### Dissimilarity index recommendations
# Per reference sheet and Misbaths code, it is recommended di limited to:
# 1. Metro areas (larger geo area of interest) with more than one enumeration district (or geo subunits)
# 2. Remove eds with 0 pop for all groups considered
# 3. Remove metro areas with at least one group population < 1000

#### Number of eds per metro area
metarea_eds <- data %>% filter(metarea > 0) %>%
  group_by(metarea) %>%
  summarise(count = n_distinct(enumdist)) %>%
  ungroup() 

summary(metarea_eds$count)

# Range of eds in metro areas = 48 to 13,227

### Number of eds with Black and White pop = 0
ed_bw_all0 <- 
  data %>% filter(ed_wht_pop==0 & ed_blk_pop==0)

# 81 eds with White and Black pops = 0 

### Number of met areas with Black pop < 1000

metarea_lt1000 <- 
  data %>% filter(metarea > 0 & 
                    (met_blk_pop <1000)) 
length(unique(metarea_lt1000$metarea))
summary(metarea_lt1000$met_blk_pop) 

# 17 met areas with Black pop < 1000 (~12%). These met areas will have unstable dis.

# Met area dissimilarity calcs -----------------------------------------

# Calculation, per Life Course Indicators tip sheet from Misbath:
# D = .5*sum_i(abs(xi/X - yi/Y)) where i is the geographic subunit (eg, tracts)
# x = minority population estimate in geographic subunit
# X = minority pop in larger geographic area of interest (eg, county, met area)
# y = reference population estimate in geo submit
# Y = reference pop in larger geographic area of interest

### Propose: 
# Keep data as is, and use the white/not white pop + Black/white pop DIs.
# Once we have the data in the enclave, look to see how much our sample size 
# decreases if we impose metro area group population restrictions 
# (eg, 1000, 500, etc.)

# 8/15/2023 TMM changing filtering from an id variable (see county DI script)
# first, remove eds with Black and White pops=0
filter_data <- data %>% 
  filter(metarea > 0) %>%
  filter(!(ed_blk_pop==0 & ed_wht_pop==0)) # should have 151149 obs

## version 1: Black vs white using tip sheet (above) and Darmouth article 
## [https://www.dartmouth.edu/~segregation/IndicesofSegregation.pdf]

v1_processed_data = filter_data %>%
  mutate(ed_blk_wht_diff = 
           abs((ed_blk_pop/met_blk_pop)-(ed_wht_pop/met_wht_pop))) %>%
  group_by(metarea) %>%
  mutate(sum_blk_wht_diff = sum(ed_blk_wht_diff),
         di_bw = 0.5*sum_blk_wht_diff) %>%
  ungroup() 

di_v1_data = v1_processed_data %>%
  dplyr::select(metarea, di_bw) %>%
  distinct(metarea, .keep_all=TRUE)

## version 2: white v not white using Life Course Indicator's tip sheet from Misbath
v2_processed_data = v1_processed_data %>% 
  mutate(ed_wht_nwht_diff = 
           abs((ed_nwht_pop/met_nwht_pop)-(ed_wht_pop/met_wht_pop))) %>%
  group_by(metarea) %>%
  mutate(sum_wht_nwht_diff = sum(ed_wht_nwht_diff),
         di_wnw = 0.5*sum_wht_nwht_diff) %>%
  ungroup() 

di_v2_data = v2_processed_data %>%
  dplyr::select(metarea, di_wnw) %>%
  distinct(metarea, .keep_all=TRUE)

## merge two versions
di_data_all <- di_v1_data %>%
  left_join(di_v2_data, by=c("metarea")) %>% # 8/15/2023 TMM added join by 'metarea'
  mutate(met_di_wnw = ifelse(metarea==0,NA,di_wnw),
         met_di_bw = ifelse(metarea==0,NA,di_bw)) %>%
  mutate(met_di_wnw_cat = 
           ifelse(di_wnw < 0.3, "well integrated",
                  ifelse(di_wnw >=0.3 & di_wnw <= 0.6, "moderately segregated", 
                         ifelse(di_wnw > 0.6, "highly segregated", NA))),
         met_di_bw_cat = 
           ifelse(di_bw < 0.3, "well integrated",
                  ifelse(di_bw >=0.3 & di_bw <= 0.6, "moderately segregated", 
                         ifelse(di_bw > 0.6, "highly segregated", NA)))) %>%
  dplyr::select(-c(di_wnw, di_bw))

# merge and save ----------------------------------------------------

final_data = data %>%
  left_join(di_data_all, by=c("metarea"))

saveRDS(final_data, paste0('C:/Users/tmobley/Desktop/geo1940-crosswalks/',
                           'comm-level-social-vars/data/clean-data-2/',
                           'ipums1940_area_lvl_metarea_di_dat.RDS'))

# DI Descriptive stats ------------------------------------------------- 
# di_data_all %>% 
#   ungroup() %>%
#   select(c(di_bw, di_wnw)) %>%
#   psych::describe() %>% 
#   select(n, median, min, max, mean, sd) %>% 
#   knitr::kable()
# 
# final_data %>% filter(!is.na(di_bw)) %>%
#   mutate(ed_pop_tot_cat = ifelse(ed_pop_tot < 100," <100",">=100")) %>%
#   ggplot(aes(x=ed_blk_pop, y=di_bw)) + geom_point() + 
#   facet_wrap(.~ed_pop_tot_cat)
# 
# final_data %>% filter(!is.na(di_bw)) %>%
#   mutate(ed_blk_pop_cat = ifelse(ed_blk_pop < 100," <100",">=100")) %>%
#   ggplot(aes(x=ed_blk_pop, y=di_bw, color=ed_pop_tot)) +
#   geom_jitter() +
#   facet_wrap(.~ed_blk_pop_cat)
# 
# ## From tip sheet, DI can be categorized into the following:
# ### DI < 0.3 = "well integrated"
# ### 0.3 <= DI <= 0.6 = "moderately segregated"
# ### DI > 0.6 = "highly segregated"
# 
# table(di_data_all$di_bw_cat, exclude=NULL)
# table(di_data_all$di_wnw_cat, exclude=NULL)
# 
# ## DI - Histograms 
# 
# ### DI, version 1: Black vs white DI using references cited above
# 
# di_data_all %>% 
#   filter(metarea!=0) %>%
#   ggplot(aes(x = di_bw)) + 
#   geom_histogram(bins=50) + 
#   labs(x = "Dissimilarity Index, v3", y = "Count", title = "")
# 
# ### DI, version 2: Not white vs white DI using references cited above
# 
# di_data_all %>% 
#   filter(metarea!=0) %>%
#   ggplot(aes(x = di_wnw)) + 
#   geom_histogram(bins=50) + 
#   labs(x = "Dissimilarity Index, v2", y = "Count", title = "")