# title: "07_county_dissimilarity_index"
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

data <- readRDS(paste0('C:/Users/tmobley/Desktop/geo1940-crosswalks/',
                       'comm-level-social-vars/data/clean-data-2/',
                       'ipums1940_area_lvl_metarea_di_dat.RDS'))

# Enumeration district pop stats --------------------------------------
data %>% 
  dplyr::select(ed_pop_tot, ed_pop_bwrace, ed_wht_pop, ed_nwht_pop, ed_blk_pop, 
         ed_am_in_pop, ed_api_pop, ed_chinese_pop, ed_japanese_pop) %>%
  psych::describe() %>% 
  dplyr::select(n, median, min, max) %>% 
  knitr::kable()

hist(data$ed_pop_tot)

# County pop stats

data %>%
  dplyr::select(c(statefip, countyicp, cnty_pop_tot, cnty_pop_bwrace, 
                  cnty_wht_pop, cnty_nwht_pop, cnty_blk_pop, cnty_am_in_pop, 
                  cnty_api_pop, cnty_chinese_pop, cnty_japanese_pop)) %>%
  distinct() %>%
  dplyr::select(cnty_pop_tot, cnty_wht_pop, cnty_blk_pop, cnty_am_in_pop,
         cnty_api_pop, cnty_chinese_pop, cnty_japanese_pop) %>%
  psych::describe() %>% 
  dplyr::select(n, median, min, max) 

## Dissimilarity index specs
# Per reference sheet and Misbaths code, it is recommended dis are limited to:
# 1. Counties (larger geo area of interest) with more than one enumeration district (or geo subunits)
# 2. Remove eds with Black and White pop = 0
# 3. Remove counties with at least one group population < 1000
# Note: the reference sheet recommends using 50,000 or 10,000 as county pop threshold. 
# I look at these counties but do not exclude them. We should revisit this later!
  
### Number of eds per county
county_eds <- data %>% 
  group_by(statefip, countyicp) %>%
  mutate(count = n_distinct(enumdist)) %>%
  dplyr::select(c(statefip, countyicp, count, cnty_blk_pop, cnty_wht_pop, 
                  cnty_pop_tot)) %>%
  ungroup() 

summary(county_eds$count)

county_1ed <- county_eds %>% filter(count==1)

# Range of eds in counties = 1 to 3,797
# Four counties have 1 ed -- will remove them from county-level DI calculation.

# 8/15/2023 TMM removed state + county + ed id var
### Number of eds with Black and White pop = 0
ed_bw0 <- 
  data %>% filter(ed_wht_pop==0 & ed_blk_pop==0) %>%
  dplyr::select(c(statefip, countyicp, enumdist))

# 81 eds with Black and White pops = 0. Remove these from calculation

### Number of counties with total pop < 10,000
county_lt10000 <- 
  data %>% 
  group_by(statefip, countyicp) %>% 
  filter(cnty_pop_tot<10000) %>%
  distinct(statefip, countyicp, .keep_all=TRUE) %>%
  ungroup() 

county_lt10000 %>% 
  dplyr::select(cnty_pop_tot, cnty_wht_pop, cnty_blk_pop) %>%
  psych::describe() %>% 
  dplyr::select(n, median, min, max) %>% 
  knitr::kable()

# 715 counties have total pop < 10,000. 

### Number of counties with total pop < 5,000

county_lt5000 <- 
  data %>% 
  group_by(statefip, countyicp) %>% 
  filter(cnty_pop_tot<5000) %>%
  distinct(statefip, countyicp, .keep_all=TRUE) %>%
  ungroup() # 8/15/2023 TMM adding ungroup

county_lt5000 %>% 
  dplyr::select(cnty_wht_pop, cnty_blk_pop) %>%
  psych::describe() %>% 
  dplyr::select(n, median, min, max) %>% 
  knitr::kable()

# 241 counties have total population < 5,000. 

### Number of counties with White pop < 1000

county_w_lt1000 <- 
  data %>% filter(cnty_wht_pop < 1000) %>%
  distinct(statefip, countyicp, .keep_all = TRUE)

# 11 counties have White pop < 1000.

### Number of counties with Black pop < 1000

county_b_lt1000 <- 
  data %>% filter(cnty_blk_pop < 1000) %>%
  distinct(statefip, countyicp, .keep_all = TRUE)

county_b_lt1000 %>% 
  dplyr::select(cnty_wht_pop, cnty_blk_pop) %>%
  psych::describe() %>% 
  dplyr::select(n, median, min, max) %>% 
  knitr::kable()

# 1,929 counties have Black population < 1,000.

### Number of counties with Black pop = 0
# 8/15/2023 TMM removed state + county id var
county_b0 <- 
  data %>% 
  filter(cnty_blk_pop==0) %>%
  distinct(statefip, countyicp, .keep_all=TRUE) %>%
  dplyr::select(statefip, countyicp, cnty_blk_pop, cnty_wht_pop, ed_blk_pop,
                ed_wht_pop)
  
# [Note: no counties have 0 White people]
# 422 counties with 0 Black people. Should remove these.

# County dissimilarity calcs ----------------------------------------

# Calculation, per Life Course Indicators tip sheet from Misbath:
# D = .5*sum_i(abs(xi/X - yi/Y)) where i is the geographic subunit (eg, tracts)
# x = minority population estimate in geographic subunit
# X = minority pop in larger geographic area of interest (eg, county, met area)
# y = reference population estimate in geo submit
# Y = reference pop in larger geographic area of interest

# v2: TMM changing filtering because ids are not unique 

filter_data <- data %>%
  filter(cnty_blk_pop!=0) %>%
  group_by(statefip, countyicp) %>%
  mutate(count = n_distinct(enumdist)) %>%
  filter(count!=1) %>% 
  ungroup()

# 8/15/2023 TMM adding check to see if any eds have 0 black and 0 white pop
# check <- filter_data %>% filter(ed_blk_pop==0 & ed_wht_pop==0) # 77 obs
# check2 <- data %>% distinct(statefip, countyicp) # 3099 counties
# check3 <- filter_data %>% distinct(statefip, countyicp) # 2677 counties

# should have 141481 - 77 = 141404 obs
filter_data <- filter_data %>% 
  filter(!(ed_blk_pop==0 & ed_wht_pop==0))

# Note: we shouldn't lose any counties because we get rid of counties with 1 ed
# check4 <- filter_data %>% distinct(statefip, countyicp) # 2669 counties

# version 1: Black vs white using tip sheet (above) and Darmouth article [https://www.dartmouth.edu/~segregation/IndicesofSegregation.pdf]
v1_processed_data = filter_data %>%
  mutate(ed_blk_wht_diff = 
           abs((ed_blk_pop/cnty_blk_pop)-(ed_wht_pop/cnty_wht_pop))) %>%
  group_by(statefip, countyicp) %>%
  mutate(sum_blk_wht_diff = sum(ed_blk_wht_diff),
         cnty_di_bw = 0.5*sum_blk_wht_diff) %>%
  ungroup() # 8/15/2023 TMM adding ungroup

di_v1_data = v1_processed_data %>%
  dplyr::select(statefip, countyicp, cnty_di_bw) %>%
  distinct(statefip, countyicp, .keep_all=TRUE)

# version 2: white v not white using Life Course Indicators tip sheet from Misbath
v2_processed_data = v1_processed_data %>% 
  mutate(ed_wht_nwht_diff = 
           abs((ed_nwht_pop/cnty_nwht_pop)-(ed_wht_pop/cnty_wht_pop))) %>%
  group_by(statefip, countyicp) %>%
  mutate(sum_wht_nwht_diff = sum(ed_wht_nwht_diff),
         cnty_di_wnw = 0.5*sum_wht_nwht_diff) %>%
  ungroup() # 8/15/2023 TMM adding ungroup

di_v2_data = v2_processed_data %>%
  dplyr::select(statefip, countyicp, cnty_di_wnw) %>%
  distinct(statefip, countyicp, .keep_all=TRUE)

# merge two versions
di_data_all <- di_v1_data %>%
  left_join(di_v2_data) %>% 
  mutate(cnty_di_wnw_cat = 
           ifelse(cnty_di_wnw >0 & cnty_di_wnw < 0.3, "well integrated",
                  ifelse(cnty_di_wnw >=0.3 & cnty_di_wnw <= 0.6, "moderately segregated", 
                         ifelse(cnty_di_wnw > 0.6, "highly segregated", NA))),
         cnty_di_bw_cat = 
           ifelse(cnty_di_bw >0 & cnty_di_bw < 0.3, "Well integrated",
                  ifelse(cnty_di_bw >=0.3 & cnty_di_bw <= 0.6, "Moderately segregated", 
                         ifelse(cnty_di_bw > 0.6, "Highly segregated", NA))))

# merge three versions with ed data set
final_data = data %>%
  left_join(di_data_all) 

# save final data set -------------------------------------------------

saveRDS(final_data, paste0('C:/Users/tmobley/Desktop/geo1940-crosswalks/',
                           'comm-level-social-vars/data/clean-data-2/',
                           'ipums1940_area_level_dat.RDS'))

# DI Descriptive stats ---------------------------------------------

# di_data_all %>% 
#   ungroup() %>%
#   select(c(cnty_di_bw, cnty_di_wnw)) %>%
#   psych::describe() %>% 
#   select(n, median, min, max, mean, sd) %>% 
#   knitr::kable()
# 
# final_data %>% filter(!is.na(cnty_di_bw)) %>%
#   mutate(ed_pop_tot_cat = ifelse(ed_pop_tot < 100," <100",">=100")) %>%
#   ggplot(aes(x=ed_blk_pop, y=cnty_di_bw)) + geom_point() + 
#   facet_wrap(.~ed_pop_tot_cat)
# 
# final_data %>% filter(!is.na(cnty_di_bw)) %>%
#   mutate(ed_blk_pop_cat = ifelse(ed_blk_pop < 100," <100",">=100")) %>%
#   ggplot(aes(x=ed_blk_pop, y=cnty_di_bw, color=ed_pop_tot)) +
#   geom_jitter() +
#   facet_wrap(.~ed_blk_pop_cat)
# 
# # From tip sheet, DI can be categorized into the following:
# # - DI < 0.3 = "well integrated"
# # - 0.3 <= DI <= 0.6 = "moderately degregated"
# # - DI > 0.6 = "highly segregated"
# 
# table(di_data_all$cnty_di_bw_cat, exclude=NULL)
# table(di_data_all$cnty_di_wnw_cat, exclude=NULL)
# 
# # DI Histograms ----------------------------------------------------- 
# 
# ## DI, version 1: Black vs white DI using references cited above
# 
# di_data_all %>% 
#   ggplot(aes(x = cnty_di_bw)) + 
#   geom_histogram() + 
#   labs(x = "County dissimilarity Index, v3", y = "Count", title = "")
# 
# ## DI, version 2: Not white vs white DI using references cited above
# 
# di_data_all %>% 
#   ggplot(aes(x = cnty_di_wnw)) + 
#   geom_histogram() + 
#   labs(x = "County dissimilarity Index, v2", y = "Count", title = "")