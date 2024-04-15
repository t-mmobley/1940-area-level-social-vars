# title: "03_area_level_hh_vars"
# author: "Taylor Mobley"
  
library(tidyverse)
library(psych)
library(epiDisplay)
library(ggplot2)
library(ggpubr)
options(scipen = 10)
options(digits = 10)

# Read -------------------------------------------------------------
hh_data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                          "comm-level-social-vars/data/clean-data-2/",
                          "ipums1940_hh_dat.RDS"))

tots <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                       "comm-level-social-vars/data/",
                       "clean-data-2/ipums1940_pop_hh_counts.RDS"))
gc()

# Household-level variable descriptive stats -------------------------

# ## 1. Home ownership 
# 
# ### Desired variable: owner-occupied housing units, percent of total housing units
# 
# table(hh_data$ownershp_factor, hh_data$gq_factor, exclude=NULL)
# table(hh_data$ownershp, hh_data$gq_factor, exclude=NULL)
# tab1(hh_data$ownershp_factor, cum.percent = TRUE)
# 
# ## 2. Home value 
# 
# ### Desired measure: median home value
# 
# gc()
# hh_data %>%
#   filter(ownershp==1) %>%
#   summarize(n = n(),
#             min_valueh = min(valueh, na.rm=TRUE),
#             iqr_valueh = IQR(valueh, na.rm=TRUE),
#             median_valueh = median(valueh, na.rm=TRUE),
#             max_valueh = max(valueh, na.rm=TRUE),
#             mean_valueh = mean(valueh, na.rm=TRUE),
#             sd_valueh = sd(valueh, na.rm=TRUE),
#             ptile20_valueh = quantile(valueh, na.rm=T, probs=0.2), #600
#             ptile80_valueh = quantile(valueh, na.rm=T, probs=0.8)) #4750
# 
# ## 3. Rent [note: top-coded at 9,997 in 1940]
# 
# ### Desired variable: median gross monthly rent
# 
# gc()
# hh_data %>%
#   group_by(ownershp_factor) %>%
#   summarize(n = n(),
#             min_rent = min(rent, na.rm=TRUE),
#             iqr_rent = IQR(rent, na.rm=TRUE),
#             median_rent = median(rent, na.rm=TRUE),
#             max_rent = max(rent, na.rm=TRUE),
#             mean_rent = mean(rent, na.rm=TRUE),
#             sd_rent = sd(rent, na.rm=TRUE))

# Household summary measures ----------------------------------------

# hh_data %>% count(ownershp_factor, rent==0) 

# check rent distribution when home is owned -- all 0. Don't count these!
# check <- hh_data %>%filter(ownershp==1) %>%
#   summarize(min = min(rent),
#             max = max(rent),
#             median = median(rent))

# check rent distribution when gq -- also all 0. Shouldn't count these
# check <- hh_data %>%filter(ownershp==0) %>%
#   summarize(min = min(rent),
#             max = max(rent),
#             median = median(rent))

# check home value when proprty is rented -- all NAs. Shouldn't affect stats
# check <- hh_data %>%filter(ownershp==2) %>%
#   summarize(min = min(valueh),
#             max = max(valueh),
#             median = median(valueh))

# check home value when gq. -- all NAs. Should be ok
# check <- hh_data %>%filter(ownershp==0) %>%
#   summarize(min = min(valueh),
#             max = max(valueh),
#             median = median(valueh))

# checking extreme median value from summary data -- TMM 2/1/2023
# check2 <- hh_data %>% 
#   filter(statefip==55 & countyicp==1330 & race %in% c(1,2)) %>% 
#   dplyr::select(c(race, valueh, relate_factor, gq_factor, ownershp_factor))

# format data -------------------------------------------------------
# make rent = NA when ownership !=2
hh_data <- hh_data %>%
  mutate(rent = ifelse(ownershp==2,rent,NA))
gc()

# ED level ----------------------------------------------------------

# TMM -- for Paloma, I calculate prop owned and prop rented excluding dwellings outside of HHs
# (at least that's my intention). I think this makes sense based on other documentation I've read
# but let me know if you disagree! 
# Additionally, prop gq variables are just the proportion of ALL DWELLINGS that are group quarters
# or "fragments". I don't think we will use this variable, but flagging that it is a different/
# not comparable with prop rented and prop owned. 

## First overall
test <- hh_data %>% 
  count(statefip, countyicp, enumdist, ownershp_factor) %>%
  pivot_wider(names_from = ownershp_factor, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(ed_prop_owned = `Owned or being bought (loan)`/(`Owned or being bought (loan)`+Rented),
         ed_prop_rented = Rented/(`Owned or being bought (loan)`+Rented),
         ed_prop_gq = `N/A`/(`Owned or being bought (loan)` + Rented + `N/A`)) %>% # Not owned/rented = GQs or fragments
  dplyr::select(-c(`Owned or being bought (loan)`, Rented, `N/A`)) %>%
  replace(is.na(.), 0) 

## Splitting it because it takes too long to run

## Valueh
test2_valueh <- hh_data %>% 
  filter(ownershp!=0) %>%
  group_by(statefip, countyicp, enumdist, ownershp_factor) %>%
  summarize(valueh_ed_median = median(valueh, na.rm=TRUE),
            valueh_ed_25pct = quantile(valueh, c(0.25), na.rm=TRUE),
            valueh_ed_75pct = quantile(valueh, c(0.75), na.rm=TRUE)) %>%
  ungroup() %>%   
  pivot_wider(names_from = ownershp_factor, 
              values_from = c(valueh_ed_median,valueh_ed_25pct,valueh_ed_75pct))

test2_valueh <- test2_valueh %>% 
  dplyr::select(-c(valueh_ed_median_Rented, valueh_ed_25pct_Rented,
                   valueh_ed_75pct_Rented)) %>%
  rename(valueh_ed_median = `valueh_ed_median_Owned or being bought (loan)`,
         valueh_ed_25pct = `valueh_ed_25pct_Owned or being bought (loan)`,
         valueh_ed_75pct = `valueh_ed_75pct_Owned or being bought (loan)`) 

##Rent
test2_rent <- hh_data %>%
  filter(ownershp != 0) %>%
  group_by(statefip, countyicp, enumdist, ownershp_factor) %>%
  summarize(
    rent_ed_median = median(rent, na.rm = TRUE),
    rent_ed_25pct = quantile(rent, c(0.25), na.rm = TRUE),
    rent_ed_75pct = quantile(rent, c(0.75), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  pivot_wider(
    names_from = ownershp_factor,
    values_from = c(rent_ed_median,
                    rent_ed_25pct,
                    rent_ed_75pct)) 

test2_rent <- test2_rent %>% 
  dplyr::select(-c(`rent_ed_median_Owned or being bought (loan)`,
                   `rent_ed_25pct_Owned or being bought (loan)`,
                   `rent_ed_75pct_Owned or being bought (loan)`)) %>%
  rename(rent_ed_median = `rent_ed_median_Rented`,
         rent_ed_25pct = `rent_ed_25pct_Rented`,
         rent_ed_75pct = `rent_ed_75pct_Rented`)

## Now by race
test3 <- hh_data %>% 
  filter(race %in% c(1,2)) %>%
  count(statefip, countyicp, enumdist, ownershp_factor, race) %>%
  pivot_wider(names_from=c(ownershp_factor, race), values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(ed_wht_prop_owned = `Owned or being bought (loan)_1`/(`Owned or being bought (loan)_1`+Rented_1),
         ed_wht_prop_rented = Rented_1/(`Owned or being bought (loan)_1`+Rented_1),
         ed_wht_prop_gq = `N/A_1`/(`Owned or being bought (loan)_1` + Rented_1 + `N/A_1`),
         ed_blk_prop_owned = `Owned or being bought (loan)_2`/(`Owned or being bought (loan)_2`+Rented_2),
         ed_blk_prop_rented = Rented_2/(`Owned or being bought (loan)_2`+Rented_2),
         ed_blk_prop_gq = `N/A_2`/(`Owned or being bought (loan)_2` + Rented_2 + `N/A_2`)) %>% 
  dplyr::select(-c(`Owned or being bought (loan)_1`, Rented_1, `N/A_1`,
                   `Owned or being bought (loan)_2`, Rented_2, `N/A_2`)) %>%
  ungroup() %>% 
  replace(is.na(.), 0) 

# check -- 8/15/2023 TMM 
# sapply(test3, function(x) sum(is.na(x)))

test4_valueh <- hh_data %>% 
  filter(ownershp!=0 & race %in% c(1,2)) %>%
  group_by(statefip, countyicp, enumdist, ownershp_factor, race) %>%
  summarize(valueh_ed_median = median(valueh, na.rm=TRUE),
            valueh_ed_25pct = quantile(valueh, c(0.25), na.rm=TRUE),
            valueh_ed_75pct = quantile(valueh, c(0.75), na.rm=TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = c(ownershp_factor, race), 
              values_from = c(valueh_ed_median,valueh_ed_25pct,valueh_ed_75pct)) 
gc()

colnames(test4_valueh)
test4_valueh <- test4_valueh %>% 
  dplyr::select(-c(valueh_ed_median_Rented_1, valueh_ed_25pct_Rented_1,
                   valueh_ed_75pct_Rented_1,
                   valueh_ed_median_Rented_2, valueh_ed_25pct_Rented_2,
                   valueh_ed_75pct_Rented_2)) %>%
  rename(valueh_wht_ed_median = `valueh_ed_median_Owned or being bought (loan)_1`,
         valueh_wht_ed_25pct = `valueh_ed_25pct_Owned or being bought (loan)_1`,
         valueh_wht_ed_75pct = `valueh_ed_75pct_Owned or being bought (loan)_1`,
         valueh_blk_ed_median = `valueh_ed_median_Owned or being bought (loan)_2`,
         valueh_blk_ed_25pct = `valueh_ed_25pct_Owned or being bought (loan)_2`,
         valueh_blk_ed_75pct = `valueh_ed_75pct_Owned or being bought (loan)_2`)
gc()

### rent
test4_rent <- hh_data %>% 
  filter(ownershp!=0 & race %in% c(1,2)) %>%
  group_by(statefip, countyicp, enumdist, ownershp_factor, race) %>%
  summarize(
    rent_ed_median = median(rent, na.rm = TRUE),
    rent_ed_25pct = quantile(rent, c(0.25), na.rm = TRUE),
    rent_ed_75pct = quantile(rent, c(0.75), na.rm = TRUE)) %>%
  ungroup() %>% 
  pivot_wider(names_from = c(ownershp_factor, race), 
              values_from = c(rent_ed_median, rent_ed_25pct,rent_ed_75pct))
gc()

colnames(test4_rent)
test4_rent <- test4_rent %>% 
  dplyr::select(-c(`rent_ed_median_Owned or being bought (loan)_1`,
                   `rent_ed_25pct_Owned or being bought (loan)_1`,
                   `rent_ed_75pct_Owned or being bought (loan)_1`,
                   `rent_ed_median_Owned or being bought (loan)_2`,
                   `rent_ed_25pct_Owned or being bought (loan)_2`,
                   `rent_ed_75pct_Owned or being bought (loan)_2`)) %>%
  rename(rent_wht_ed_median = `rent_ed_median_Rented_1`,
         rent_wht_ed_25pct = `rent_ed_25pct_Rented_1`,
         rent_wht_ed_75pct = `rent_ed_75pct_Rented_1`,
         rent_blk_ed_median = `rent_ed_median_Rented_2`,
         rent_blk_ed_25pct = `rent_ed_25pct_Rented_2`,
         rent_blk_ed_75pct = `rent_ed_75pct_Rented_2`)
gc()

## 8/15/2023 TMM removed valueh percentile counts because no longer using for ICE
## Number of white hhs >= 80th percentile home value 
## Number of black hhs <=20th percentile home value

## Number of white hh owners 
test5 <- hh_data %>% 
  filter(ownershp==1 & race==1) %>%
  count(statefip, countyicp, enumdist) %>%
  rename(ownhh_wht_ed = n)

## Number of black hh renters
test6 <- hh_data %>% 
  filter(ownershp == 2 & race == 2) %>%
  count(statefip, countyicp, enumdist) %>%
  rename(renthh_blk_ed=n)

hh_ed_data <- test %>%
  left_join(test2_valueh) %>%
  left_join(test2_rent) %>%
  left_join(test3) %>%
  left_join(test4_valueh) %>%
  left_join(test4_rent) %>%
  left_join(test5) %>%
  left_join(test6) %>%
  mutate(ownhh_wht_ed = replace_na(ownhh_wht_ed, 0),
         renthh_blk_ed = replace_na(renthh_blk_ed, 0))

remove(test)
remove(test2_valueh)
remove(test2_rent)
remove(test3)
remove(test4_valueh)
remove(test4_rent)
remove(test5)
remove(test6)
gc()

# check -- 8/15/2023 TMM
# sapply(hh_ed_data, function(x) sum(is.na(x)))
# check <- hh_ed_data %>% filter(is.na(ed_wht_prop_owned))
# check2 <- hh_data %>% filter(statefip==4 & countyicp==10 & enumdist==100111)

# remaining count variable missingness due to enum dists with no White or Black participants
hh_ed_data <- hh_ed_data %>% 
  mutate(ed_wht_prop_owned = replace_na(ed_wht_prop_owned,0),
         ed_wht_prop_rented = replace_na(ed_wht_prop_rented,0),
         ed_wht_prop_gq = replace_na(ed_wht_prop_gq,0),    
         ed_blk_prop_owned = replace_na(ed_blk_prop_owned,0),
         ed_blk_prop_rented = replace_na(ed_blk_prop_rented,0),      
         ed_blk_prop_gq = replace_na(ed_blk_prop_gq,0))

# County-level ------------------------------------------------------

## First Overall
test <- hh_data %>% 
  count(statefip, countyicp, ownershp_factor) %>% 
  pivot_wider(names_from = ownershp_factor, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(cnty_prop_owned = `Owned or being bought (loan)`/(`Owned or being bought (loan)`+Rented),
         cnty_prop_rented = Rented/(`Owned or being bought (loan)`+Rented),
         cnty_prop_gq = `N/A`/(`Owned or being bought (loan)` + Rented + `N/A`)) %>% 
  dplyr::select(-c(`Owned or being bought (loan)`, Rented, `N/A`))

## not diving, it take less time because there are less rows
test2 <- hh_data %>% 
  filter(ownershp!=0) %>%
  group_by(statefip, countyicp, ownershp_factor) %>%
  summarize(valueh_cnty_median = median(valueh, na.rm=TRUE),
            valueh_cnty_25pct = quantile(valueh, c(0.25), na.rm=TRUE),
            valueh_cnty_75pct = quantile(valueh, c(0.75), na.rm=TRUE),
            rent_cnty_median = median(rent, na.rm=TRUE),
            rent_cnty_25pct = quantile(rent, c(0.25), na.rm=TRUE),
            rent_cnty_75pct = quantile(rent, c(0.75), na.rm=TRUE)) %>%
  pivot_wider(names_from = ownershp_factor, 
              values_from = c(valueh_cnty_median,valueh_cnty_25pct,
                              valueh_cnty_75pct,rent_cnty_median,
                              rent_cnty_25pct,rent_cnty_75pct)) %>%
  ungroup()
  
gc()
# head(test2)
# colnames(test2)
test2 <- test2 %>% 
  dplyr::select(-c(valueh_cnty_median_Rented, valueh_cnty_25pct_Rented,
                   valueh_cnty_75pct_Rented,
                   `rent_cnty_median_Owned or being bought (loan)`,
                   `rent_cnty_25pct_Owned or being bought (loan)`,
                   `rent_cnty_75pct_Owned or being bought (loan)`)) %>%
  rename(valueh_cnty_median = `valueh_cnty_median_Owned or being bought (loan)`,
         valueh_cnty_25pct = `valueh_cnty_25pct_Owned or being bought (loan)`,
         valueh_cnty_75pct = `valueh_cnty_75pct_Owned or being bought (loan)`,
         rent_cnty_median = `rent_cnty_median_Rented`,
         rent_cnty_25pct = `rent_cnty_25pct_Rented`,
         rent_cnty_75pct = `rent_cnty_75pct_Rented`)


## Now by race
test3 <- hh_data %>% group_by(statefip, countyicp) %>% 
  filter(race %in% c(1,2)) %>%
  count(ownershp_factor, race) %>%
  pivot_wider(names_from=c(ownershp_factor, race), values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(cnty_wht_prop_owned = `Owned or being bought (loan)_1`/(`Owned or being bought (loan)_1`+Rented_1),
         cnty_wht_prop_rented = Rented_1/(`Owned or being bought (loan)_1`+Rented_1),
         cnty_wht_prop_gq = `N/A_1`/(`Owned or being bought (loan)_1` + Rented_1 + `N/A_1`),
         cnty_blk_prop_owned = `Owned or being bought (loan)_2`/(`Owned or being bought (loan)_2`+Rented_2),
         cnty_blk_prop_rented = Rented_2/(`Owned or being bought (loan)_2`+Rented_2),
         cnty_blk_prop_gq = `N/A_2`/(`Owned or being bought (loan)_2` + Rented_2 + `N/A_2`)) %>% 
  dplyr::select(-c(`Owned or being bought (loan)_1`, Rented_1, `N/A_1`,
                   `Owned or being bought (loan)_2`, Rented_2, `N/A_2`)) %>%
  ungroup() %>% 
  replace(is.na(.), 0)

test4 <- hh_data %>% 
  filter(ownershp!=0 & race %in% c(1,2)) %>%
  group_by(statefip, countyicp, ownershp_factor, race) %>%
  summarize(valueh_cnty_median = median(valueh, na.rm=TRUE),
            valueh_cnty_25pct = quantile(valueh, c(0.25), na.rm=TRUE),
            valueh_cnty_75pct = quantile(valueh, c(0.75), na.rm=TRUE),
            rent_cnty_median = median(rent, na.rm=TRUE),
            rent_cnty_25pct = quantile(rent, c(0.25), na.rm=TRUE),
            rent_cnty_75pct = quantile(rent, c(0.75), na.rm=TRUE)) %>%
  pivot_wider(names_from = c(ownershp_factor, race), 
              values_from = c(valueh_cnty_median,valueh_cnty_25pct,valueh_cnty_75pct,
                              rent_cnty_median,rent_cnty_25pct,rent_cnty_75pct)) %>%
  ungroup() 
  
gc()

# colnames(test4)
test4 <- test4 %>% 
  dplyr::select(-c(valueh_cnty_median_Rented_1, valueh_cnty_25pct_Rented_1,
                   valueh_cnty_75pct_Rented_1,
                   `rent_cnty_median_Owned or being bought (loan)_1`,
                   `rent_cnty_25pct_Owned or being bought (loan)_1`,
                   `rent_cnty_75pct_Owned or being bought (loan)_1`,
                   valueh_cnty_median_Rented_2, valueh_cnty_25pct_Rented_2,
                   valueh_cnty_75pct_Rented_2,
                   `rent_cnty_median_Owned or being bought (loan)_2`,
                   `rent_cnty_25pct_Owned or being bought (loan)_2`,
                   `rent_cnty_75pct_Owned or being bought (loan)_2`)) %>%
  rename(valueh_wht_cnty_median = `valueh_cnty_median_Owned or being bought (loan)_1`,
         valueh_wht_cnty_25pct = `valueh_cnty_25pct_Owned or being bought (loan)_1`,
         valueh_wht_cnty_75pct = `valueh_cnty_75pct_Owned or being bought (loan)_1`,
         rent_wht_cnty_median = `rent_cnty_median_Rented_1`,
         rent_wht_cnty_25pct = `rent_cnty_25pct_Rented_1`,
         rent_wht_cnty_75pct = `rent_cnty_75pct_Rented_1`,
         valueh_blk_cnty_median = `valueh_cnty_median_Owned or being bought (loan)_2`,
         valueh_blk_cnty_25pct = `valueh_cnty_25pct_Owned or being bought (loan)_2`,
         valueh_blk_cnty_75pct = `valueh_cnty_75pct_Owned or being bought (loan)_2`,
         rent_blk_cnty_median = `rent_cnty_median_Rented_2`,
         rent_blk_cnty_25pct = `rent_cnty_25pct_Rented_2`,
         rent_blk_cnty_75pct = `rent_cnty_75pct_Rented_2`)

## Number of white hhs >= 80th percentile home value 
## Number of black hhs <=20th percentile home value

## Number of white hh owners 
test5 <- hh_data %>% 
  filter(ownershp==1 & race==1) %>%
  count(statefip, countyicp) %>%
  rename(ownhh_wht_cnty = n)

## Number of black hh renters
test6 <- hh_data %>% 
  filter(ownershp == 2 & race == 2) %>%
  count(statefip, countyicp) %>%
  rename(renthh_blk_cnty = n)

hh_cnty_data <- test %>%
  left_join(test2) %>%
  left_join(test3) %>%
  left_join(test4) %>%
  left_join(test5) %>%
  left_join(test6) %>%
  mutate(ownhh_wht_cnty = replace_na(ownhh_wht_cnty, 0),
         renthh_blk_cnty = replace_na(renthh_blk_cnty, 0))

remove(test)
remove(test2)
remove(test3)
remove(test4)
remove(test5)
remove(test6)
gc()

# check -- 8/15/2023 TMM. Only see missingness in some of the continuous vars
# sapply(hh_cnty_data, function(x) sum(is.na(x)))

# merge and save ----------------------------------------------------
hh_area_data <- tots %>%
  left_join(hh_ed_data) %>%
  left_join(hh_cnty_data)

# check -- 8/15/2023 TMM. We should only see missingness in continuous variables
# sapply(hh_area_data, function(x) sum(is.na(x)))

saveRDS(hh_area_data, file=paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                                  "comm-level-social-vars/data/",
                                  "clean-data-2/ipums1940_area_hh_vars.RDS"))

# some descriptives -------------------------------------------------

# ## 1. Proportion homes owned, rented per ED
# 
# hist(hh_area_data$ed_prop_owned)
# hist(hh_area_data$ed_prop_rented)
# hist(hh_area_data$ed_prop_notowned_rented)
# 
# ## 2. Home value median, 25th percentile, 75th percentile 
# 
# ### distribution < $20,000 because it is so skewed
# 
# hh_area_data %>% filter(valueh_ed_median <20000) %>% 
#   ggplot(aes(x=valueh_ed_median)) + geom_histogram(bins=50)
# hh_area_data %>% filter(valueh_ed_median <20000) %>% 
#   ggplot(aes(x=valueh_ed_25pct)) + geom_histogram(bins=50)
# hh_area_data %>% filter(valueh_ed_median <20000) %>% 
#   ggplot(aes(x=valueh_ed_75pct)) + geom_histogram(bins=50)
# 
# 
# ## 3. Proportion homes owned, rented per county
# 
# c <- hh_area_data %>% distinct(statefip, countyicp, .keep_all=TRUE) 
# 
# hist(c$cnty_prop_owned)
# hist(c$cnty_prop_rented)
# hist(c$cnty_prop_notowned_rented)
# 
# ## 4. Home value median, 25th percentile, 75th percentile 
# 
# c %>% ggplot(aes(x=valueh_cnty_median)) + geom_histogram(bins=50)
# c %>% ggplot(aes(x=valueh_cnty_25pct)) + geom_histogram(bins=50)
# c %>% ggplot(aes(x=valueh_cnty_75pct)) + geom_histogram(bins=50)

