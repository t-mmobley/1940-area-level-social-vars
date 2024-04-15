# Libraries --------------------------------------------------------
library(tidyverse)
library(ipumsr)

# Read -------------------------------------------------------------
data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                             "comm-level-social-vars/data/clean-data-2/",
                             "ipums1940_indiv_dat.RDS"))
# Format data --------------------------------------------------------

# lower cae colnames
colnames(data) <- tolower(colnames(data))

# Person-level pop counts --------------------------------------------

# sort data 
data <- data[order(data$statefip, data$countyicp, data$enumdist, 
                   data$race), ]

gc()
# ED counts ----------------------------------------------------------
ed_tot_counts <- data %>% count(statefip, countyicp, enumdist) %>%
  rename(ed_pop_tot = n)
gc()
ed_tot25_counts <- data %>% 
  filter(age>=25) %>%
  count(statefip, countyicp, enumdist) %>%
  rename(ed_pop25_tot = n)
gc()
ed_tot16_counts <- data %>% 
  filter(age>=16) %>%
  count(statefip, countyicp, enumdist) %>%
  rename(ed_pop16_tot = n)
gc()
ed_tot14_counts <- data %>% 
  filter(age>=14) %>%
  count(statefip, countyicp, enumdist) %>%
  rename(ed_pop14_tot = n)
gc()

ed_tot_counts <- ed_tot_counts %>%
  left_join(ed_tot25_counts) %>%
  left_join(ed_tot16_counts) %>%
  left_join(ed_tot14_counts) 

remove(ed_tot25_counts)
remove(ed_tot16_counts)
remove(ed_tot14_counts)

## counts by race 
### overall
ed_race_long <- data %>% 
  count(statefip, countyicp, enumdist, race) %>%
  rename(ed_pop_race = n) 

ed_race_wide <- ed_race_long %>% 
  pivot_wider(names_from=race, values_from=ed_pop_race) %>%
  rename(ed_wht_pop = '1',
         ed_blk_pop = '2',
         ed_am_in_pop = '3',
         ed_chinese_pop = '4',
         ed_japanese_pop = '5',
         ed_api_pop = '6',
         ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      ed_wht_pop = 0,
      ed_blk_pop = 0,
      ed_am_in_pop = 0,
      ed_chinese_pop = 0, 
      ed_japanese_pop = 0, 
      ed_api_pop = 0)) %>% 
  # count for white vs not white pop & white/black pop
  # group_by(statefip, countyicp, enumdist) %>%
  mutate(
    ed_nwht_pop = ed_blk_pop + ed_am_in_pop + ed_chinese_pop + ed_japanese_pop + 
      ed_api_pop,
    ed_pop_bwrace = ed_wht_pop + ed_blk_pop) 

### pop 25+
ed_race_long <- data %>% 
  filter(age >=25) %>%
  count(statefip, countyicp, enumdist, race) %>%
  rename(ed_pop25_race = n) 

ed_race_wide25 <- ed_race_long %>% 
  pivot_wider(names_from=race, values_from=ed_pop25_race) %>%
  rename(ed_wht_pop25 = '1',
         ed_blk_pop25 = '2',
         ed_am_in_pop25 = '3',
         ed_chinese_pop25 = '4',
         ed_japanese_pop25 = '5',
         ed_api_pop25 = '6',
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      ed_wht_pop25 = 0,
      ed_blk_pop25 = 0,
      ed_am_in_pop25 = 0,
      ed_chinese_pop25 = 0, 
      ed_japanese_pop25 = 0, 
      ed_api_pop25 = 0)) %>% 
  # count for white vs not white pop & white/black pop
  mutate(
    ed_nwht_pop25 = ed_blk_pop25 + ed_am_in_pop25 + ed_chinese_pop25 + 
      ed_japanese_pop25 + ed_api_pop25,
    ed_pop25_bwrace = ed_wht_pop25 + ed_blk_pop25) 

### pop 16+
ed_race_long <- data %>% 
  filter(age >=16) %>%
  count(statefip, countyicp, enumdist, race) %>%
  rename(ed_pop16_race = n) 

ed_race_wide16 <- ed_race_long %>% 
  pivot_wider(names_from=race, values_from=ed_pop16_race) %>%
  rename(ed_wht_pop16 = '1',
         ed_blk_pop16 = '2',
         ed_am_in_pop16 = '3',
         ed_chinese_pop16 = '4',
         ed_japanese_pop16 = '5',
         ed_api_pop16 = '6',
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      ed_wht_pop16 = 0,
      ed_blk_pop16 = 0,
      ed_am_in_pop16 = 0,
      ed_chinese_pop16 = 0, 
      ed_japanese_pop16 = 0, 
      ed_api_pop16 = 0)) %>% 
  # count for white vs not white pop & white/black pop
  mutate(
    ed_nwht_pop16 = ed_blk_pop16 + ed_am_in_pop16 + ed_chinese_pop16 + 
      ed_japanese_pop16 + ed_api_pop16,
    ed_pop16_bwrace = ed_wht_pop16 + ed_blk_pop16) 


### pop 14+
ed_race_long <- data %>% 
  filter(age >=14) %>%
  count(statefip, countyicp, enumdist, race) %>%
  rename(ed_pop14_race = n) 

ed_race_wide14 <- ed_race_long %>% 
  pivot_wider(names_from=race, values_from=ed_pop14_race) %>%
  rename(ed_wht_pop14 = '1',
         ed_blk_pop14 = '2',
         ed_am_in_pop14 = '3',
         ed_chinese_pop14 = '4',
         ed_japanese_pop14 = '5',
         ed_api_pop14 = '6',
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      ed_wht_pop14 = 0,
      ed_blk_pop14 = 0,
      ed_am_in_pop14 = 0,
      ed_chinese_pop14 = 0, 
      ed_japanese_pop14 = 0, 
      ed_api_pop14 = 0)) %>% 
  # count for white vs not white pop & white/black pop
  mutate(
    ed_nwht_pop14 = ed_blk_pop14 + ed_am_in_pop14 + ed_chinese_pop14 + 
      ed_japanese_pop14 + ed_api_pop14,
    ed_pop14_bwrace = ed_wht_pop14 + ed_blk_pop14) 

# checks
# head(ed_raceth_long)
# head(ed_raceth_wide)
# summary(ed_raceth_wide$ed_wht_pop)
# summary(ed_raceth_wide$ed_blk_pop)

ed_counts <- ed_tot_counts %>%
  left_join(ed_race_wide) %>%
  left_join(ed_race_wide25) %>%
  left_join(ed_race_wide16) %>%
  left_join(ed_race_wide14)
  
remove(ed_tot_counts)
remove(ed_race_long)
remove(ed_race_wide)
remove(ed_race_wide25)
remove(ed_race_wide16)
remove(ed_race_wide14)

gc()

# Met area counts --------------------------------------------------
data <- data[order(data$metarea, data$race), ]
gc()

metarea_tot_counts <- data %>% count(metarea) %>%
  rename(met_pop_tot = n)
gc()
metarea_tot25_counts <- data %>% 
  filter(age>=25) %>%
  count(metarea) %>%
  rename(met_pop25_tot = n)
gc()
metarea_tot16_counts <- data %>% 
  filter(age>=16) %>%
  count(metarea) %>%
  rename(met_pop16_tot = n)
gc()
metarea_tot14_counts <- data %>% 
  filter(age>=14) %>%
  count(metarea) %>%
  rename(met_pop14_tot = n)
gc()

metarea_tot_counts <- metarea_tot_counts %>%
  left_join(metarea_tot25_counts) %>%
  left_join(metarea_tot16_counts) %>%
  left_join(metarea_tot14_counts) 

remove(metarea_tot25_counts)
remove(metarea_tot16_counts)
remove(metarea_tot14_counts)

## by race
### overall
metarea_race_long <- data %>% count(metarea, race) %>%
  rename(metarea_pop_race = n)

metarea_race_wide <- metarea_race_long %>% 
  pivot_wider(names_from=race, values_from=metarea_pop_race) %>%
  rename(met_wht_pop = '1',
         met_blk_pop = '2',
         met_am_in_pop = '3',
         met_chinese_pop = '4',
         met_japanese_pop = '5',
         met_api_pop = '6') %>%
  as.data.frame() %>%
  replace_na(
    list(
      met_wht_pop = 0,
      met_blk_pop = 0,
      met_am_in_pop = 0,
      met_chinese_pop = 0,
      met_japanese_pop = 0,
      met_api_pop = 0
    )
  ) %>%
  # count for white vs not white pop & white/black pop
  mutate(
    met_nwht_pop = met_blk_pop + met_am_in_pop + met_api_pop + 
      met_chinese_pop + met_japanese_pop,
    met_pop_bwrace = met_wht_pop + met_blk_pop)

gc()

### pop 25+
metarea_race_long <- data %>% 
  filter(age >=25) %>%
  count(metarea, race) %>%
  rename(metarea_pop25_race = n) 

metarea_race_wide25 <- metarea_race_long %>% 
  pivot_wider(names_from=race, values_from=metarea_pop25_race) %>%
  rename(met_wht_pop25 = '1',
         met_blk_pop25 = '2',
         met_am_in_pop25 = '3',
         met_chinese_pop25 = '4',
         met_japanese_pop25 = '5',
         met_api_pop25 = '6',
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      met_wht_pop25 = 0,
      met_blk_pop25 = 0,
      met_am_in_pop25 = 0,
      met_chinese_pop25 = 0, 
      met_japanese_pop25 = 0, 
      met_api_pop25 = 0)) %>% 
  # count for white vs not white pop & white/black pop
  mutate(
    met_nwht_pop25 = met_blk_pop25 + met_am_in_pop25 + met_chinese_pop25 + 
      met_japanese_pop25 + met_api_pop25,
    met_pop25_bwrace = met_wht_pop25 + met_blk_pop25) 

gc()

### pop 16+
metarea_race_long <- data %>% 
  filter(age >=16) %>%
  count(metarea, race) %>%
  rename(metarea_pop16_race = n) 

metarea_race_wide16 <- metarea_race_long %>% 
  pivot_wider(names_from=race, values_from=metarea_pop16_race) %>%
  rename(met_wht_pop16 = '1',
         met_blk_pop16 = '2',
         met_am_in_pop16 = '3',
         met_chinese_pop16 = '4',
         met_japanese_pop16 = '5',
         met_api_pop16 = '6',
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      met_wht_pop16 = 0,
      met_blk_pop16 = 0,
      met_am_in_pop16 = 0,
      met_chinese_pop16 = 0, 
      met_japanese_pop16 = 0, 
      met_api_pop16 = 0)) %>% 
  # count for white vs not white pop & white/black pop
  mutate(
    met_nwht_pop16 = met_blk_pop16 + met_am_in_pop16 + met_chinese_pop16 + 
      met_japanese_pop16 + met_api_pop16,
    met_pop16_bwrace = met_wht_pop16 + met_blk_pop16) 

gc()

### pop 14+
metarea_race_long <- data %>% 
  filter(age >=14) %>%
  count(metarea, race) %>%
  rename(metarea_pop14_race = n) 

metarea_race_wide14 <- metarea_race_long %>% 
  pivot_wider(names_from=race, values_from=metarea_pop14_race) %>%
  rename(met_wht_pop14 = '1',
         met_blk_pop14 = '2',
         met_am_in_pop14 = '3',
         met_chinese_pop14 = '4',
         met_japanese_pop14 = '5',
         met_api_pop14 = '6',
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      met_wht_pop14 = 0,
      met_blk_pop14 = 0,
      met_am_in_pop14 = 0,
      met_chinese_pop14 = 0, 
      met_japanese_pop14 = 0, 
      met_api_pop14 = 0)) %>% 
  # count for white vs not white pop & white/black pop
  mutate(
    met_nwht_pop14 = met_blk_pop14 + met_am_in_pop14 + met_chinese_pop14 + 
      met_japanese_pop14 + met_api_pop14,
    met_pop14_bwrace = met_wht_pop14 + met_blk_pop14) 

# checks 
# head(metarea_raceth_long)
# head(metarea_raceth_wide)
gc()

metarea_counts <- metarea_tot_counts %>% 
  left_join(metarea_race_wide) %>%
  left_join(metarea_race_wide25) %>%
  left_join(metarea_race_wide16) %>%
  left_join(metarea_race_wide14) 

remove(metarea_tot_counts)
remove(metarea_race_long)
remove(metarea_race_wide)
remove(metarea_race_wide25)
remove(metarea_race_wide14)
remove(metarea_race_wide16)
gc()

# County counts ------------------------------------------------------
data <- data[order(data$statefip, data$countyicp), ]
gc()

county_tot_counts <- data %>% count(statefip, countyicp) %>%
  rename(cnty_pop_tot = n)
gc()
county_tot25_counts <- data %>% 
  filter(age>=25) %>%
  count(statefip, countyicp) %>%
  rename(cnty_pop25_tot = n)
gc()
county_tot16_counts <- data %>% 
  filter(age>=16) %>%
  count(statefip, countyicp) %>%
  rename(cnty_pop16_tot = n)
gc()
county_tot14_counts <- data %>% 
  filter(age>=14) %>%
  count(statefip, countyicp) %>%
  rename(cnty_pop14_tot = n)

gc()

county_tot_counts <- county_tot_counts %>%
  left_join(county_tot25_counts) %>%
  left_join(county_tot16_counts) %>%
  left_join(county_tot14_counts) 

remove(county_tot25_counts)
remove(county_tot16_counts)
remove(county_tot14_counts)

## by race
### overall
county_race_long <- data %>% count(statefip, countyicp, race) %>%
  rename(cnty_pop_race = n)

county_race_wide <- county_race_long %>%
  pivot_wider(names_from = race, values_from = cnty_pop_race) %>%
  rename(
    cnty_wht_pop = '1',
    cnty_blk_pop = '2',
    cnty_am_in_pop = '3',
    cnty_chinese_pop = '4',
    cnty_japanese_pop = '5',
    cnty_api_pop = '6'
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      cnty_wht_pop = 0,
      cnty_blk_pop = 0,
      cnty_am_in_pop = 0,
      cnty_chinese_pop = 0,
      cnty_japanese_pop = 0,
      cnty_api_pop = 0
    )
  ) %>%
  # count for white vs not white pop & white/black pop
  mutate(
    cnty_nwht_pop = cnty_blk_pop + cnty_am_in_pop + cnty_chinese_pop +
      cnty_japanese_pop + cnty_api_pop, 
    cnty_pop_bwrace = cnty_wht_pop +cnty_blk_pop) 

### pop 25+
county_race_long <- data %>%   filter(age >= 25) %>%
  count(statefip, countyicp, race) %>%
  rename(cnty_pop25_race = n)

county_race_wide25 <- county_race_long %>%
  pivot_wider(names_from = race, values_from = cnty_pop25_race) %>%
  rename(
    cnty_wht_pop25 = '1',
    cnty_blk_pop25 = '2',
    cnty_am_in_pop25 = '3',
    cnty_chinese_pop25 = '4',
    cnty_japanese_pop25 = '5',
    cnty_api_pop25 = '6'
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      cnty_wht_pop25 = 0,
      cnty_blk_pop25 = 0,
      cnty_am_in_pop25 = 0,
      cnty_chinese_pop25 = 0,
      cnty_japanese_pop25 = 0,
      cnty_api_pop25 = 0
    )
  ) %>%
  # count for white vs not white pop & white/black pop
  mutate(
    cnty_nwht_pop25 = cnty_blk_pop25 + cnty_am_in_pop25 + cnty_chinese_pop25 +
      cnty_japanese_pop25 + cnty_api_pop25, 
    cnty_pop_bwrace25 = cnty_wht_pop25 +cnty_blk_pop25) 

### pop 16+
county_race_long <- data %>%   filter(age >= 16) %>%
  count(statefip, countyicp, race) %>%
  rename(cnty_pop16_race = n)

county_race_wide16 <- county_race_long %>%
  pivot_wider(names_from = race, values_from = cnty_pop16_race) %>%
  rename(
    cnty_wht_pop16 = '1',
    cnty_blk_pop16 = '2',
    cnty_am_in_pop16 = '3',
    cnty_chinese_pop16 = '4',
    cnty_japanese_pop16 = '5',
    cnty_api_pop16 = '6'
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      cnty_wht_pop16 = 0,
      cnty_blk_pop16 = 0,
      cnty_am_in_pop16 = 0,
      cnty_chinese_pop16 = 0,
      cnty_japanese_pop16 = 0,
      cnty_api_pop16 = 0
    )
  ) %>%
  # count for white vs not white pop & white/black pop
  mutate(
    cnty_nwht_pop16 = cnty_blk_pop16 + cnty_am_in_pop16 + cnty_chinese_pop16 +
      cnty_japanese_pop16 + cnty_api_pop16, 
    cnty_pop_bwrace16 = cnty_wht_pop16 +cnty_blk_pop16) 

### pop 14+
county_race_long <- data %>%   filter(age >= 14) %>%
  count(statefip, countyicp, race) %>%
  rename(cnty_pop14_race = n)

county_race_wide14 <- county_race_long %>%
  pivot_wider(names_from = race, values_from = cnty_pop14_race) %>%
  rename(
    cnty_wht_pop14 = '1',
    cnty_blk_pop14 = '2',
    cnty_am_in_pop14 = '3',
    cnty_chinese_pop14 = '4',
    cnty_japanese_pop14 = '5',
    cnty_api_pop14 = '6'
  ) %>%
  as.data.frame() %>%
  replace_na(
    list(
      cnty_wht_pop14 = 0,
      cnty_blk_pop14 = 0,
      cnty_am_in_pop14 = 0,
      cnty_chinese_pop14 = 0,
      cnty_japanese_pop14 = 0,
      cnty_api_pop14 = 0
    )
  ) %>%
  # count for white vs not white pop & white/black pop
  mutate(
    cnty_nwht_pop14 = cnty_blk_pop14 + cnty_am_in_pop14 + cnty_chinese_pop14 +
      cnty_japanese_pop14 + cnty_api_pop14, 
    cnty_pop_bwrace14 = cnty_wht_pop14 +cnty_blk_pop14) 

# checks 
# head(county_raceth_long)
# head(county_raceth_wide)

county_counts <- county_tot_counts %>% 
  left_join(county_race_wide) %>%
  left_join(county_race_wide25) %>%
  left_join(county_race_wide16) %>%
  left_join(county_race_wide14) 

remove(county_tot_counts)
remove(county_race_long)
remove(county_race_wide)
remove(county_race_wide25)
remove(county_race_wide16)
remove(county_race_wide14)
gc()

# Merge and distinct -----------------------------------------------
data <- data %>%
  dplyr::select(c(statefip, countyicp, metarea, enumdist)) %>% 
  distinct() %>%
  left_join(ed_counts) %>%
  left_join(metarea_counts) %>%
  left_join(county_counts)

remove(ed_counts)
remove(metarea_counts)
remove(county_counts)
gc()

# Household counts ---------------------------------------------------
hh_data <- readRDS(paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                          "comm-level-social-vars/data/clean-data-2/",
                          "ipums1940_hh_dat.RDS"))
gc()

# Enumeration districts
# table(hh_data$gq, hh_data$gq_factor, exclude=NULL) # gq 1 & 2 = households
ed_hhs <- hh_data %>% filter(gq %in% c(1,2)) %>%
  count(statefip, countyicp, enumdist) %>%
  rename(ed_hh_tot = n)

## Note: this is for an ICE measure denominator
ed_ownhhs <- hh_data %>% filter(ownershp==1) %>%
  count(statefip, countyicp, enumdist) %>%
  rename(ed_hhown_tot = n)

ed_gqs <- hh_data %>% filter(gq %in% c(3,4,6)) %>% 
  count(statefip, countyicp, enumdist) %>%
  rename(ed_gq_tot = n)

# Met areas
metarea_hhs <- hh_data %>% filter(gq %in% c(1,2)) %>%
  count(metarea) %>%
  rename(met_hh_tot = n)

metarea_ownhhs <- hh_data %>% filter(ownershp==1) %>%
  count(metarea) %>%
  rename(met_hhown_tot = n)

metarea_gqs <- hh_data %>% filter(gq %in% c(3,4,6)) %>%
  count(metarea) %>%
  rename(met_gq_tot = n)

# Counties
county_hhs <- hh_data %>% filter(gq %in% c(1,2)) %>%
  count(statefip, countyicp) %>%
  rename(cnty_hh_tot = n)

county_ownhhs <- hh_data %>% filter(ownershp==1) %>%
  count(statefip, countyicp) %>%
  rename(cnty_hhown_tot = n)

county_gqs <- hh_data %>% filter(gq %in% c(3,4,6)) %>%
  count(statefip, countyicp) %>%
  rename(cnty_gq_tot = n)

hh_tots <- hh_data %>%
  dplyr::select(c(statefip, countyicp, metarea, 
                  # metaread, 
                  enumdist)) %>%
  distinct() %>%
  left_join(ed_hhs) %>%
  left_join(ed_ownhhs) %>%
  left_join(ed_gqs) %>%
  left_join(metarea_hhs) %>%
  left_join(metarea_ownhhs) %>%
  left_join(metarea_gqs) %>%
  left_join(county_hhs) %>%
  left_join(county_ownhhs) %>%
  left_join(county_gqs)

remove(ed_hhs)
remove(ed_ownhhs)
remove(metarea_hhs)
remove(metarea_ownhhs)
remove(county_hhs)
remove(county_ownhhs)
remove(ed_gqs)
remove(metarea_gqs)
remove(county_gqs)
remove(hh_data)
gc()

# Merge hh and indiv counts ------------------------------------------
data <- data %>% 
  left_join(hh_tots) 

# check -- TMM 8/15/2023. Variables with missingness include
# ed_pop25_tot, ed_hh_tot, ed_hhown_tot, ed_gq_tot, and cnty_gq_tot
# missingness should be changed to 0 (these variables were calc'd using filter)
# sapply(data, function(x) sum(is.na(x)))

data <- data %>%
  replace(is.na(.), 0)

# check -- no duplicate state, county, ed obs in final data!
# check <- data %>%
#   count(statefip, countyicp, enumdist)

# check -- TMM 8/15/2023
# sapply(data, function(x) sum(is.na(x)))

# Save
saveRDS(data, paste0("C:/Users/tmobley/Desktop/geo1940-crosswalks/",
                             "comm-level-social-vars/data/",
                             "clean-data-2/ipums1940_pop_hh_counts.RDS"))
