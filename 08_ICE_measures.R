# title: "08_ICE_measures"
# author: Taylor Mobley

library(tidyverse)
library(sf)
library(numform)
library(viridis)
library(stringr)
options(scipen = 10)
options(digits = 10)

# Read -----------------------------------------------------------------
data <- readRDS(paste0('C:/Users/tmobley/Desktop/geo1940-crosswalks/',
                           'comm-level-social-vars/data/clean-data-2/',
                           'ipums1940_area_level_dat.RDS'))

# ICE measures ---------------------------------------------------------

# 08/14/2023 -- TMM removing race & home value ICE measure 
## [calculated = (valueh80ptile_wht_ed-valueh20ptile_blk_ed)/(ed_hhown_tot)] &
## [ = (valueh80ptile_wht_cnty-valueh20ptile_blk_cnty)/(cnty_hhown_tot)]

# ED-level 

ed_ICE <- data %>% group_by(statefip, countyicp, enumdist) %>% 
  mutate(raceICE_ed = (ed_wht_pop - ed_blk_pop)/(ed_pop_tot),
         raceeduICE_ed = ((educ_gt8_ed_wht_n-educ_le8_ed_blk_n)/(ed_pop25_tot)),
         raceemp14ICE_ed = ((emp14_ed_wht_n-unemp14_ed_blk_n)/(ed_pop14_tot)),
         raceemp16ICE_ed = ((emp16_ed_wht_n-unemp16_ed_blk_n)/(ed_pop16_tot)),
         raceownhhICE_ed = (ownhh_wht_ed-renthh_blk_ed)/(ed_hh_tot)) %>%
  ungroup()

# County-level

county_ICE <- ed_ICE %>% group_by(statefip, countyicp) %>% 
  mutate(raceICE_cnty = (cnty_wht_pop - cnty_blk_pop)/(cnty_pop_tot),
       raceeduICE_cnty = 
         ((educ_gt8_cnty_wht_n-educ_le8_cnty_blk_n)/(cnty_pop25_tot)),
       raceemp14ICE_cnty = ((emp14_cnty_wht_n-unemp14_cnty_blk_n)/(cnty_pop14_tot)),
       raceemp16ICE_cnty = ((emp16_cnty_wht_n-unemp16_cnty_blk_n)/(cnty_pop16_tot)),
       raceownhhICE_cnty = (ownhh_wht_cnty-renthh_blk_cnty)/(cnty_hh_tot)) %>%
  ungroup()

# save data ------------------------------------------------------------

saveRDS(county_ICE, paste0('C:/Users/tmobley/Desktop/geo1940-crosswalks/',
                       'comm-level-social-vars/data/clean-data-2/',
                       'ipums1940_area_level_race_dat_clean.RDS'))

saveRDS(county_ICE, paste0('C:/Users/tmobley/Box/',
                           'Historical_determinants_aging_R01_NIA/data/',
                           'Social-exposures/ipums1940_measures/',
                           'comm-level-social-vars/clean-data/',
                           'ipums1940_area_level_race_dat_clean.RDS'))
