# 1940-area-level-social-vars
This repository includes all of the necessary code to create 1940 area-level social factors using full count 1940 Census microdata from IPUMS. The data are publicly available with agreement to special usage terms and can be found here: (https://usa.ipums.org/usa-action/samples#usa_100_percent)

Collectively, the code produces a clean area-level 1940 census data set with social indicators (e.g., area-level education) and common measures of segregation, including the dissimilarity index and three ICE measures. With a few exceptions, the indicators are constructed at the county, metropolitan, and enumeration district level. Finally, area-level social factors are constructed (1) overall and (2) race-stratified for Black and White US populations.

Data citation: 
Steven Ruggles, Matt A. Nelson, Matthew Sobek, Catherine A. Fitch, Ronald Goeken, J. David Hacker, Evan Roberts, and J. Robert Warren. IPUMS Ancestry Full Count Data: Version 4.0 [usa_00023.xml]. Minneapolis, MN: IPUMS, 2024. https://doi.org/10.18128/D014.V4.0

File descriptions:
- 01_data_cleaning.R -- This script imports the full count 1940 IPUMS census data set, cleans and formats all variables (where needed), and creates a race variable. Then, I create data subsets: a household-level data set, individual-level data for the population 16+ years old, 14+ years old, 25+ years old, and all ages.
  
- 02_pop_hh_counts.R -- This R script constructs population totals at the enumeration district, metro area, and county-level (1) overall and (2) by race. It also creates household counts at the enumeration district, metro area, and county-level
  
- 03_area_level_hh_vars.R -- This script derives household-level social measures summarized at the enumeration district (first portion) and county-level (second portion). The measures include: proportion owned/rented/neither, home value, and rent (1) overall and (2) by race. I also derive some race-specific counts for ICE measures later on.
  
- 04_ed_level_indiv_vars.R -- This script is similar to script 3 (above), except it derives individual-level variables summarized at the enumeration district-level only. The measures include: indicators of education (among population 25+) and employment and wages (among 14+ and 16+ populations), each overall and by race.
  
- 05_cnty_level_indiv_vars.R --  This script derives the same indicators described above but at the county-level rather than the ed-level.
  
- 06_metarea_dissimilarity_index.R --  This script derives two dissimilarity index measures (one assessing Black/White pop differences, and the other assessing non-white/white pop differences) at the metro area-level with enumeration districts as geographic subunits.
  
- 07_county_dissimilarity_index v2.R -- This script derives two dissimilarity index measures (one assessing Black/white pop differences, and the other assessing non-white/white pop differences) at the metro area-level with enumeration districts as geographic subunits.
  
- 08_ICE_measures.R -- This script derives four ICE measures at the (1) ed-level and (2) county-level. They include: race only, race and education, and race and home ownership. Every measure should have a range between -1 and 1.
