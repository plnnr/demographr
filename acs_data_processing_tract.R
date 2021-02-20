##### Load libraries, set working directory #####
if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, tidycensus, tigris, rio, sf, scales, mapview)

options(
  scipen = 999,
  digits = 4,
  tigris_class = "sf",
  tigris_use_cache = T
)

`%notin%` <- function(lhs, rhs) !(lhs %in% rhs)

range01 <- function(x, ...){(x - min(x, ...)) / (max(x, ...) - min(x, ...))}

cv_interp <- function(cv) {
  ret <- case_when(cv == 0 | cv > 0.4 ~ "3. Not reliable",
                   cv >= 0.2 & cv < 0.4 ~ "2. Use caution",
                   cv < 0.2 ~ "1. Reliable",
                   is.na(cv) ~ "3. Not reliable")
}

aggregate_long <- function(df, vars2collapse, ...) {
  group_var <- enquos(...)
  
  df %>% 
    filter(variable %in% vars2collapse) %>%
    group_by(!!! group_var) %>% 
    summarize(est = sum(estimate),
              moe = moe_sum(moe, estimate))
}

aggregate_long_varnum <- function(df, varnums2collapse, ...) {
  group_var <- enquos(...)
  
  df %>% 
    filter(varnum %in% varnums2collapse) %>%
    group_by(!!! group_var) %>% 
    summarize(est = sum(estimate),
              moe = moe_sum(moe, estimate))
}

make_summary_var <- function(df) {
  df %>% filter(variable == "Total") %>% rename(summary_est = est, summary_moe = moe) %>% select(GEOID, year, subgroup, summary_est, summary_moe)
}

add_pct <- function(aggd_df) {
  ret <- aggd_df %>%
    left_join(., make_summary_var(aggd_df), by = c('GEOID', 'year', 'subgroup')) %>%
    group_by(GEOID, year, variable) %>%
    mutate(pct_est = est / summary_est,
           pct_moe = moe_prop(est, summary_est, moe, summary_moe),
           parameter = "Percent") %>%
    select(GEOID, year, parameter, subgroup, variable, est = pct_est, moe = pct_moe, universe, topic) %>% ungroup() %>%
    rbind(., aggd_df) %>% ungroup() 
}


moe_chgprop <- function(moe_y1, moe_y2, est_y1, est_y2) {
  ## Calculate moe for change in proportions
  moe <- (sqrt(moe_y2 ^ 2 + (est_y2 / est_y1) ^ 2 * moe_y1 ^ 2 )) / est_y1
  return(moe)
}

sig_test <- function(est_y1, est_y2, moe_y1, moe_y2) {
  ## Determine if there is a difference between two numbers; TRUE values are significant at 90% confidence
  se1 <- moe_y1 / 1.645
  se2 <- moe_y2 / 1.645
  test <- abs((est_y1 - est_y2) / sqrt(se1 ^ 2 + se2 ^ 2))
  return(test > 1.645)
}

overlap_se <- function(y1, y2, moe_y1, moe_y2) {
  se1 <- moe_y1 / 1.645
  se2 <- moe_y2 / 1.645
  c <- (y2 - y1) / 5
  sex1x2 <- sqrt(1 - c) * sqrt(se1 ^ 2 + se2 ^ 2)
  return(sex1x2)
}

var2race <- function(raw_variable) {
  ret <- case_when(substr(raw_variable, 7, 7) == "_" ~ "All",
                   substr(raw_variable, 7, 7) == "A" ~ "White, not Hispanic",
                   substr(raw_variable, 7, 7) == "B" ~ "Black, not Hispanic",
                   substr(raw_variable, 7, 7) == "C" ~ "Native American, not Hispanic",
                   substr(raw_variable, 7, 7) == "D" ~ "Asian, not Hispanic",
                   substr(raw_variable, 7, 7) == "E" ~ "Hawaiian or Pacific Islander, not Hispanic",
                   substr(raw_variable, 7, 7) == "F" ~ "Another race, not Hispanic",
                   substr(raw_variable, 7, 7) == "G" ~ "Two or more races, not Hispanic",
                   substr(raw_variable, 7, 7) == "H" ~ "White alone, not Hispanic",
                   substr(raw_variable, 7, 7) == "I" ~ "Hispanic or Latino",
                   TRUE ~ NA_character_)
  return(ret)
}

##### Resources #####
acs18 <- load_variables(2018, "acs5", cache = TRUE)
cpi <- rio::import("resources/cpi-u-rs_1950-current.xlsx") %>% select(year, inflation_factor_2019)
pdx_msa_definition <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')

msa.sf <- tigris::tracts("AZ", cb = T) %>% filter(COUNTYFP == "019") %>% select(GEOID)

state_to_download <- "AZ"
county_to_download <- "Pima"


##### Tract-level estimates #####
### Education tables
education <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B15002",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

edu_aggd<- rbind(
  education %>% aggregate_long_varnum(., varnums2collapse = c(3:14, 20:31), GEOID, year) %>% mutate(variable = "No four-year degree", subgroup = "All", universe = "Population 25 Years and Over", parameter = "Total", topic = "Educational Attainment"),
  education %>% aggregate_long_varnum(., varnums2collapse = c(15:18, 32:35), GEOID, year) %>% mutate(variable = "Bachelor's or higher", subgroup = "All", universe = "Population 25 Years and Over", parameter = "Total", topic = "Educational Attainment"),
  education %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Population 25 Years and Over", parameter = "Total", topic = "Educational Attainment")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))

##### Education by race #####
education_by_race <- map_df(c(2010:2019), function(acs_year) {
  
  tabs <- map_chr(c("A", "B", "C", "D", "E", "F", "G", "H", "I"), ~ paste0("C15002",.x))
  
  ret <- map_df(tabs, function(tab){
    get_acs(geography = "tract", state = state_to_download, county = county_to_download, table = tab, year = acs_year) %>%
      mutate(year = acs_year)
  }) 
}) %>% 
  mutate(subgroup = var2race(variable),
         varnum = as.numeric(str_sub(variable, -3)))

education_by_race_aggd1 <- rbind(
  education_by_race %>% aggregate_long_varnum(., varnums2collapse = c(3,4,5,8,9,10), GEOID, year, subgroup) %>% mutate(variable = "No four-year degree", universe = "Population 25 Years and Over", parameter = "Total", topic = "Educational Attainment"),
  education_by_race %>% aggregate_long_varnum(., varnums2collapse = c(6, 11), GEOID, year, subgroup) %>% mutate(variable = "Bachelor's or higher", universe = "Population 25 Years and Over", parameter = "Total", topic = "Educational Attainment"),
  education_by_race %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year, subgroup) %>% mutate(variable = "Total", universe = "Population 25 Years and Over", parameter = "Total", topic = "Educational Attainment")
) %>% ungroup() %>%
  add_pct(.) %>%
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))

tmp0 <- rbind(education_by_race_aggd1, edu_aggd)

tmp0 %>%
  filter(parameter == "Total", subgroup == "All") %>%
  select(GEOID, year, variable, sumest = est, summoe = moe) -> tmp1

tmp0 %>%
  filter(parameter == "Total", subgroup == "White alone, not Hispanic") %>%
  select(GEOID, year, variable, whest =  est, whmoe = moe) -> tmp2

edu_final <- left_join(tmp1, tmp2, by = c("GEOID", "year", "variable")) %>%
  mutate(est = sumest - whest,
         moe = sqrt(whmoe^2 + summoe^2), ## MOE for subtracted variables same as adding https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20170419_MOE_Transcript.pdf
         subgroup = "People of Color", universe = "Population 25 Years and Over", parameter = "Total", topic = "Educational Attainment", parameter = "Total") %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  add_pct(.) %>%
  filter(!(parameter == "Percent" & variable == "Total")) %>%
  rbind(., tmp0) %>%
  arrange(GEOID, year, variable)


##### Age (youth and elderly) tables ####
age <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B01001",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

age_aggd <- rbind(
  age %>% aggregate_long_varnum(., varnums2collapse = c(3:6, 27:30), GEOID, year) %>% mutate(variable = "Under 18", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Age"),
  age %>% aggregate_long_varnum(., varnums2collapse = c(3, 4, 27, 28), GEOID, year) %>% mutate(variable = "Under 10", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Age"),
  age %>% aggregate_long_varnum(., varnums2collapse = c(7:19, 31:43), GEOID, year) %>% mutate(variable = "Age 18 to 64", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Age"),
  age %>% aggregate_long_varnum(., varnums2collapse = c(20:25, 44:49), GEOID, year) %>% mutate(variable = "Over 64", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Age"),
  age %>% aggregate_long_varnum(., varnums2collapse = c(23:25, 47:49), GEOID, year) %>% mutate(variable = "Over 74", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Age"),
  age %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Age")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


age_by_race <- map_df(c(2010:2019), function(acs_year) {
  
  tabs <- map_chr(c("A", "B", "C", "D", "E", "F", "G", "H", "I"), ~ paste0("B01001",.x))
  
  ret <- map_df(tabs, function(tab){
    get_acs(geography = "tract", state = state_to_download, county = county_to_download, table = tab, year = acs_year) %>%
      mutate(year = acs_year)
  }) 
}) %>% 
  mutate(subgroup = var2race(variable),
         varnum = as.numeric(str_sub(variable, -3)))

age_by_race_aggd1 <- rbind(
  age_by_race %>% aggregate_long_varnum(., varnums2collapse = c(3:6, 18:21), GEOID, year, subgroup) %>% mutate(variable = "Under 18", universe = "Total Population", parameter = "Total", topic = "Age"),
  age_by_race %>% aggregate_long_varnum(., varnums2collapse = c(3, 4, 18, 19), GEOID, year, subgroup) %>% mutate(variable = "Under 10", universe = "Total Population", parameter = "Total", topic = "Age"),
  age_by_race %>% aggregate_long_varnum(., varnums2collapse = c(7:13, 22:28), GEOID, year, subgroup) %>% mutate(variable = "Age 18 to 64", universe = "Total Population", parameter = "Total", topic = "Age"),
  age_by_race %>% aggregate_long_varnum(., varnums2collapse = c(14:16, 29:31), GEOID, year, subgroup) %>% mutate(variable = "Over 64", universe = "Total Population", parameter = "Total", topic = "Age"),
  age_by_race %>% aggregate_long_varnum(., varnums2collapse = c(15, 16, 30, 31), GEOID, year, subgroup) %>% mutate(variable = "Over 74", universe = "Total Population", parameter = "Total", topic = "Age"),
  age_by_race %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year, subgroup) %>% mutate(variable = "Total", universe = "Total Population", parameter = "Total", topic = "Age")
) %>% ungroup() %>%
  add_pct(.) %>%
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))

tmp0 <- rbind(age_by_race_aggd1, age_aggd)

tmp0 %>%
  filter(parameter == "Total", subgroup == "All") %>%
  select(GEOID, year, variable, sumest = est, summoe = moe) -> tmp1

tmp0 %>%
  filter(parameter == "Total", subgroup == "White alone, not Hispanic") %>%
  select(GEOID, year, variable, whest =  est, whmoe = moe) -> tmp2

age_final <- left_join(tmp1, tmp2, by = c("GEOID", "year", "variable")) %>%
  mutate(est = sumest - whest,
         moe = sqrt(whmoe^2 + summoe^2), ## MOE for subtracted variables same as adding https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20170419_MOE_Transcript.pdf
         subgroup = "People of Color", universe = "Total Population", parameter = "Total", topic = "Age", parameter = "Total") %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  add_pct(.) %>%
  filter(!(parameter == "Percent" & variable == "Total")) %>%
  rbind(., tmp0) %>%
  arrange(GEOID, year, variable)


##### Tenure tables ####
tenure <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B25003",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

ten_aggd <- rbind(
  tenure %>% aggregate_long_varnum(., varnums2collapse = c(3), GEOID, year) %>% mutate(variable = "Renters", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure"),
  tenure %>% aggregate_long_varnum(., varnums2collapse = c(2), GEOID, year) %>% mutate(variable = "Owners", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure"),
  tenure %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


##### Median age housing stock tables ####
stock_age <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B25037",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

stock_aggd <- rbind(
  stock_age %>% aggregate_long_varnum(., varnums2collapse = c(3), GEOID, year) %>% mutate(variable = "Median Age of Housing Stock", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Age of Housing Stock"),
  stock_age %>% aggregate_long_varnum(., varnums2collapse = c(2), GEOID, year) %>% mutate(variable = "Median Age of Housing Stock", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Age of Housing Stock"),
  stock_age %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Median Age of Housing Stock", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Age of Housing Stock")
) %>% ungroup() %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))

## Do not use this below, since it messes with the legend rendering
# stock_aggdpct <- stock_aggd %>%
#   mutate(parameter = "Percent") %>%
#   rbind(., stock_aggd)


##### Race tables ####
race <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B03002",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

race_aggd <- rbind(
  race %>% aggregate_long_varnum(., varnums2collapse = c(3), GEOID, year) %>% mutate(variable = "White alone, not Hispanic", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(4), GEOID, year) %>% mutate(variable = "Black alone, not Hispanic", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(5), GEOID, year) %>% mutate(variable = "Native American alone, not Hispanic", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(6), GEOID, year) %>% mutate(variable = "Asian alone, not Hispanic", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(7), GEOID, year) %>% mutate(variable = "Hawaiian or Pacific Isalnder alone, not Hispanic", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(8), GEOID, year) %>% mutate(variable = "Another race alone, not Hispanic", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(9), GEOID, year) %>% mutate(variable = "Two or more races, not Hispanic", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(12), GEOID, year) %>% mutate(variable = "Hispanic or Latino", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(4, 5, 7, 12), GEOID, year) %>% mutate(variable = "Black, Indigenous, Latinx", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(4, 5, 6, 7, 8, 9, 12), GEOID, year) %>% mutate(variable = "People of Color", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race"),
  race %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Total Population", parameter = "Total", topic = "Race")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


###### MHI tables ####
mhi_by_race <- map_df(c(2010:2019), function(acs_year) {
  
  tabs <- map_chr(c("", "A", "B", "C", "D", "E", "F", "G", "H", "I"), ~ paste0("B19013",.x))
  
  ret <- map_df(tabs, function(tab){
    get_acs(geography = "tract", state = state_to_download, county = county_to_download, table = tab, year = acs_year) %>%
      mutate(year = acs_year)
  }) 
}) %>% 
  mutate(subgroup = var2race(variable),
         varnum = as.numeric(str_sub(variable, -3)))

mhi <- mhi_by_race %>% 
  left_join(., cpi, by = "year") %>%
  mutate(est = estimate * inflation_factor_2019,
         moe = moe * inflation_factor_2019,
         variable = "Median Household Income", universe = "Households",  parameter = "Total",  topic = "Income") %>%
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable, subgroup) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))

## Do not use this below, since it messes with the legend rendering
# mhi_aggdpct <- mhi %>%
#   mutate(parameter = "Percent") %>%
#   rbind(., mhi)


##### Poverty tables ####
poverty <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "C17002",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

pov_aggd <- rbind(
  poverty %>% aggregate_long_varnum(., varnums2collapse = c(2,3), GEOID, year) %>% mutate(variable = "Income Under 1x Poverty", subgroup = "All", universe = "Population for Whom Poverty Status Is Determined", parameter = "Total", topic = "Poverty"),
  poverty %>% aggregate_long_varnum(., varnums2collapse = c(4:8), GEOID, year) %>% mutate(variable = "Income Over 1x Poverty", subgroup = "All", universe = "Population for Whom Poverty Status Is Determined", parameter = "Total", topic = "Poverty"),
  poverty %>% aggregate_long_varnum(., varnums2collapse = c(2:7), GEOID, year) %>% mutate(variable = "Income Under 2x Poverty", subgroup = "All", universe = "Population for Whom Poverty Status Is Determined", parameter = "Total", topic = "Poverty"),
  poverty %>% aggregate_long_varnum(., varnums2collapse = c(8), GEOID, year) %>% mutate(variable = "Income Over 2x Poverty", subgroup = "All", universe = "Population for Whom Poverty Status Is Determined", parameter = "Total", topic = "Poverty"),
  poverty %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Population for Whom Poverty Status Is Determined", parameter = "Total", topic = "Poverty")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))




##### Disability by race and age for years 2012 - 2018 (NOT AVAILABLE PRIOR TO 2012) ####
disability_by_race_by_age <- map_df(c(2012:2019), function(acs_year) {
  
  tabs <- map_chr(c("", "A", "B", "C", "D", "E", "F", "G", "H", "I"), ~ paste0("B18101",.x))
  
  ret <- map_df(tabs, function(tab){
    get_acs(geography = "tract", state = state_to_download, county = county_to_download, table = tab, year = acs_year) %>%
      mutate(year = acs_year)
  }) 
}) %>% 
  mutate(subgroup = var2race(variable),
         varnum = as.numeric(str_sub(variable, -3)))

disability_by_age <- disability_by_race_by_age %>% filter(subgroup == "All")
disability_by_race_by_age <- disability_by_race_by_age %>% filter(subgroup != "All")

disability_status <- rbind(
  disability_by_race_by_age %>% aggregate_long_varnum(., varnums2collapse = c(3,6,9), GEOID, year, subgroup) %>% mutate(variable = "With a disability", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Disability Status"),
  disability_by_race_by_age %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year, subgroup) %>% mutate(variable = "Total", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Disability Status"),
  disability_by_age %>% aggregate_long_varnum(., varnums2collapse = c(4,7,10,13,16,19,23,26,29,32,35,38), GEOID, year) %>% mutate(variable = "With a disability", subgroup = "All", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Disability Status"),
  disability_by_age %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Disability Status"),
  disability_by_age %>% aggregate_long_varnum(., varnums2collapse = c(16, 19, 35, 38), GEOID, year) %>% mutate(variable = "With a disability", subgroup = "Elders 65+", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Disability Status"),
  disability_by_age %>% aggregate_long_varnum(., varnums2collapse = c(15, 18, 34, 37), GEOID, year) %>% mutate(variable = "Total", subgroup = "Elders 65+", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Disability Status")
) %>% ungroup() %>%
  add_pct(.) %>%
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>%
  filter(!(parameter == "Percent" & variable == "Total"))


###### LEP tables #####
lepvintage <- map_df(c(2010:2015), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B16002",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

lep <- map_df(c(2016:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "C16002",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

leps <- rbind(lepvintage, lep)

lep_aggd <- rbind(
  leps %>% aggregate_long_varnum(., varnums2collapse = c(4, 7, 10, 13), GEOID, year) %>% mutate(variable = "LEP Households", subgroup = "All", universe = "Households", parameter = "Total", topic = "Limited English Proficiency"),
  leps %>% aggregate_long_varnum(., varnums2collapse = c(5, 8, 11, 14), GEOID, year) %>% mutate(variable = "Non-LEP Households", subgroup = "All", universe = "Households", parameter = "Total", topic = "Limited English Proficiency"),
  leps %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Households", parameter = "Total", topic = "Limited English Proficiency")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


##### Housing cost burden tables #####
hcb <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B25106",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

hcb_aggd <- rbind(
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(6, 10, 14, 18, 22, 28, 32, 36, 40, 44), GEOID, year) %>% mutate(variable = "CB >30%", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost"),
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(4,8,12,16,20,26,30,34,38,42,5,9,13,17,21,27,31,35,39,43), GEOID, year) %>% mutate(variable = "CB <30%", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost"),
  
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(28, 32, 36, 40, 44), GEOID, year) %>% mutate(variable = "CB >30%", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost"),
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(6, 10, 14, 18, 22), GEOID, year) %>% mutate(variable = "CB >30%", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost"),
  
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(27, 31, 35, 39, 43, 26,30,34,38,42), GEOID, year) %>% mutate(variable = "CB <30%", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost"),
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(5, 9, 13, 17, 21, 4,8,12,16,20), GEOID, year) %>% mutate(variable = "CB <30%", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost"),
  
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(24), GEOID, year) %>% mutate(variable = "Total", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost"),
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(2), GEOID, year) %>% mutate(variable = "Total", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost"),
  hcb %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Housing Cost")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


##### Single Mothers #####
## Single mothers and household types by race by year
single_moms <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B11003",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

moms_aggd <- rbind(
  single_moms %>% aggregate_long_varnum(., varnums2collapse = c(16), GEOID, year) %>% mutate(variable = "Single mother with own kids < 18", subgroup = "All", universe = "Families", parameter = "Total", topic = "Family Type"),
  single_moms %>% aggregate_long_varnum(., varnums2collapse = c(17, 18), GEOID, year) %>% mutate(variable = "Single mother with own kids < 6", subgroup = "All", universe = "Families", parameter = "Total", topic = "Family Type"),
  single_moms %>% aggregate_long_varnum(., varnums2collapse = c(10), GEOID, year) %>% mutate(variable = "Single father with own kids < 18", subgroup = "All", universe = "Families", parameter = "Total", topic = "Family Type"),
  single_moms %>% aggregate_long_varnum(., varnums2collapse = c(11, 12), GEOID, year) %>% mutate(variable = "Single father with own kids < 6", subgroup = "All", universe = "Families", parameter = "Total", topic = "Family Type"),
  single_moms %>% aggregate_long_varnum(., varnums2collapse = c(16, 10), GEOID, year) %>% mutate(variable = "Single parents with own kids < 18", subgroup = "All", universe = "Families", parameter = "Total", topic = "Family Type"),
  single_moms %>% aggregate_long_varnum(., varnums2collapse = c(11, 12, 17, 18), GEOID, year) %>% mutate(variable = "Single parents with own kids < 6", subgroup = "All", universe = "Families", parameter = "Total", topic = "Family Type"),
  single_moms %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Families", parameter = "Total", topic = "Family Type")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


##### Tenure by Units in Structure #####
ten_by_unitsstr <- map_df(c(2010:2019), function(acs_year) {
  q <- get_acs(geography = "tract", state = state_to_download, county = county_to_download, 
               table = "B25032", year = acs_year, survey = "acs5") %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

labelten <- rio::import("resources/tenunitsstr_varname.xlsx")

ten_by_unitsstr2 <- ten_by_unitsstr %>% 
  left_join(., acs18, by = c("variable" = "name")) %>%
  left_join(., labelten, by = c("label" = "long"))

tenunts <- rbind(
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(3,4), GEOID, year) %>% mutate(variable = "Single-family", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(5,6), GEOID, year) %>% mutate(variable = "2-4 units", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(7,8), GEOID, year) %>% mutate(variable = "5-19 units", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(9), GEOID, year) %>% mutate(variable = "20-49 units", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(10), GEOID, year) %>% mutate(variable = "50+ units", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(11, 12), GEOID, year) %>% mutate(variable = "Mobile home, boat, RV, van", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(2), GEOID, year) %>% mutate(variable = "Total", subgroup = "Owners", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(14,15), GEOID, year) %>% mutate(variable = "Single-family", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(16,17), GEOID, year) %>% mutate(variable = "2-4 units", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(18,19), GEOID, year) %>% mutate(variable = "5-19 units", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(20), GEOID, year) %>% mutate(variable = "20-49 units", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(21), GEOID, year) %>% mutate(variable = "50+ units", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(22, 23), GEOID, year) %>% mutate(variable = "Mobile home, boat, RV, van", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(13), GEOID, year) %>% mutate(variable = "Total", subgroup = "Renters", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(3,4,14,15), GEOID, year) %>% mutate(variable = "Single-family", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(5,6,16,17), GEOID, year) %>% mutate(variable = "2-4 units", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(7,8,18,19), GEOID, year) %>% mutate(variable = "5-19 units", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(9,20), GEOID, year) %>% mutate(variable = "20-49 units", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(10,21), GEOID, year) %>% mutate(variable = "50+ units", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(11,12,22, 23), GEOID, year) %>% mutate(variable = "Mobile home, boat, RV, van", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure"),
  
  ten_by_unitsstr2 %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Occupied Housing Units", parameter = "Total", topic = "Tenure by Units in Structure")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


##### Health Insurance Coverage #####
insurance <- map_df(c(2013:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B27010",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

insurance_aggd <- rbind(
  insurance %>% aggregate_long_varnum(., varnums2collapse = c(17,33,50,66), GEOID, year) %>% mutate(variable = "No health insurance coverage", subgroup = "All", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Health Insurance"),
  insurance %>% aggregate_long_varnum(., varnums2collapse = c(3,10,19,26,35,42,52,58), GEOID, year) %>% mutate(variable = "With any health insurance coverage", subgroup = "All", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Health Insurance"),
  insurance %>% aggregate_long_varnum(., varnums2collapse = c(4,20,36,53), GEOID, year) %>% mutate(variable = "With employer-based health insurance coverage only", subgroup = "All", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Health Insurance"),
  insurance %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Civilian Noninstitutionalized Population", parameter = "Total", topic = "Health Insurance")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


##### Food Stamps Recipients #####
foodstamps <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B22001",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

foodstamps_aggd <- rbind(
  foodstamps %>% aggregate_long_varnum(., varnums2collapse = c(2), GEOID, year) %>% mutate(variable = "Food stamps received in last 12 months", subgroup = "All", universe = "Households", parameter = "Total", topic = "Food Stamps/SNAP"),
  foodstamps %>% aggregate_long_varnum(., varnums2collapse = c(5), GEOID, year) %>% mutate(variable = "No food stamps received in last 12 months", subgroup = "All", universe = "Households", parameter = "Total", topic = "Food Stamps/SNAP"),
  foodstamps %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Households", parameter = "Total", topic = "Food Stamps/SNAP")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


##### Internet Access #####
internet <- map_df(c(2017:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B28002",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))

comp_w_broadband <- map_df(c(2017:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B28003",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3)))


#### IMPORTANT: Table B28003 can be compared with B28002; HOWEVER, they both cannot be compared with B28009 (by race) for totals;
## Even though B28009 (by race) contains the same variables as B28003, the control totals do not equal one another

comp_w_broadband_by_race <- map_df(c(2017:2019), function(acs_year) {
  tabs <- map_chr(c("A", "B", "C", "D", "E", "F", "G", "H", "I"), ~ paste0("B28009",.x))
  ret <- map_df(tabs, function(tab){
    get_acs(geography = "tract", state = state_to_download, county = county_to_download, table = tab, year = acs_year) %>%
      mutate(year = acs_year)
  }) 
}) %>% 
  mutate(subgroup = var2race(variable),
         varnum = as.numeric(str_sub(variable, -3)))


internet_aggd <- rbind(
  internet %>% aggregate_long_varnum(., varnums2collapse = c(2, 12), GEOID, year) %>% mutate(variable = "With internet access", subgroup = "All", universe = "Households", parameter = "Total", topic = "Internet Access"),
  internet %>% aggregate_long_varnum(., varnums2collapse = c(13), GEOID, year) %>% mutate(variable = "No internet access", subgroup = "All", universe = "Households", parameter = "Total", topic = "Internet Access"),
  internet %>% aggregate_long_varnum(., varnums2collapse = c(6), GEOID, year) %>% mutate(variable = "Cellular data plan only", subgroup = "All", universe = "Households", parameter = "Total", topic = "Internet Access"),
  comp_w_broadband %>% aggregate_long_varnum(., varnums2collapse = c(4), GEOID, year) %>% mutate(variable = "Has computer with broadband", subgroup = "All", universe = "Households", parameter = "Total", topic = "Internet Access"),
  comp_w_broadband %>% aggregate_long_varnum(., varnums2collapse = c(6,5), GEOID, year) %>% mutate(variable = "No computer and/or no internet", subgroup = "All", universe = "Households", parameter = "Total", topic = "Internet Access"),
  internet %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year) %>% mutate(variable = "Total", subgroup = "All", universe = "Households", parameter = "Total", topic = "Internet Access")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))

internet_race_aggd <- rbind(
  comp_w_broadband_by_race %>% aggregate_long_varnum(., varnums2collapse = c(4), GEOID, year, subgroup) %>% mutate(variable = "Has computer with broadband", universe = "Households", parameter = "Total", topic = "Internet Access"),
  comp_w_broadband_by_race %>% aggregate_long_varnum(., varnums2collapse = c(6,5), GEOID, year, subgroup) %>% mutate(variable = "No computer and/or no internet", universe = "Households", parameter = "Total", topic = "Internet Access"),
  comp_w_broadband_by_race %>% aggregate_long_varnum(., varnums2collapse = c(1), GEOID, year, subgroup) %>% mutate(variable = "Total", universe = "Households", parameter = "Total", topic = "Internet Access")
) %>% ungroup() %>% 
  add_pct(.) %>% 
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))

internet_final <- rbind(internet_aggd, internet_race_aggd) %>% arrange(GEOID, year, variable)


### Internet for aggregate households of color? -- NOTE: Do not uncomment code below, because the control totals cannot be compared to each other. Leaving here for future note when this question is asked
# # 
# # tmp0 <- rbind(internet_race_aggd, internet_aggd)
# # 
# # tmp0 %>%
# #   filter(parameter == "Total", subgroup == "All") %>%
# #   select(GEOID, year, variable, sumest = est, summoe = moe) -> tmp1
# # 
# # tmp0 %>%
# #   filter(parameter == "Total", subgroup == "White alone, not Hispanic") %>%
# #   select(GEOID, year, variable, whest =  est, whmoe = moe) -> tmp2
# # 
# # internet_final <- left_join(tmp1, tmp2, by = c("GEOID", "year", "variable")) %>%
# #   mutate(est = sumest - whest,
# #          moe = sqrt(whmoe^2 + summoe^2), ## MOE for subtracted variables same as adding https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20170419_MOE_Transcript.pdf
# #          subgroup = "Households of Color", universe = "Households", parameter = "Total", topic = "Internet Access") %>% 
# #   select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
# #   filter(!(subgroup == "Households of Color" & variable %in% c("Cellular data plan only", "No internet access", "With internet access"))) %>%
# #   add_pct(.) %>%
# #   filter(!(parameter == "Percent" & variable == "Total")) %>%
# #   rbind(., tmp0) %>%
# #   arrange(GEOID, year, variable)
# # 



##### Median gross rent table ####
med_gross_rent <- map_df(c(2010:2019), function(acs_year) {
  query <- get_acs(geography = "tract", 
                   table = "B25064",
                   state = state_to_download, 
                   county = county_to_download,
                   survey = "acs5", 
                   year = acs_year, 
                   cache_table = TRUE) %>%
    mutate(year = acs_year)
}) %>% mutate(varnum = as.numeric(str_sub(variable, -3))) 

mgr <- med_gross_rent %>% 
  left_join(., cpi, by = "year") %>%
  mutate(subgroup = "All",
         est = estimate * inflation_factor_2019,
         moe = moe * inflation_factor_2019,
         variable = "Median Gross Rent", universe = "Renter-Occupied Households",  parameter = "Total",  topic = "Housing Cost") %>%
  select(GEOID, year, parameter, subgroup, variable, est, moe, universe, topic) %>%
  arrange(GEOID, year, variable, subgroup) %>% 
  filter(!(parameter == "Percent" & variable == "Total"))


##### Final export #####
tract_demo <- rbind(edu_final, age_final, ten_aggd, stock_aggd, race_aggd, 
                    mhi, pov_aggd, disability_status, lep_aggd, hcb_aggd, mgr,
                    moms_aggd, tenunts, insurance_aggd, foodstamps_aggd, internet_final) %>%
  mutate(cv = moe / 1.645,
         rel = factor(cv_interp(cv))) %>%
  select(GEOID:moe, cv, rel, universe, topic) %>%
  mutate(topic = case_when(topic == "Educational Attainment by Race" ~ "Educational Attainment",
                           topic == "Disability Status by Race" ~ "Disability Status",
                           topic == "Disability Status by Age" ~ "Disability Status",
                           TRUE ~ topic),
         sumlevel = "140") %>%
  select(sumlevel, GEOID, year, topic, parameter, subgroup, universe, variable, est, moe, cv, rel) %>%
  arrange(GEOID, topic, variable, parameter, subgroup, year)

saveRDS(tract_demo, "data/pima_tract_demographic_data_2010t2019_20210218.rds")

saveRDS(msa.sf, "data/pima_msa.sf_20210218.rds")



## Test mapping
tract_demo %>%
  filter(variable == "Single mother with own kids < 18" ,
         parameter == "Percent",
         year == 2019) %>% 
  left_join(., msa.sf, by = "GEOID") %>% st_as_sf() %>%
  mapview(., zcol = "est", at = seq(0, .45, 0.05))



tract_demo %>%
  filter(variable == "Median Household Income" ,
         parameter == "Total",
         subgroup == "All",
         year == 2010) %>%
  left_join(., msa.sf, by = "GEOID") %>% st_as_sf() %>%
  mapview(., zcol = "est", at = seq(0, 140000, 20000))
levels(as.factor(tract_demo$variable))

