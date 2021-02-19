library(tidyverse)
library(tidycensus)
library(sf)

##### Data and resources #####
# msa.sf <- readRDS("data/msa.sf_20210218.rds") %>% st_transform("+proj=longlat +datum=WGS84")
# demographic_data_long <- readRDS("demographic_data_long_20201210.rds") %>%
#   mutate(GEOID_selected = paste0(GEOID, "_selectedLayer"))

msa.sf <- readRDS("data/pima_msa.sf_20210218.rds") %>% st_transform("+proj=longlat +datum=WGS84")
demographic_data_long <- readRDS("data/pima_tract_demographic_data_2010t2019_20210218.rds") %>%
  mutate(GEOID_selected = paste0(GEOID, "_selectedLayer"))


##### Functions #####
findLocations.sf <- function(drawn_shape, reference_shape, reference_id_colname) {
  ## slimmed simple features port of geoshaper https://github.com/RedOakStrategic/geoshaper
  
  reference_id_colname <- sym(reference_id_colname)
  polygon_coordinates <- drawn_shape$geometry$coordinates
  feature_type <- drawn_shape$properties$feature_type
  
  drawn_polygon <- st_polygon(list(
    do.call(rbind, 
            lapply(polygon_coordinates[[1]], 
                   function(x) {c(x[[1]][1], x[[2]][1])}))
  ))
  
  reference_shape %>%
    st_filter(., drawn_polygon) %>%
    pull(!!reference_id_colname)
}

cv_interp <- function(cv) {
  ret <- case_when(cv == 0 | cv > 0.4 ~ "3. Not reliable",
                   cv >= 0.2 & cv < 0.4 ~ "2. Use caution",
                   cv < 0.2 ~ "1. Reliable",
                   is.na(cv) ~ "3. Not reliable")
}

summarize_tracts <- function(.df, tract_ids) {
  if(.df$parameter[1] == "Total") {
  .df %>%
    filter(GEOID %in% tract_ids) %>%
    group_by(sumlevel, year, topic, parameter, subgroup, universe, variable) %>%
    summarize(est = sum(est),
              moe = moe_sum(moe, est, na.rm = T),
              cv = moe / 1.645 / est,
              rel = cv_interp(cv)) 
  }
  else if(.df$parameter[1] == "Percent") {
    # TODO Re-write so that percentages are recalculated
    break()
  }
}

