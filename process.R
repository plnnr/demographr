library(tidyverse)
library(sf)
library(tigris)

pdx_msa_definition <- c('41005', '41009', '41051', '41067', '41071', '53011', '53059')

msa.sf <- rbind(tigris::tracts("OR", cb = T), tigris::tracts("WA", cb = T)) %>%
  filter(substr(GEOID, 1, 5) %in% pdx_msa_definition) %>% 
  st_transform(3857) %>%
  select(GEOID)

# saveRDS(msa.sf, "data/msa.sf_20210218.rds")

