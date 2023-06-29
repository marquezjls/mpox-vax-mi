library(tidyverse)

counties_mi_r <- tidycensus::get_acs(
    geography = "county",
    variables = "B01003_001E",
    state = "MI",
    year = 2021,
    geometry = TRUE
)

write_rds(counties_mi_r, "data/counties_mi.rds")
