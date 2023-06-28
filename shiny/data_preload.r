library(tidyverse)

counties_mi <- tidycensus::get_acs(
    geography = "county",
    variables = "B01003_001E",
    state = "MI",
    year = 2021,
    geometry = TRUE
)

write_rds(counties_mi, "data/counties.rds")
