library(tidyverse)

counties_mi_r <- tidycensus::get_acs(
    geography = "county",
    variables = "B01003_001E",
    state = "MI",
    year = 2021,
    geometry = TRUE
)

db <- counties_mi_r %>%
    mutate(county_name = str_split_i(NAME, ",", 1)) %>%
    select(county_name, geometry) %>%
    left_join(county_all, by = c("county_name" = "County"))

view(db)
write_rds(db, "data/db.rds")
