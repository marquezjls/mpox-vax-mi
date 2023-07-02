library(tigris)
library(tidyverse)

# counties_mi_r gets the shape data of Michigan counties
counties_mi_r <- counties(
    state = "MI",
    year = 2021,
    cb = TRUE
) %>%
    select(
        NAMELSAD,
        geometry
    ) %>%
    rename(
        County = NAMELSAD,
    )

write_rds(counties_mi_r, "data/counties_mi.rds")
