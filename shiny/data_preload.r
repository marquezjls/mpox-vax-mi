counties <- tidycensus::get_acs(
    geography = "county",
    variables = "B01003_001E",
    state = "MI",
    geometry = TRUE
)