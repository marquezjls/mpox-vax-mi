library(tidyverse)

county_prep_r <- read_csv("data/mi_prep.csv", skip = 7)
county_hiv_r <- read_csv("data/mi_hiv.csv", skip = 9)
county_vaccine_r <- read_csv("data/vaccine_rates_mdhhs_24apr23.csv")

county_prep <- county_prep_r %>%
    select(County, Cases) %>%
    mutate(
        cases_prep = parse_number(na_if(Cases, "Data suppressed"))
    )

county_hiv <- county_hiv_r %>%
    select(County, Cases) %>%
    mutate(
        cases_hiv = parse_number(na_if(Cases, "Data suppressed"))
    )

county_vaccine <- county_vaccine_r %>%
    rename(County = COUNTY) %>%
    mutate(County = paste0(County, " County")) %>%
    mutate(County = if_else(
        County == "Detroit City County",
        "Wayne County",
        County
    )) %>%
    group_by(County) %>%
    summarise(first_dose = sum(`First Dose`), second_dose = sum(`Second Dose`))

county_all <- county_prep %>%
    left_join(county_hiv, by = "County") %>%
    rowwise() %>%
    mutate(
        total_cnt = sum(cases_hiv, cases_prep, na.rm = TRUE),
        adj_total_cnt = 1.25 * total_cnt
    ) %>%
    ungroup() %>%
    left_join(county_vaccine, by = "County") %>%
    select(County, adj_total_cnt, first_dose, second_dose) %>%
    view()
county_all
