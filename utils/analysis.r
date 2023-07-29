library(tidyverse)

county_prep_r <- read_csv("data/mi_prep.csv", skip = 7)
county_hiv_r <- read_csv("data/mi_hiv.csv", skip = 9)
county_vaccine_r <- read_csv("data/vaccine_rates_mdhhs_24apr23.csv")

# county_prep is the data of PrEP in Michigan counties
## population is the at risk population vs cases
county_prep <- county_prep_r %>%
    select(County, cases_prep = Population)

# county_hiv is the data of HIV in Michigan counties
## hiv cases is the number of HIV cases
county_hiv <- county_hiv_r %>%
    select(County, Cases) %>%
    mutate(
        cases_hiv = parse_number(na_if(Cases, "Data suppressed"))
    )

# county_vaccine is the data of vaccine in Michigan counties
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

# county_merged is the data of all three data sets
county_merged <- county_prep %>%
    left_join(county_hiv, by = "County") %>%
    rowwise() %>%
    mutate(
        total_cnt = sum(cases_hiv, cases_prep, na.rm = TRUE),
        adj_total_cnt = 1.25 * total_cnt
    ) %>%
    ungroup() %>%
    left_join(county_vaccine, by = "County") %>%
    mutate(
        first_pct = round(first_dose / adj_total_cnt, 3),
        second_pct = round(second_dose / adj_total_cnt, 3)
    ) %>%
    mutate(
        first_pct = if_else(is.infinite(first_pct), NA, first_pct),
        second_pct = if_else(is.infinite(second_pct), NA, second_pct)
    ) %>%
        select(County, adj_total_cnt, first_pct, second_pct)

# prediction_model is the model used to predict the outbreak probability
prediction_model <- read_csv("data/prediction_model.csv")

# county_model is the predicted outbreak probability of Michigan counties
county_model <- county_merged %>%
    cross_join(prediction_model) %>%
    group_by(County) %>%
    mutate(
        coverage_diff = first_pct - Coverage
    ) %>%
    filter(coverage_diff >= 0) %>%
    slice_min(coverage_diff) %>%
    select(County, outbreak_prob)

# county_all is the data_merged and county_model merged
county_all <- county_merged %>%
    left_join(county_model, by = "County") %>%
    mutate(
        outbreak_prob = round(outbreak_prob, 3)
    )

# counties_geo is the geographic data of Michigan counties from **geo.r**
county_geo <- read_rds("data/counties_mi.rds")

# county_final is the final data for the shiny app
county_final <- county_geo %>%
    left_join(as.data.frame(county_all), by = c("County" = "County"))

write_rds(county_final, "data/db.rds")
