library(tidyverse)
library(lubridate)
library(sf)
library(tmap)
library(tidycensus) ## package to navigate census
library(cartogram)

## Downloading feature geometry from the Census website.  To cache shapefiles for use
## in future sessions, set `options(tigris_use_cache = TRUE)`.
options(tigris_use_cache = TRUE)

## import files

## people living with HIV
df_hiv_counties <- read_csv(file = "AtlasPlusTableData (6).csv", skip = 9) ## this just happens to be named this way.
## people on PREP
df_prep_counties <- read_csv(file = "AtlasPlusTableData (5).csv", skip = 7) ## this just happens to be named thsi way
## vaccines from MDHHS website
df_vaccines <- read_csv(file = "vaccine_rates_mdhhs_24apr23.csv")


df_hiv_counties <- df_hiv_counties %>%
  select(Indicator, County, FIPS, cnt = Cases) %>%
  mutate(
    cnt = ifelse(cnt == "Data suppressed", NA, cnt),
    cnt = as.double(cnt)
  )

df_prep_counties <- df_prep_counties %>%
  select(Indicator, County, FIPS, cnt = Population)


df_vaccines <- df_vaccines %>%
  rename(County = COUNTY) %>%
  mutate(County = paste0(County, " County")) %>%
  mutate(County = ifelse(County == "Detroit City County", "Wayne County", County)) %>%
  group_by(`Preparedness Region`, County) %>%
  summarise(first_dose = sum(`First Dose`), second_dose = sum(`Second Dose`)) %>%
  ungroup()


df_all <- bind_rows(df_hiv_counties, df_prep_counties)



df_all <- df_all %>%
  arrange(County) %>%
  group_by(County, FIPS) %>%
  summarise(total_cnt = sum(cnt, na.rm = T)) %>%
  mutate(adj_total_cnt = 1.25 * total_cnt) %>%
  ungroup() %>%
  left_join(df_vaccines) %>%
  mutate(
    pct_first_dose = first_dose / adj_total_cnt,
    pct_second_dose = second_dose / adj_total_cnt
  )


### By pct
df_all %>%
  filter(total_cnt > 0) %>%
  ggplot(aes(x = County, y = pct_first_dose, fill = County)) +
geom_point(aes(y = pct_first_dose, size = adj_total_cnt), alpha = 0.5, shape = 21) +
  geom_segment(aes(x = County, xend = County, y = 0, yend = pct_first_dose)) +
  scale_size(range = c(1, 15)) +
  hrbrthemes::theme_ipsum() +
  viridis::scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "A") +
  ylab("Pct High Risk Vaccinated") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### By size of county
df_all %>%
  filter(total_cnt > 0) %>%
  ggplot(aes(x = County, y = pct_first_dose, fill = County)) +
  geom_point(aes(y = pct_first_dose, size = adj_total_cnt), alpha = 0.5, shape = 21) +
  geom_segment(aes(x = fct_reorder(County, -adj_total_cnt), xend = fct_reorder(County, -adj_total_cnt), y = 0, yend = pct_first_dose)) +
  scale_size(range = c(1, 15)) +
  hrbrthemes::theme_ipsum() +
  viridis::scale_fill_viridis(discrete = TRUE, guide = FALSE, option = "A") +
  ylab("Pct High Risk Vaccinated") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



pop_tract <- get_acs(
  geography = "county", variables = "B01003_001E", state = "MI",
  geometry = T
)

tmap_mode("plot")
tmap_mode("view")
qtm(pop_tract)

county_pct_sf <- pop_tract %>%
  left_join(df_all %>% mutate(FIPS = as.character(FIPS)), by = c("GEOID" = "FIPS"))

county_pct_sf_transformed <- st_transform(county_pct_sf, crs = "+init=epsg:3395")
county_carto <- cartogram_ncont(county_pct_sf_transformed, "adj_total_cnt")

tm_shape(county_pct_sf) +
  tm_polygons(col = "pct_first_dose", style = "jenks") +
  tm_layout(frame = FALSE, legend.position = c("left", "bottom"))

tm_shape(county_pct_sf_transformed) +
  tm_borders() +
  tm_shape(county_carto) +
  tm_polygons("pct_first_dose", id = "NAME", popup.vars = c("NAME", "pct_first_dose"))
