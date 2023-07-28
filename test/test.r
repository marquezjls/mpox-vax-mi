# runApp runs the shiny app
library(shiny)
library(leaflet)

db_test <- read_rds("data/db.rds")
county_list <- db %>%
    arrange(County) %>%
    pull(County)
runApp("./web")

# leaflet debugging
leaflet(data = db) %>%
    setView(-85.602, 44.315, zoom = 7) %>%
    addTiles() %>%
    addPolygons(
        fillColor = ~ colorQuantile(
            palette = "Blues",
            first_pct,
            n = 5
        )(first_pct),
        color = "black",
        weight = 0.5,
        fillOpacity = 0.5,
        highlight = highlightOptions(
            color = "black",
            fillOpacity = 0.8,
            bringToFront = TRUE
        ),
        popup = ~ paste0(county_name, ": ", first_pct)
    )