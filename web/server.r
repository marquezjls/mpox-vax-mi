library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

# Load data
# db get the preloaded data of Michigan counties
db <- read_rds("../data/db.rds")

# county_list get the list of names of counties
county_list <- db %>%
    arrange(county_name) %>%
    pull(county_name)

# Server is used to create the web application logics
server <- function(input, output) {
    # browser is used to debug the shiny app
    observeEvent(input$browser, {
        browser()
    })

    reactive_color_palatte <- reactive({
        req(input$layer)
        ifelse(
            input$layer == "outbreak_prob",
            "Reds",
            "Blues"
            )
    })

    # reactive_color is used to create the color palette for the map
    reactive_color <- reactive({
        colorQuantile(
            palette = reactive_color_palatte(),
            db[[input$layer]],
            n = 5
        )(db[[input$layer]])
    })

    # output$map is the leaflet map
    output$map <- renderLeaflet({
        leaflet(data = db) %>%
            setView(-85.602, 44.315, zoom = 7) %>%
            addTiles() %>%
            addPolygons(
                fillColor = reactive_color(),
                color = "black",
                weight = 0.5,
                fillOpacity = 0.6,
                highlight = highlightOptions(
                    color = "black",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                ),
                popup = ~ paste0(county_name, ": ", first_pct)
            )
    })
}
