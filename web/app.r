library(shiny)
library(leaflet)
library(tidycensus)
library(sf)
library(tidyverse)
library(viridis)

# Preload data
# counties get the preloaded data of Michigan counties
counties <- read_rds("data/counties.rds")

# county_list get the list of names of counties
counties <- counties %>%
    mutate(county_name = str_split_i(NAME, ",", 1))

county_list <- counties %>%
    arrange(county_name) %>%
    pull(county_name)

# UI defines UI for application
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(
        top = 10, right = 10,
        class = "panel",
        selectInput(
            "county",
            "County:",
            county_list
        ),
        selectInput(
            "layer",
            "Layer:",
            c("First Dose", "Second Dose", "Risk Estimate")
        )
    )
)

# Server is used to create the web application logics
server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet(data = counties) %>%
            setView(-85.602, 44.315, zoom = 5) %>%
            addTiles() %>%
            addPolygons(
                fillColor = ~ colorQuantile(palette = viridis(5), estimate, n = 5)(estimate),
                color = "black",
                weight = 0.5,
                fillOpacity = 0.5,
                highlight = highlightOptions(
                    color = "black",
                    fillOpacity = 0.8,
                    bringToFront = TRUE
                ),
                label = ~ paste0(county_name, ": ", estimate)
            )
    })
}

# ShinyAPP runs the application
shinyApp(ui = ui, server = server)

# leaflet debug
leaflet(data = counties) %>%
    setView(-85.602, 44.315, zoom = 7) %>%
    addTiles() %>%
    addPolygons(
        fillColor = ~ colorQuantile(
            palette = viridis(5),
            estimate,
            n = 5
        )(estimate),
        color = "black",
        weight = 0.5,
        fillOpacity = 0.5,
        highlight = highlightOptions(
            color = "black",
            fillOpacity = 0.8,
            bringToFront = TRUE
        ),
        label = ~ paste0(county_name, ": ", estimate),
        popup = ~ paste0(county_name, ": ", estimate)
    )
