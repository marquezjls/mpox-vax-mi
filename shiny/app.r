library(shiny)
library(leaflet)
library(tidycensus)
library(sf)
library(tidyverse)

# UI defines UI for application
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(
        top = 10, right = 10,
        selectInput(
            "county",
            "County:",
            c("a", "b", "c")
        ),
        selectInput(
            "layer",
            "Layer:",
            c("First Dose", "Second Dose", "Risk Estimate")
        )
    )
)

# counties get the preloaded data of Michigan counties
counties <- read_rds("data/counties.rds")

# Server is used to create the web application logics
server <- function(input, output) {
    output$map <- renderLeaflet({
        leaflet(data = counties) %>%
        setView(-85.602, 44.315, zoom = 5) %>%
        addTiles() %>%
        addPolygons(
            fillColor = ~colorQuantile("YlOrRd", estimate, n = 5),
            fillOpacity = 0.7,
            weight = 0.5,
            highlight = highlightOptions(
                weight = 3,
                color = "black",
                fillOpacity = 0.7,
                bringToFront = TRUE
            ),
            label = ~paste0(NAME, ": ", estimate)
        )
    })
}

# ShinyAPP runs the application
shinyApp(ui = ui, server = server)
