library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

# Load data
# db get the preloaded data of Michigan counties
db <- read_rds("../data/db.rds")

# county_list generates the list of names of counties
county_list <- db %>%
    arrange(County) %>%
    pull(County)

# ui defines UI for application
ui <- bootstrapPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
    ),
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
            inputId = "layer",
            label = "Map Layer:",
            choices = c(
                "First Dose" = "first_pct",
                "Second Dose" = "second_pct",
                "Risk Estimate" = "outbreak_prob"
            ),
            selected = "first_pct"
        ),
        uiOutput("info")
    )
)

# server is used to create the web application logics
server <- function(input, output) {
    # reactive_info is the reactive data for the info panel
    reactive_info <- reactive({
        req(input$county)
        db %>%
            filter(County == input$county) # nolint
    })

    # output$info is the info panel
    observeEvent(input$county, {
        output$info <- renderUI({
            div(
                class = "info",
                div(
                    class = "info-item",
                    span(class = "info-item-title", "FIRST DOSE VACCINE",),
                    span(
                        class = "info-item-value",
                        reactive_info()$first_pct * 100, "%"
                    )
                ),
                div(
                    class = "info-item",
                    span(class = "info-item-title", "SECOND DOSE VACCINE"),
                    span(
                        class = "info-item-value",
                        reactive_info()$second_pct * 100, "%"
                    )
                ),
                div(
                    class = "info-item",
                    span(class = "info-item-title", "OUTBREAK PROBABILITY"),
                    span(
                        class = "info-item-value-red",
                        reactive_info()$outbreak_prob * 100, "%"
                    )
                )
            )
        })
    })

    # input$map_shape_click updates the county selection from clicks on the map
    observeEvent(input$map_shape_click, {
        updateSelectInput(
            inputId = "county",
            selected = input$map_shape_click$id
        )
        input$map_shape_click$id
    })

    # reactive_color_palatte is *Blues* for vaccine coverages
    # and is *Reds* for outbreak probability
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
                layerId = ~County,
                highlight = highlightOptions(
                    color = "black",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                )
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
