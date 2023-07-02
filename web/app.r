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
    pull(County) %>%
    append("---", after = 0)

# ui defines UI for application
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    actionButton("browser", "Trigger browser()"),
    uiOutput("log"),
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
            label = "Layer:",
            choices = c(
                "First Dose" = "first_pct",
                "Second Dose" = "second_pct",
                "Risk Estimate" = "outbreak_prob"
            ),
            selected = "first_pct"
        )
    )
)

# server is used to create the web application logics
server <- function(input, output) {
    # browser is used to debug the shiny app
    observeEvent(input$browser, {
        browser()
    })

    observeEvent(input$county, {
        output$log <- renderUI({
            paste0("You have selected ", input$county)
        })
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

# reactive_popup is used to create the popup for the map


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
                layerId = ~ County,
                highlight = highlightOptions(
                    color = "black",
                    fillOpacity = 0.9,
                    bringToFront = TRUE
                ),
                popup = ~ paste0(
                    County, ": ",
                    db[[input$layer]] * 100, "%"
                )
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)