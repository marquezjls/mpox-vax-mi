library(shiny)
library(leaflet)
library(tidycensus)
library(sf)

# Define UI for application
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

# Get census data
counties <- tidycensus::get_acs(
    geography = "county",
    variables = "B01003_001E",
    state = "MI",
    geometry = TRUE
)



# Server is used to create the web application logics
server <- function(input, output) {

    output$map <- renderLeaflet({
        leaflet() %>%
        setView(-85.602, 44.315, zoom = 5) %>%
        addTiles() %>%
        addPolygons(
            data = counties,
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

# Run the application
shinyApp(ui = ui, server = server)


# Diana in the health department surveillance
# combine some data sources

# fire up
# Integration the 