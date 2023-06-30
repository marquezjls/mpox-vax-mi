# ui defines UI for application
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    actionButton("browser", "Trigger browser()"),
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
                "Risk Estimate" = "risk_estimate"
            ),
            selected = "first_pct"
        )
    )
)