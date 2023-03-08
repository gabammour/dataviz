library(shiny)
library(DT)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  navbarPage("Navigation", tabPanel("Première page")),
  titlePanel("Indicateurs macroéconomiques par pays."),
  fluidRow(
    column(width = 4, textInput("pays", "Choisissez un pays :", "")),
    column(
      width = 4,
      selectInput(
        "indicateur",
        "Choisissez un indicateur :",
        c("Population", "CO2 par habitants", "PIB par habitants en PPA")
      )
    ),
    column(
      width = 4,
      selectInput(
        "graph_type",
        "Choisissez un type de graphique :",
        c("Evolution temporelle")
      )
    ),
    column(width = 4, actionButton("valider", "Valider"))
  ),
  fluidRow(column(width = 6, plotOutput("graph"))
  ),
  fluidRow(column(width = 6, DTOutput("stats")))
)
server <- function(input, output) {
  observeEvent(input$valider, {
    output$stats <- renderDT({
      statistiques(input$indicateur, input$pays)
    })
    if (input$graph_type == "Evolution temporelle"){
      output$graph <- renderPlot({
        selected_col <- pop[,input$pays]
        selected_data <- data.frame(Date = pop$Date, Population = selected_col)
        plot_pop(data = selected_data, xvar = "Date", yvar = "Population")
      })
    }
  })
}

shinyApp(ui, server)


