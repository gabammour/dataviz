library(shiny)
library(DT)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  navbarPage("Navigation", tabPanel("Première page")),
  titlePanel("Indicateurs macroéconomiques par pays."),
  fluidRow(
    column(width = 4, selectInput("pays", "Choisissez un pays :", choices = NULL)),
    column(
      width = 4,
      selectInput(
        "indicateur",
        "Choisissez un indicateur :",
        c("Population", "CO2 par habitants", "PIB par habitants en PPA")
      )
    ),
    column(width = 4, actionButton("valider", "Valider"))
  ),
  fluidRow(column(width = 6, DTOutput("stats1")))
)

server <- function(input, output) {
  
  choix_pays <- reactive({
    colnames(base_select(input$indicateur))[-1]
  })
  
  observe({
    updateSelectInput(
      inputId = "pays",
      label = "Choisissez un pays :",
      choices = choix_pays()
    )
  })
  
  observeEvent(input$valider, {
    output$stats1 <- renderDT({
      statistiques2(input$indicateur, input$pays)
    })
  })
}

shinyApp(ui, server)


