library(shiny)
library(DT)

ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  "Indicateurs Macroéconomiques",
  tabPanel(
    "Bases enregistrées",
    titlePanel("Indicateurs macroéconomiques par pays."),
    fluidRow(
      column(
        width = 12,
        div(
          class = "d-flex",
          tagList(
            column(width = 3, selectInput("pays", "Choisissez un pays :", choices = NULL)),
            column(
              width = 3,
              selectInput(
                "indicateur",
                "Choisissez un indicateur :",
                c("Population", "CO2 par habitants", "PIB par habitants en PPA")
              )
            ),
            column(
              width = 3,
              selectInput(
                "graph_type",
                "Choisissez un type de graphique :",
                c("Evolution temporelle")
              )
            ),
            column(width = 3, actionButton("valider", "Valider"))
          )
        )
      )
    ), 
    
    fluidRow(column(width = 8, plotOutput("graph"), offset = 2)),
    fluidRow(column(width = 8, DTOutput("stats1"), offset = 2))
  ),
  tabPanel(
    "Bases banque mondiales",
    titlePanel("Indicateurs Macroéconomiques par pays"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", 
                  label = HTML(paste("Choisir un fichier au format csv issu des indicateurs banques mondiale contenant les données par pays. Vous pouvez cliquer ",
                                     tags$a("ici", href = "https://donnees.banquemondiale.org/indicateur", target = "_blank"))),
                  accept = ".csv"),
        selectInput("colonne", "Sélectionner un pays", choices = NULL),
        verbatimTextOutput("Nom_base")
      ),
      mainPanel(DT::dataTableOutput("stats"))
    )
  )
)

server <- function(input, output) {
  
  #Onglet 1 : 
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
    output$stats1<- renderDT({
      statistiques(base_select(input$indicateur), input$pays)
    })
    if (input$graph_type == "Evolution temporelle"){
      output$graph <- renderPlot({
        selected_col <- pop[,input$pays]
        selected_data <- data.frame(Date = pop$Date, Population = selected_col)
        plot_pop(data = selected_data, xvar = "Date", yvar = "Population")
      })
    }
  })
  
  #Onglet 2: 
  
  # Chargement des données
  data <- reactive({
    req(input$file)
    mod_base(input$file$datapath)[[1]]
  })
  
  # Sélection des colonnes
  observe({
    req(data())
    updateSelectInput(
      inputId = "colonne",
      choices = colnames(data())
    )
  })
  
  #Affichage du nom de la base 
  output$Nom_base <- renderText({
    if (is.null(input$file)) {
      "Nom de la variable"
    } else {
      mod_base(input$file$datapath)[[2]]
    }
  })
  # Affichage des statistiques
  output$stats <- renderDT({
    req(data(), input$colonne)
    statistiques(data(), input$colonne)
  })
  
}

shinyApp(ui = ui, server = server)



