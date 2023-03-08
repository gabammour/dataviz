library(shiny)
library(DT)
library(readxl)

# Interface utilisateur
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  titlePanel("Statistiques des données"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choisir un fichier"),
      selectInput("colonne", "Sélectionner un pays", choices = NULL), 
      verbatimTextOutput("Nom_base")
    ),
    mainPanel(
      DT::dataTableOutput("stats")
    )
  )
)

# Serveur
server <- function(input, output) {
  
  # Chargement des données
  
  data <- reactive({
    req(input$file)
    #read.csv2(input$file$datapath, stringsAsFactors = FALSE, sep = ";")
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

# Lancement de l'application
shinyApp(ui = ui, server = server)




