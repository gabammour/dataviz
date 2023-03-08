# Librairie : 
library(readxl)
library(DT)

# Fonction des statistiques sur la base : 
statistiques <- function(base, nom_colonne) {
  moyenne <- round(mean(base[[nom_colonne]], na.rm = TRUE), 4)
  mediane <- round(median(base[[nom_colonne]],na.rm = TRUE), 4)
  ecart_type <- round(sd(base[[nom_colonne]],na.rm = TRUE), 4)
  minimum <- round(min(base[[nom_colonne]],na.rm = TRUE), 4)
  maximum <- round(max(base[[nom_colonne]],na.rm = TRUE), 4)
  kurtosis <- round(e1071::kurtosis(base[[nom_colonne]],na.rm = TRUE), 4)
  skewness <- round(e1071::skewness(base[[nom_colonne]],na.rm = TRUE), 4)
  Jarque_bera  <- round(tseries::jarque.bera.test(base[[nom_colonne]])$p.value,4)
  resultat <- data.frame(
    "Statistiques" = c(
      "Moyenne",
      "Médiane",
      "Ecart-type",
      "Minimum",
      "Maximum",
      "Kurtosis",
      "Skewness",
      "Jarque bera p-value, test de normalité"
    ),
    "Valeur" = c(
      moyenne,
      mediane,
      ecart_type,
      minimum,
      maximum,
      kurtosis,
      skewness, 
      Jarque_bera
    )
  )
  return( 
    datatable(resultat,rownames = FALSE, 
              
              class = "cell-border hover",
              extensions = c("Buttons", "Select"),
              selection = 'none',
              options = list(
                dom = "Bfrtip", 
                pageLength = 10,
                select = list(style = 'os', items = 'row'),
                buttons = c(
                  'copy', 'csv', 'pdf',
                  # selection des elements
                  'selectAll', 'selectNone', 'selectRows'
                )
              )
    )
  )
}


"1960 à 2022"
mod_base <- function(adresse_base) {
  stopifnot(file.exists(adresse_base))
  df <- read.csv2(adresse_base ,skip = 4,sep = ",")
  df <- df[-c(2, 4), -c(2, 4)]
  indicateur <- df[1, 2]
  df <- df[, -2]
  df <- as.data.frame(t(df))
  colnames(df) <- df[1, ]
  df <- df[-c(1, 64), ]
  df <- apply(df, 2, as.numeric)
  df<-as.data.frame(df)
  return(list(df, indicateur))
}

ip <- "C:/Users/batra/Desktop/API_SP.POP.TOTL_DS2_fr_csv_v2_4904028.csv"

View(mod_base(ip)[[1]])




