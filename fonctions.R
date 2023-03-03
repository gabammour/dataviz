# Librairie : 
library(DT)
library(moments)
library(stringdist)
pop <- read.csv2("Base/pop.csv", sep = ";")


pays_proche <- function(base, nom_colonne){ 
  list_name <- colnames(base)[-1] # On retire la première colonne (années)
  find <-
    stringdist(list_name, nom_colonne, method = "lv")
  return(unique(list_name[which(find == min(find))])[1])
}

# Statistiques : 
statistiques <- function(base, nom_colonne) {
  nom_colonne <- pays_proche(base,nom_colonne)
  moyenne <- round(mean(base[[nom_colonne]]), 4)
  mediane <- round(median(base[[nom_colonne]]), 4)
  ecart_type <- round(sd(base[[nom_colonne]]), 4)
  minimum <- round(min(base[[nom_colonne]]), 4)
  maximum <- round(max(base[[nom_colonne]]), 4)
  kurtosis <- round(e1071::kurtosis(base[[nom_colonne]]), 4)
  skewness <- round(e1071::skewness(base[[nom_colonne]]), 4)
  
  resultat <- data.frame(
    "Statistiques" = c(
      "Moyenne",
      "Médiane",
      "Ecart-type",
      "Minimum",
      "Maximum",
      "Kurtosis",
      "Skewness"
    ),
    "Valeur" = c(
      moyenne,
      mediane,
      ecart_type,
      minimum,
      maximum,
      kurtosis,
      skewness
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

statistiques(pop,"France")







statistiques(pop,pays_proche(pop,"France"))



