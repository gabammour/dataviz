# Librairie : 
library(DT)
library(moments)
pop <- read.csv2("Base/pop.csv", sep = ";")

 #Statistiques : 
statistiques <- function(base, nom_colonne) {
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


statistiques(pop,"Aruba")

library(stringdist)
pays_proche <- function(base, nom_colonne){ 
  list_name <- colnames(base)
  find <-
    stringdist(toupper(list_name), toupper(nom_colonne), method = "lv")
  return(toupper(unique(list_name[which(find == min(find))])[1]))
}


pays_proche(pop,"france")





