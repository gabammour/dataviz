# Librairie : 
library(DT)
library(stringdist)
library(ggplot2)

#Chargement des bases: 
pop <- read.csv2("Base/pop.csv", sep = ";")
co2 <- read.csv2("Base/co2.csv", sep = ";")
pib_hab_ppa <- read.csv2("Base/PIB_hab_ppa.csv", sep = ";")

#Selection de la base de données 
base_select <- function(nom_base){
  vect_base <- c("Population", "CO2 par habitants", "PIB par habitants en PPA")
  if (nom_base == vect_base[1]) { 
    base <- pop
  } else if (nom_base == vect_base[2]){ 
    base <- co2
  } else if(nom_base == vect_base[3]){ 
    base <- pib_hab_ppa}
  return(base)
}

#Selection du pays de saisie le plus proche d'un pays répertorié dans la base
pays_proche <- function(nom_base, nom_colonne){ 
  base <- base_select(nom_base)
  list_name <- colnames(base)[-1] # On retire la première colonne (années)
  find <-
    stringdist(list_name, nom_colonne, method = "lv")
  return(unique(list_name[which(find == min(find))])[1])
}

# Fonction des statistiques sur la base : 
statistiques <- function(nom_base, nom_colonne) {
  base <- base_select(nom_base)
  nom_colonne <- pays_proche(nom_base,nom_colonne)
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

statistiques("Population","France")



plot_pop <- function(data, xvar, yvar) {
  ggplot(data = data, aes_string(x = xvar, y = yvar, group = 2)) +
    geom_line() +
    labs(title = "Evolution de la population", x = "Années", y = "Population (en habitants)", color = "Légende : " ) +
    theme_light() +
    guides(color = "none")+ 
    theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))
}
plot_pop(pop, pop$Date, pop$France)
