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
  
  return (datatable(resultat, 
                    options = list(
                      dom = "ftip" # i = information, f = filtre, p = pagination, t = table
                    ),
                    class = "cell-border hover"))
}

statistiques(pop,"Aruba")



### *Ex 4 : Utilisation du jeu de données `iris` pour apprendre à utiliser les `options` de `{DT}`*    
```{r dt}

### *Ex 5 : Modifier l'aspect de la table*    
Il existe dans {DT} des styles css prédéfinis et applicables grâce au paramètre class qui peut prendre comme valeurs :   
  
_ cell-border : pour une bordure pleine des cellules  
_ compact : réduit les espaces entre les lignes  
_ hover : mise en avant de la ligne lors du survol du curseur  
_ nowrap : pour supprimer les retours à la ligne dans les cellules  
_ order-column : mise en évidence de la colonne selon laquelle est triée la table  
_ row-border : pour une bordure uniquement en haut et en bas des colonnes (ne peut être utilisé en même temps que cell-border pour des raisons évidentes)  
_ stripe : pour des lignes “rayées”, c’est-à-dire avec deux couleurs alternées  
_ display : pour l’ensemble stripe, hover, row-border et order-column  
```{r changer_apparence_dt}
# changer l'apparence
iris %>%
  datatable(class = "cell-border compact display")
```

### *Ex 6 : Intéragire avec les données*  
```{r interaction_dt}
# laisser à l'utilisateur la possibilité de modifier les données
iris %>%
  datatable(
    options = list(pageLength = 5), 
    # editable = "row" 
    editable = "cell"
  )

# enregistrer un fichier
iris %>%
  datatable(rownames = FALSE, 
            extensions = c("Buttons"),
            options = list(
              dom = "Bfrtip", 
              pageLength = 5,
              buttons = c(
                'copy', 'csv', 'excel', 'pdf', 'print'
              )
            )
  )

# sélection des lignes à enregistrer
iris %>%
  datatable(rownames = FALSE, 
            extensions = c("Buttons", "Select"),
            options = list(
              dom = "Bfrtip", 
              pageLength = 5,
              select = list(style = 'os', items = 'row'),
              buttons = c(
                'copy', 'csv', 'excel', 'pdf', 'print',
                # selection des elements
                'selectAll', 'selectNone', 'selectRows'
              )
            )
  )



