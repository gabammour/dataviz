# Fonctions pour la fusion :

# Librairie :
library(stringdist)
library(readxl)
library(DT)

# Fonction statistiques onglet 1 & 2  : ----
{
  statistiques <- function(base, nom_colonne) {
    if (all(is.na(base[[nom_colonne]]))) {
      moyenne = mediane = ecart_type = minimum = maximum = kurtosis = skewness = "Données vides pour le pays."
    }
    else {
      moyenne <- round(mean(base[[nom_colonne]], na.rm = TRUE), 4)
      mediane <- round(median(base[[nom_colonne]], na.rm = TRUE), 4)
      ecart_type <- round(sd(base[[nom_colonne]], na.rm = TRUE), 4)
      minimum <- round(min(base[[nom_colonne]], na.rm = TRUE), 4)
      maximum <- round(max(base[[nom_colonne]], na.rm = TRUE), 4)
      kurtosis <-
        round(e1071::kurtosis(base[[nom_colonne]], na.rm = TRUE), 4)
      skewness <-
        round(e1071::skewness(base[[nom_colonne]], na.rm = TRUE), 4)
    }
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
      datatable(
        resultat,
        rownames = FALSE,
        
        class = "cell-border hover",
        extensions = c("Buttons", "Select"),
        selection = 'none',
        options = list(
          dom = "Bfrtip",
          pageLength = 10,
          select = list(style = 'os', items = 'row'),
          buttons = c(
            'copy',
            'csv',
            'pdf',
            # selection des elements
            'selectAll',
            'selectNone',
            'selectRows'
          )
        )
      )
    )
  }
  
  #Fonctions onglet 1 :
  
  #Chargement des bases: ----
  
  pop <- read.csv2("Base/pop.csv", sep = ";")
  co2 <- read.csv2("Base/co2.csv", sep = ";")
  pib_hab_ppa <- read.csv2("Base/PIB_hab_ppa.csv", sep = ";")
  
  #Selection de la base de données: ----
  
  base_select <- function(nom_base) {
    vect_base <-
      c("Population", "CO2 par habitants", "PIB par habitants en PPA")
    if (nom_base == vect_base[1]) {
      base <- pop
    } else if (nom_base == vect_base[2]) {
      base <- co2
    } else if (nom_base == vect_base[3]) {
      base <- pib_hab_ppa
    }
    return(base)
  }
  
  #Selection du pays de saisie le plus proche d'un pays répertorié dans la base: ----
  pays_proche <- function(nom_base, nom_colonne) {
    base <- base_select(nom_base)
    list_name <-
      colnames(base)[-1] # On retire la première colonne (années)
    find <-
      stringdist(list_name, nom_colonne, method = "lv")
    return(unique(list_name[which(find == min(find))])[1])
  }
  
  #Plot:
  library(ggplot2)
  # ne pas oublier : colnames(pop)[1] <- "Date"
  
  plot_pop <- function(data, yvar, y_lab) {
    if (all(is.na(yvar))) {
      ggplot() +
        labs(title = "Jeu de données vide pour le pays.", x = "", y = "") +
        theme_void()
    } else {
      ggplot(data = data, aes(x = Date , y = yvar, group = 2)) +
        geom_line() +
        labs(
          title = paste("Evolution: ", y_lab , " de 1960 à 2022."),
          x = "Années",
          y = y_lab,
          color = "Légende : "
        ) +
        theme_light() +
        guides(color = "none") +
        theme(
          plot.title = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 20
          ),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)
        ) +
        scale_x_continuous(limits = c(1960, 2022),
                           breaks = seq(1960, 2022, by = 5))
    }
  }
  
  plot_hist <- function(data, xvar, x_lab) {
    if (all(is.na(xvar))) {
      ggplot() +
        labs(title = "Jeu de données vide pour le pays.", x = "", y = "") +
        theme_void()
    } else {
      ggplot(data = data, aes(x = xvar)) +
        geom_histogram(fill = "lightblue") +
        labs(
          title = paste("Histogramme : ", x_lab , " de 1960 à 2022."),
          x = x_lab,
          y = "Nombre",
          color = "Légende : "
        ) +
        theme_light() +
        guides(color = "none") +
        theme(
          plot.title = element_text(
            hjust = 0.5,
            vjust = 0.5,
            size = 20
          ),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)
        )
    }
  }
  
  # Fonctions onglet 2:  ----
  
  #Fonction de modification de la base
  mod_base <- function(adresse_base) {
    stopifnot(file.exists(adresse_base))
    df <- read.csv2(adresse_base , skip = 4, sep = ",")
    df <- df[-c(2, 4),-c(2, 4)]
    indicateur <- df[1, 2]
    df <- df[,-2]
    df <- as.data.frame(t(df))
    colnames(df) <- df[1,]
    df <- df[-c(1),]
    df <- apply(df, 2, as.numeric)
    df_dates <-
      data.frame(Date = as.numeric(seq(1960, 2022, by = 1)))
    df <- cbind(Date = df_dates, as.data.frame(df))
    return(list(df, indicateur))
  }
  
}
