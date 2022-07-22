rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)
library(plotly)
library(wesanderson)

# chargement des fonctions
source(file = "scripts/00_fonctions.R")

# chargement des données au format RData
load(file = "processed_data/ref.RData")

# ---------------------------------------------------
# calage des modèles
# ---------------------------------------------------

# Largeur plein bord

m1 <- lm_unitaire(
  data = ref,
  var_dep = "Lpb_moy",
  jeu_donnees_selectionne = "tbv_ref"
)

m2 <- lm_unitaire(
  data = ref,
  var_dep = "Htot_moy",
  jeu_donnees_selectionne = "tbv_ref"
)

m3 <- lm_unitaire(
  data = ref,
  var_dep = "Lpb_moy",
  jeu_donnees_selectionne = "gallineau_2020"
)

m4 <- lm_unitaire(
  data = ref,
  var_dep = "Htot_moy",
  jeu_donnees_selectionne = "gallineau_2020"
)

m5 <- lm_unitaire(
  data = ref,
  var_dep = "Lpb_moy",
  jeu_donnees_selectionne = "carhyce_ref_armo"
)

m6 <- lm_unitaire(
  data = ref,
  var_dep = "Htot_moy",
  jeu_donnees_selectionne = "carhyce_ref_armo"
)


modeles <- rbind(m1, m2, m3, m4, m5, m6) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  separate(col = rowname,
           into = c("Variable dépendante", "Jeu de données"),
           sep = " / ")
  



save(modeles,
     file = "output/modeles.RData")
