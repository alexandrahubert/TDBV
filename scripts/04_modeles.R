rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)

# chargement des fonctions
source(file = "scripts/00_fonctions.R")

# chargement des données au format RData
load(file = "processed_data/ref.RData")

# ---------------------------------------------------
# calage des modèles
# ---------------------------------------------------

# Largeur plein bord

m_lpb_tbv <- lm_unitaire(
  data = ref,
  var_dep = "Lpb",
  jeu_donnees_selectionne = "tbv_ref"
)

m_hpb_tbv <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = "tbv_ref"
)

m_lpb_gal <- lm_unitaire(
  data = ref,
  var_dep = "Lpb",
  jeu_donnees_selectionne = "gallineau_2020"
)

m_hpb_gal <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = "gallineau_2020"
)

m_lpb_car <- lm_unitaire(
  data = ref,
  var_dep = "Lpb",
  jeu_donnees_selectionne = "carhyce_ref_armo"
)

m_hpb_car <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = "carhyce_ref_armo"
)

# m7 <- lm_unitaire(
#   data = ref,
#   var_dep = "l_h",
#   jeu_donnees_selectionne = "tbv_ref"
# )
# 
# m8 <- lm_unitaire(
#   data = ref,
#   var_dep = "l_h",
#   jeu_donnees_selectionne = "gallineau_2020"
# )
# 
# m9 <- lm_unitaire(
#   data = ref,
#   var_dep = "l_h",
#   jeu_donnees_selectionne = "carhyce_ref_armo"
# )



modeles <- rbind(m_lpb_tbv,
                 m_lpb_gal,
                 m_lpb_car,
                 m_hpb_tbv,
                 m_hpb_gal,
                 m_hpb_car#,
               #  m7,
              #   m8,
               #  m9
              ) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  separate(
    col = rowname,
    into = c("Variable dépendante", "Jeu de données"),
    sep = " / "
  )






save(modeles,
     file = "output/modeles.RData")

