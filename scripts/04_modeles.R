rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)

# chargement des fonctions
source(file = "scripts/00_fonctions.R")

# chargement des données au format RData
load(file = "scripts/ref.RData")

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
  jeu_donnees_selectionne = "galineau_2020"
)

m_hpb_gal <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = "galineau_2020"
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

# assemblage du tableau. Les modèles sont des listes dont le 2e élement est mis en forme
# depuis le summary()
modeles <- rbind(m_lpb_tbv[[2]],
                 m_lpb_gal[[2]],
                 m_lpb_car[[2]],
                 m_hpb_tbv[[2]],
                 m_hpb_gal[[2]],
                 m_hpb_car[[2]]) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  separate(
    col = rowname,
    into = c("Variable dépendante", "Jeu de données"),
    sep = " / "
  )

# --------------------------------------------------------------------
# modèmes simplifiés avec seulement surface BV en var expl mais jdd variables
# largeur
ms_lpb_tbv <- lm_unitaire(
  data = ref,
  var_dep = "Lpb",
  jeu_donnees_selectionne = c("tbv_ref"),
  pente_incluse = FALSE
)

ms_lpb_gal <- lm_unitaire(
  data = ref,
  var_dep = "Lpb",
  jeu_donnees_selectionne = c("galineau_2020"),
  pente_incluse = FALSE 
)

ms_lpb_car <- lm_unitaire(
  data = ref,
  var_dep = "Lpb",
  jeu_donnees_selectionne = c("carhyce_ref_armo"),
  pente_incluse = FALSE 
)

ms_lpb_tbv_gal <- lm_unitaire(
  data = ref,
  var_dep = "Lpb",
  jeu_donnees_selectionne = c("tbv_ref", "galineau_2020"),
  pente_incluse = FALSE 
)

ms_lpb_tbv_gal_car <- lm_unitaire(
  data = ref,
  var_dep = "Lpb",
  jeu_donnees_selectionne = c("tbv_ref", "galineau_2020", "carhyce_ref_armo"),
  pente_incluse = FALSE 
)

# hauteur
ms_hpb_tbv <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = c("tbv_ref"),
  pente_incluse = FALSE
)

ms_hpb_gal <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = c("galineau_2020"),
  pente_incluse = FALSE 
)

ms_hpb_car <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = c("carhyce_ref_armo"),
  pente_incluse = FALSE 
)

ms_hpb_tbv_gal <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = c("tbv_ref", "galineau_2020"),
  pente_incluse = FALSE 
)

ms_hpb_tbv_gal_car <- lm_unitaire(
  data = ref,
  var_dep = "Hpb",
  jeu_donnees_selectionne = c("tbv_ref", "galineau_2020", "carhyce_ref_armo"),
  pente_incluse = FALSE 
)


modeles_simp <- rbind(ms_lpb_tbv[[2]],
                      ms_lpb_gal[[2]],
                      ms_lpb_car[[2]],
                      ms_lpb_tbv_gal[[2]],
                      ms_lpb_tbv_gal_car[[2]],
                      ms_hpb_tbv[[2]],
                      ms_hpb_gal[[2]],
                      ms_hpb_car[[2]],
                      ms_hpb_tbv_gal[[2]],
                      ms_hpb_tbv_gal_car[[2]]
) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  separate(
    col = rowname,
    into = c("Variable dépendante", "Jeu de données"),
    sep = " / "
  )

# modèles finaux retenus. on en récupère le 1er élément qui permettra d'utiliser predict.lm()
mf_lpb <- ms_lpb_tbv[[1]]
mf_hpb <- ms_hpb_tbv[[1]]

# conserver le RData dans le répertoire du Rmd en vue du déploiement de l'appli
save(modeles,
     modeles_simp,
     mf_lpb,
     mf_hpb,
     file = "scripts/modeles.RData")

