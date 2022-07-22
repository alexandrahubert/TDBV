rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)
library(plotly)
library(wesanderson)

# chargement des fonctions
source(file = "scripts/00_fonctions.R")

# chargement des données au format RData
load(file = "processed_data/ref.RData")

# ----------------------------------------------------------
# Graphiques bivariés des variables de morpho en fonction de la surface du BV
# ----------------------------------------------------------

g_lpb_sbv <- mon_nuage(
  data = ref,
  x = Surface_BV_km2,
  y = Lpb_moy,
  col = jeu_donnees,
  label = etiquette,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "Largeur plein bord (m)"
)



g_pente_sbv <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = Surface_BV_km2,
  y = pente_eau_m_m,
  col = jeu_donnees,
  label = etiquette,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "Pente de la ligne d'eau (m/m)"
)


g_hpb_sbv <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = Surface_BV_km2,
  y = Htot_moy,
  col = jeu_donnees,
  label = etiquette,
  #        y_log = FALSE,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "Hauteur plein bord (m)"
)


# passage en plotly pour avoir l'interactivité
g_lpb_sbv <- ggplotly(g_lpb_sbv) %>%
  layout(legend = list(
    orientation = "h",
    x = 0,
    y = -0.2
  ))
g_pente_sbv <- ggplotly(g_pente_sbv) %>%
  layout(legend = list(
    orientation = "h",
    x = 0,
    y = -0.2
  ))
g_hpb_sbv <- ggplotly(g_hpb_sbv) %>%
  layout(legend = list(
    orientation = "h",
    x = 0,
    y = -0.2
  ))

# affichage
g_lpb_sbv
g_pente_sbv
g_hpb_sbv

# ----------------------------------------------------------
# Graphiques bivariés des variables de morpho en fonction de la pente
# ----------------------------------------------------------

g_lpb_pente <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = pente_eau_m_m,
  y = Lpb_moy,
  col = jeu_donnees,
  label = etiquette,
  x_lab = "Pente de la ligne d'eau (m/m)",
  y_lab = "Largeur plein bord (m)"
)



# g_pente_pente <- mon_nuage(data = ref %>%
#                            filter(pente_eau_m_m > 0),
#                          x = pente_eau_m_m,
#                          y = pente_eau_m_m,
#                          col = jeu_donnees,
#                          label = etiquette,
#                          x_lab = "Surface du bassin versant (km²)",
#                          y_lab = "Pente de la ligne d'eau (m/m)")


g_hpb_pente <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = pente_eau_m_m,
  y = Htot_moy,
  col = jeu_donnees,
  label = etiquette,
  #        y_log = FALSE,
  x_lab = "Pente de la ligne d'eau (m/m)",
  y_lab = "Hauteur plein bord (m)"
)


# passage en plotly pour avoir l'interactivité
g_lpb_pente <- ggplotly(g_lpb_pente) %>%
  layout(legend = list(
    orientation = "h",
    x = 0,
    y = -0.2
  ))
g_hpb_pente <- ggplotly(g_hpb_pente) %>%
  layout(legend = list(
    orientation = "h",
    x = 0,
    y = -0.2
  ))


# affichage
g_lpb_sbv
g_pente_sbv
g_hpb_sbv
g_lpb_pente
g_hpb_pente






save(g_lpb_sbv,
     g_pente_sbv,
     g_hpb_sbv,
     file = "output/graphiques_bivaries.RData")



