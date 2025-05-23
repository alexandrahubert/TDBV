rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)
library(plotly)
library(scales)
library(wesanderson)
library(mapview)

# chargement des fonctions
source(file = "scripts/00_fonctions.R")

# chargement des données au format RData
load(file = "scripts/ref.RData")

# Graphiques bivariés des variables de morpho en fonction de la surface du BV

g_lpb_sbv <- mon_nuage(
  data = ref,
  x = Surface_BV_km2,
  y = Lpb,
  col = jeu_donnees,
  label = etiquette,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "Largeur à plein bord (m)"
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
  y = Hpb,
  col = jeu_donnees,
  label = etiquette,
  #        y_log = FALSE,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "Hauteur à plein bord (m)"
)

g_lh_sbv <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = Surface_BV_km2,
  y = l_h,
  col = jeu_donnees,
  label = etiquette,
  #        y_log = FALSE,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "Largeur sur Hauteur"
)

# granulo
logy <- FALSE

g_d16_sbv <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = Surface_BV_km2,
  y = D16,
  col = jeu_donnees,
  label = etiquette,
  x_log = TRUE,
  y_log = logy,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "D16 (mm)"
)

g_d50_sbv <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = Surface_BV_km2,
  y = D50,
  col = jeu_donnees,
  label = etiquette,
  x_log = TRUE,
  y_log = logy,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "D50 (mm)"
)

g_d84_sbv <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = Surface_BV_km2,
  y = D84,
  col = jeu_donnees,
  label = etiquette,
  x_log = TRUE,
  y_log = logy,
  x_lab = "Surface du bassin versant (km²)",
  y_lab = "D84 (mm)"
)

g_wol_sbv <- ggpubr::ggarrange(g_d16_sbv,
                               g_d50_sbv,
                               g_d84_sbv,
                               common.legend = TRUE,
                               ncol = 2,
                               nrow = 2)

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

g_lh_sbv <- ggplotly(g_lh_sbv) %>%
  layout(legend = list(
    orientation = "h",
    x = 0,
    y = -0.2
  ))

# affichage
g_lpb_sbv
g_pente_sbv
g_hpb_sbv
g_lh_sbv

# ----------------------------------------------------------
# Graphiques bivariés des variables de morpho en fonction de la pente
# ----------------------------------------------------------

g_lpb_pente <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = pente_eau_m_m,
  y = Lpb,
  col = jeu_donnees,
  label = etiquette,
  x_lab = "Pente de la ligne d'eau (m/m)",
  y_lab = "Largeur à plein bord (m)"
)

g_hpb_pente <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = pente_eau_m_m,
  y = Hpb,
  col = jeu_donnees,
  label = etiquette,
  #        y_log = FALSE,
  x_lab = "Pente de la ligne d'eau (m/m)",
  y_lab = "Hauteur à plein bord (m)"
)

g_lh_pente <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = pente_eau_m_m,
  y = l_h,
  y_log = FALSE,
  col = jeu_donnees,
  label = etiquette,
  x_lab = "Pente de la ligne d'eau (m/m)",
  y_lab = "Largeur sur Hauteur"
)

# granulo
g_d16_pente <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = pente_eau_m_m,
  y = D16,
  col = jeu_donnees,
  label = etiquette,
  x_log = TRUE,
  y_log = logy,
  x_lab = "Pente de la ligne d'eau (m/m)",
  y_lab = "D16 (mm)"
)

g_d50_pente <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = pente_eau_m_m,
  y = D50,
  col = jeu_donnees,
  label = etiquette,
  x_log = TRUE,
  y_log = logy,
  x_lab = "Pente de la ligne d'eau (m/m)",
  y_lab = "D50 (mm)"
)

g_d84_pente <- mon_nuage(
  data = ref %>%
    filter(pente_eau_m_m > 0),
  x = pente_eau_m_m,
  y = D84,
  col = jeu_donnees,
  label = etiquette,
  x_log = TRUE,
  y_log = logy,
  x_lab = "Pente de la ligne d'eau (m/m)",
  y_lab = "D84 (mm)"
)

g_wol_pente <- ggpubr::ggarrange(g_d16_pente,
                                 g_d50_pente,
                                 g_d84_pente,
                                 common.legend = TRUE,
                                 ncol = 2,
                                 nrow = 2)

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

g_lh_pente <- ggplotly(g_lh_pente) %>%
  layout(legend = list(
    orientation = "h",
    x = 0,
    y = -0.2
  ))

# affichage
g_lpb_sbv
g_pente_sbv
g_hpb_sbv
g_lh_sbv
g_lpb_pente
g_hpb_pente
g_lh_pente



# ----------------------------------------------------------
# Graphiques distributions en densité
# ----------------------------------------------------------

# 
g_densite_lpb <- ma_densite(data = ref,
                           x = Lpb,
                           x_lab = "Largeur à plein bord (m)")
g_densite_hpb <- ma_densite(data = ref,
                           x = Hpb,
                           x_lab = "Hauteur à plein bord (m)") 
g_densite_sbv <- ma_densite(data = ref,
                           x = Surface_BV_km2,
                           x_lab = "Surface du BV (km²), échelle log10",
                           x_log = TRUE)
g_densite_pente <- ma_densite(data = ref,
                           x = pente_eau_m_m,
                           x_lab = "Pente de la ligne d'eau (m/m)"
                           )
g_densite_lh <- ma_densite(data = ref,
                           x = l_h,
                           x_lab = "Ratio largeur / hauteur")

g_densite_d16 <- ma_densite(data = ref,
                           x = D16,
                           x_lab = "D16 (mm)")

g_densite_d50 <- ma_densite(data = ref,
                            x = D50,
                            x_lab = "D50 (mm)")

g_densite_d84 <- ma_densite(data = ref,
                            x = D84,
                            x_lab = "D84 (mm)")



g_densite <- ggpubr::ggarrange(g_densite_sbv,
                               g_densite_pente,
                               g_densite_lpb,
                               g_densite_hpb,
                               g_densite_lh,
                               common.legend = TRUE,
                               ncol = 2,
                               nrow = 3)

g_densite_wol <- ggpubr::ggarrange(g_densite_d16,
                                   g_densite_d50,
                                   g_densite_d84,
                                   common.legend = TRUE,
                                   ncol = 2,
                                   nrow = 2)

# carte ----
# ___________________________

donnees_carte <- ref_geo %>% 
  mutate(
  # jeu_donnees = case_when(
  #   jeu_donnees == "carhyce_ref_armo" ~ "Carhyce",
  #   jeu_donnees == "galineau_2020" ~ "Galineau (2020)",
  #   jeu_donnees == "tbv_ref" ~ "TBV référence"
  # ),
  Station = ifelse(is.na(Ref_sta), code_station, Ref_sta)) %>% 
  select(Station, Source = jeu_donnees)


# conserver le RData dans le répertoire du Rmd en vue du déploiement de l'appli
save(g_lpb_sbv,
     g_pente_sbv,
     g_hpb_sbv,
     g_lh_sbv,
     g_lpb_pente,
     g_hpb_pente,
     g_lh_pente,
     g_densite,
     g_densite_wol,
     g_wol_sbv,
     g_wol_pente,
     donnees_carte,
     file = "scripts/graphiques.RData")



