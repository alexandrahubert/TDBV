rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)

# chargement des données au format RData
load(file = "processed_data/tables_access.RData")

# distance entre deux radiers
dist_rad <- facies %>%
  filter(type == "Rad") %>%
  group_by(Ref_sta) %>%
  summarise(
    borne_prem_rad = min(borne),
    borne_der_rad = max(borne),
    n_rad = n() - 1
  ) %>%
  ungroup() %>%
  mutate(dist_inter_rad = (borne_der_rad - borne_prem_rad) / n_rad) %>%
  select(Ref_sta,
         dist_inter_rad)

# calcul des moyennes de profondeur de chute
moyenne <- prof_chute %>% 
  group_by(Ref_sta) %>% 
  summarise(
    moy_chute = mean(Hauteur_chute),
    moy_fd = mean(Profondeur_FD)
  ) %>% 
  ungroup() %>% 
  select(Ref_sta,
         moy_chute, moy_fd)


# assemblage du tableau de données
data <- station %>%
  select(Ref_sta, comm, topo, lieu_dit) %>%
  left_join(y = rugosite %>%
              select(Ref_sta, Coeff_K)) %>%
  left_join(y = mesures_wolman %>%
              select(Ref_sta, D16, D50, D84)) %>%
  left_join(y = pente %>%
              select(Ref_sta, pente_eau)) %>%
  left_join(y = dist_rad %>%
              select(Ref_sta,
                     dist_inter_rad)) %>%
  left_join(y = tdbv_stations_aval_l93_20200612 %>%
              select(Ref_sta,
                     Surface_BV,
                     Lpb_moy,
                     Htot_moy)) %>%
  left_join(y = moyenne %>%
              select(Ref_sta,
                     moy_chute, moy_fd))



