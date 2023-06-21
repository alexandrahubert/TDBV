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
    n_rad = n()
  ) %>%
  ungroup() %>%
  mutate(dist_inter_rad = (borne_der_rad - borne_prem_rad) / (n_rad - 1)) %>%
  select(Ref_sta,
         dist_inter_rad)

# assemblage du tableau de données
data <- station %>%
  select(Ref_sta,
         comm,
         topo,
         lieu_dit,
         Code_tron,
         jeu_donnees = Etude) %>%
  left_join(y = caractere_bv %>%
              select(Ref_sta,
                     Surface_BV_km2 = `surface BV (en km²)`)) %>%
  left_join(y = pente %>%
              select(Ref_sta,
                     pente_eau)) %>%
  mutate(#pente_eau_m_m = ifelse(pente_eau_m_m < 0, NA, pente_eau_m_m),
         pente_eau_m_m = pente_eau / 100) %>%
  left_join(y = dist_rad %>%
              select(Ref_sta,
                     dist_inter_rad)) %>%
  left_join(
    y = tdbv_stations_aval_l93_20200612 %>%
      select(
        Ref_sta,
        Lpb = Lpb_moy,
        Hpb = Htot_moy,
        Pentea_m_m,
        Coef_sinuo
      )
  ) %>%
  mutate(
    jeu_donnees = ifelse(
      str_detect(jeu_donnees, "Gallineau"),
      yes = "galineau_2020",
      no = "tbv_ref"
    ),
    pente_eau_m_m = ifelse(is.na(pente_eau_m_m),
                           yes = Pentea_m_m,
                           no = pente_eau_m_m),
    pente_eau_m_m = ifelse(pente_eau_m_m < 0,
                         yes = NA,
                         no = pente_eau_m_m)
  ) %>% 
  select(-Pentea_m_m,
         -pente_eau) %>% 
  select(Ref_sta:Code_tron,
         jeu_donnees,
         Surface_BV_km2,
         pente_eau_m_m,
         everything()) %>% 
  filter(Ref_sta != "Ref_0061") # pb sur la surface du bv

# Sélection des stations
# selon les sites, sur les Code_tron, ou bien sur les lieu_dit
# NB manque une station du 44, contact D Fatin, SMBV Isac ?
# troncons_a_supprimer <- c(1, 5, 10, 11, 12, 14, 17, 20:24, 26:32)
# lieux_dits_a_supprimer <- c('La Chauvinière','Le Champ-Fleury')

ref_data <- data %>% 
  # filter(!Code_tron %in% troncons_a_supprimer) %>%
  # filter(!lieu_dit %in% lieux_dits_a_supprimer)
  mutate(num = str_sub(Ref_sta, -3, -1),
         num = as.integer(num)) %>% 
  filter(num < 201 | num > 235) %>% 
  select(-num)

# Données Carhyce téléchargées depuis l'IED https://analytics.huma-num.fr/ied_carhyce/
# filtré sur REGION == ARMORICAIN
carhyce <- data.table::fread("raw_data/Operations_2022-07-19.csv",
                             encoding = "UTF-8") %>% 
  select(localisation = `Localisation station de mesure`,
         topo = `Cours d'eau`,
         Surface_BV_km2 = `Surface BV (km2)`,
         Coeff_K = `Coefficient rugosité`,
         D16 = `D16 (mm)`,
         D50 = `D50 (mm)`,
         D84 = `D84 (mm)`,
         pente_eau_m_km = `Pente ligne d'eau (‰)`,
         Lpb = `Largeur plein bord évaluée (m)`,
         Hpb = `Profondeur moyenne Qb (m)`,
         Coef_sinuo = `Coefficient de sinuosité`,
         ref = `Station référence modèle`
         ) %>% 
  mutate_at(vars(Surface_BV_km2:Coef_sinuo),
            function(x) str_replace(x, pattern = ",", replacement = ".")) %>% 
  mutate_at(vars(Surface_BV_km2:Coef_sinuo),
            as.numeric) %>% 
  mutate(jeu_donnees = 'carhyce_ref_armo',
         Ref_sta = NA,
         lieu_dit = NA,
         Code_tron = NA,
         pente_eau_m_m = pente_eau_m_km / 1000,
         dist_inter_rad = NA,
         moy_chute = NA,
         moy_fd = NA) %>% 
  select(-pente_eau_m_km)

ref_carhyce <- carhyce %>% 
  filter(ref == 1) %>% 
  mutate(localisation = str_to_upper(localisation),
         localisation = str_replace(localisation,
                                    pattern = " À ",
                                    replacement = " A "),
         comm = word(localisation,
                     start = -1,
                     sep = ' A ')) %>% 
  select(names(ref_data))

identical(names(ref_carhyce), names(ref_data))

ref <- rbind(ref_data, ref_carhyce) %>% 
  mutate(etiquette = paste0(Ref_sta, ", ", topo),
         l_h = Lpb/Hpb)

# conserver le RData dans le répertoire du Rmd en vue du déploiement de l'appli
save(ref, file = "scripts/ref.RData")
save(data, file = "scripts/data1.RData")
  
  
