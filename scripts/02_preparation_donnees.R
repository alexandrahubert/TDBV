rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)
library(sf)

# Chargement des contours de la HER "Massif armoricain"
her1 <- tod::wfs_sandre(url_wfs = "https://services.sandre.eaufrance.fr/geo/mdo?",
                        couche = "Hydroecoregion1") %>% 
  filter(NomHER1 == "ARMORICAIN")

mapview::mapview(her1)


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

# granulometrie
granulo <- tdbv_stations_aval_l93_20200612 %>% 
  select(Ref_sta,
         d16 = D16,
         d50 = D50,
         d84 = D84,
         Ra_D16_D84) %>% 
  filter(!is.na(d16))

# assemblage du tableau de données
data <- station %>%
  filter(Etude != "Colin, 2015") %>% 
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
      yes = "Galineau",
      no = "Jan & Bossis"
    ),
    pente_eau_m_m = ifelse(is.na(pente_eau_m_m),
                           yes = Pentea_m_m,
                           no = pente_eau_m_m),
    pente_eau_m_m = ifelse(pente_eau_m_m < 0,
                         yes = NA,
                         no = pente_eau_m_m)
  ) %>% 
  left_join(y = mesures_wolman) %>% 
  select(-Pentea_m_m,
         -pente_eau,
         -Dmoyen) %>% 
  rename(D16_D84 = `D16/D84`) %>% 
  select(Ref_sta:Code_tron,
         jeu_donnees,
         Surface_BV_km2,
         pente_eau_m_m,
         everything()) %>% 
  filter(Ref_sta != "Ref_0061") %>% # pb sur la surface du bv
  left_join(y = granulo) %>% 
  mutate(D16 = ifelse(is.na(D16), d16, D16),
         D50 = ifelse(is.na(D50), d50, D50),
         D84 = ifelse(is.na(D84), d84, D84),
         D16_D84 = ifelse(is.na(D16_D84), d16 / D84, D16_D84)) %>%
  select(-(d16:Ra_D16_D84))

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
  select(code_station = `Code station`,
         localisation = `Localisation station de mesure`,
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
         ref = `Station référence modèle`,
         x_l93 = X,
         y_l93 = Y
         ) %>% 
  mutate_at(vars(Surface_BV_km2:Coef_sinuo, x_l93, y_l93),
            function(x) str_replace(x, pattern = ",", replacement = ".")) %>% 
  mutate_at(vars(Surface_BV_km2:Coef_sinuo, x_l93, y_l93),
            as.numeric) %>% 
  mutate(jeu_donnees = 'CARHYCE',
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
                     sep = ' A '),
         D16_D84 = D16 / D84
         )

codes_station_ref_carhyce <- unique(ref_carhyce$code_station)

ref_carhyce <- ref_carhyce %>%  
  select(names(ref_data))

identical(names(ref_carhyce), names(ref_data))

ref <- rbind(ref_data, ref_carhyce) %>% 
  mutate(etiquette = paste0(Ref_sta, ", ", topo),
         l_h = Lpb / Hpb)

### Spatialisation
ref_geo <- ref %>% 
  left_join(station) %>%
#  filter(!is.na(Sta_GPS_avl_X)) %>% 
  mutate(x_wgs84 = ifelse(Sta_GPS_avl_X < 10,
                          Sta_GPS_avl_X,
                          Sta_GPS_avl_Y),
         y_wgs84 = ifelse(Sta_GPS_avl_Y < 10,
                          Sta_GPS_avl_X,
                          Sta_GPS_avl_Y),
         x_wgs84 = ifelse(Ref_sta %in% c("Ref_0002", "Ref_0005", "Ref_0146"),
                          (-1) * x_wgs84,
                          x_wgs84),
         x_wgs84 = ifelse(Ref_sta == "Ref_0226",
                          -0.71195,
                          x_wgs84),
         y_wgs84 = ifelse(Ref_sta == "Ref_0226",
                          48.12197,
                          y_wgs84)
  ) %>% 
#  select(Ref_sta, jeu_donnees, x_wgs84, y_wgs84) %>% 
  filter(y_wgs84 > 0) %>% 
  sf::st_as_sf(coords = c("x_wgs84", "y_wgs84"),
               crs = 4326) %>% 
  sf::st_transform(crs = 2154)

mapview::mapview(ref_geo)

ref_carhyce_geo <- carhyce %>% 
  filter(code_station %in% codes_station_ref_carhyce) %>%
#  select(code_station, x_l93, y_l93) %>% 
  distinct() %>% 
  # drop_na() %>% 
  sf::st_as_sf(coords = c("x_l93", "y_l93"),
               crs = 2154) %>% 
  mutate(jeu_donnees = "CARHYCE")

mapview::mapview(ref_carhyce_geo)

ref_geo <- ref_geo %>% 
  bind_rows(ref_carhyce_geo)

# to avoid error => turned off spherical geometry NOAA-EDAB/survdat#43
# see https://github.com/r-spatial/sf/issues/1762
sf_use_s2(FALSE)

mapview::mapview(her1,
                 alpha.region = 0.1,
                 col.region = "lightgreen",
                 layer.name = c("Massif Armoricain"),
                 map.types = c("Esri.WorldShadedRelief", "OpenStreetMap")) +
  mapview::mapview(ref_geo,
                   zcol = "jeu_donnees")

her1_buff <- her1 %>% 
  st_transform(crs = 2154) %>% 
  st_buffer(0.03)

ref_geo <- ref_geo %>% 
  st_join(y = her1_buff) %>% 
  filter(!is.na(NomHER1))



mapview::mapview(her1_buff,
                 alpha.region = 0.1,
                 col.region = "lightgreen",
                 layer.name = c("Massif Armoricain"),
                 map.types = c("Esri.WorldShadedRelief", "OpenStreetMap")) +
  mapview::mapview(ref_geo,
                   zcol = "jeu_donnees")

ref_filtered <- ref %>% 
  filter()

# Nb de sites par source ------
n_sites_par_source <- ref %>%
  count(jeu_donnees) %>%
  as.data.frame()

# R2 et pvalue de la relation pente - surface BV
relation_pente_sbv <- lm(log10(pente_eau_m_m) ~ log10(Surface_BV_km2),
                         data = ref %>% filter(pente_eau_m_m > 0, Surface_BV_km2 > 0))

pente_sbv_r2 <- summary(relation_pente_sbv)$r.squared %>% 
  round(2)

pente_sbv_pvalue <- anova(relation_pente_sbv)$`Pr(>F)`[1] %>% 
  signif(3)


### Sauvegarde
# Données : conserver le RData dans le répertoire du Rmd en vue du déploiement de l'appli
save(ref,
     ref_geo,
     her1,
     n_sites_par_source,
     pente_sbv_r2,
     pente_sbv_pvalue,
     file = "scripts/ref.RData")

save(data, file = "scripts/data1.RData")

# couche géo des points
sf::st_write(obj = ref_geo,
             dsn = "processed_data/stations_ref_hydromorpho.shp")
  
  
