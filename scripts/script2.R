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

# calcul des moyennes de profondeur de chute
moyenne <- prof_chute %>%
  group_by(Ref_sta) %>%
  summarise(moy_chute = mean(Hauteur_chute),
            moy_fd = mean(Profondeur_FD)) %>%
  ungroup() %>%
  select(Ref_sta,
         moy_chute, moy_fd)



# assemblage du tableau de données
data <- station %>%
  select(Ref_sta,
         comm,
         topo,
         lieu_dit,
         Code_tron) %>%
  left_join(y = caractere_bv %>%
              select(Ref_sta,
                     surf_bv_access = `surface BV (en km²)`)) %>%
  # left_join(y = mesures_wolman %>%
  #             select(Ref_sta,
  #                    D16,
  #                    D50,
  #                    D84)) %>%
  left_join(y = pente %>%
              select(Ref_sta,
                     pente_eau)) %>%
  left_join(y = dist_rad %>%
              select(Ref_sta,
                     dist_inter_rad)) %>%
  left_join(y = tdbv_stations_aval_l93_20200612 %>%
              select(Ref_sta,
                     Surface_BV,
                     Lpb_moy,
                     Htot_moy,
                     Pentea_m_m,
                     Coef_sinuo,
                     jeu_donnees = Reseau_etu)) %>%
  left_join(y = moyenne %>%
              select(Ref_sta,
                     moy_chute,
                     moy_fd)) %>%
  mutate(jeu_donnees = ifelse(str_detect(jeu_donnees, "Gallineau"),
                              yes = "gallineau_2020",
                              no = "tbv_ref")) %>% 
  select(Ref_sta:Code_tron,
         jeu_donnees,
         Surface_BV,
         Pentea_m_m,
         everything())

# Sélection des stations
# selon les sites, sur les Code_tron, ou bien sur les lieu_dit
# NB manque une station du 44, contact D Fatin, SMBV Isac ?
troncons_a_supprimer <- c(1, 5, 10, 11, 12, 14, 17, 20:24, 26:32)
lieux_dits_a_supprimer <- c('La Chauvinière','Le Champ-Fleury')

ref_data <- data %>% 
  filter(!Code_tron %in% troncons_a_supprimer) %>%
  filter(!lieu_dit %in% lieux_dits_a_supprimer) %>% 
  mutate(jeu_donnees = 'tbv_ref')

# Données Carhyce téléchargées depuis l'IED https://analytics.huma-num.fr/ied_carhyce/
# filtré sur REGION == ARMORICAIN
carhyce <- data.table::fread("raw_data/Operations_2022-07-19.csv",
                             encoding = "UTF-8") %>% 
  select(localisation = `Localisation station de mesure`,
         topo = `Cours d'eau`,
         Surface_BV = `Surface BV (km2)`,
         Coeff_K = `Coefficient rugosité`,
         D16 = `D16 (mm)`,
         D50 = `D50 (mm)`,
         D84 = `D84 (mm)`,
         pente_eau = `Pente ligne d'eau (‰)`,
         Lpb_moy = `Largeur plein bord évaluée (m)`,
         Htot_moy = `Profondeur moyenne Qb (m)`,
         Coef_sinuo = `Coefficient de sinuosité`,
         ref = `Station référence modèle`
         ) %>% 
  
  
  mutate_at(vars(Surface_BV:Coef_sinuo), function(x) str_replace(x, pattern = ",", replacement = ".")) %>% 
  mutate_at(vars(Surface_BV:Coef_sinuo), as.numeric) %>% 
  mutate(jeu_donnees = 'carhyce_ref_armo',
         Ref_sta = NA,
         lieu_dit = NA,
         Code_tron = NA,
         Pentea_m_m = NA,
         dist_inter_rad = NA,
         moy_chute = NA,
         moy_fd = NA,
         surf_bv_access = NA)

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

ref <- rbind(ref_data, ref_carhyce)
  
  


g <- ggplot(data = ref,
       aes(x = Surface_BV,
           y = Lpb_moy,
           col = jeu_donnees)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

g


model <- glm(log(Lpb_moy, base = 10) ~ log(Surface_BV, base = 10),
            data = ref_carhyce)
summary(model)

g1 <- ggplot(data = ref,
            aes(x = Surface_BV,
                y = pente_eau,
                col = jeu_donnees)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm")

g1

model <- glm(log(pente_eau, base = 10) ~ log(Surface_BV, base = 10),
             data = ref_carhyce)
summary(model)

g2 <- ggplot(data = ref %>% filter(Surface_BV > 0.1),
             aes(x = Surface_BV,
                 y = Htot_moy,
                 col = jeu_donnees)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "gam")

g2

model <- glm(log(Htot_moy, base = 10) ~ log(Surface_BV, base = 10),
             data = ref %>% filter(Surface_BV > 0.1,
                                   jeu_donnees == "tbv_ref"))

summary(model)



model <- glm(log(Htot_moy, base = 10) ~ log(Surface_BV, base = 10),
             data = ref_carhyce )
summary(model)

