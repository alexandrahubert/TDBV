
# convertir R64 bit -> R32 bit :  Tools-> Global Options-> Change R version-> choisir R 32 bit


rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)
library(RODBC)

# odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../raw_data/Tête_BV.accdb")
# channel <- odbcConnect("TBV")
# channel
# Table1Dat <- sqlFetch(channel, "creationtable")

# connexion
channel <- odbcConnectAccess2007(access.file = "../raw_data/Tête_BV.accdb")

# importer les tables qui seront utilisées

station <- sqlFetch(channel, "Station")
caractere_bv <- sqlFetch(channel, "caractere_BV")
facies <- sqlFetch(channel, "Facies")
mesures_wolman <- sqlFetch(channel, "Mesures_Wolman")
pente <- sqlFetch(channel, "Pente")
prof_chute <- sqlFetch(channel, "Prof_chute")
rugosite <- sqlFetch(channel, "Rugosite")
tdbv_stations_aval_l93_20200612 <- sqlFetch(channel, "TDBV_stations_aval_L93_20200612")

#créer jointures
data <- station %>% 
  select(Ref_sta, comm, topo, lieu_dit) %>% 
  left_join(y = rugosite %>% 
              select(Ref_sta, Coeff_K)) %>%  
  left_join(y = mesures_wolman %>% 
              select(Ref_sta, D16, D50, D84)) %>% 
  left_join(y = pente %>% 
              select(Ref_sta, pente_eau)) %>% 
  left_join(y = mesures_wolman %>% 
              select(Ref_sta, D16, D50, D84)) %>%

