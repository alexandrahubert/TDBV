##########################################################
# NB : Script pour une version Access 32 bits => R doit aussi être une version 32 bits
# Tools-> Global Options-> Change R version -> choisir R 32 bit
##########################################################


rm(list = ls()) # nettoyage de l'espace

# chargement des packages
library(tidyverse)
library(RODBC)

# connexion à la base Access
channel <- RODBC::odbcConnectAccess2007(access.file = "raw_data/Tête_BV.accdb")

# importation des tables qui seront utilisées
station <- sqlFetch(channel, "Station")
caractere_bv <- sqlFetch(channel, "caractere_BV")
facies <- sqlFetch(channel, "Facies")
mesures_wolman <- sqlFetch(channel, "Mesures_Wolman")
pente <- sqlFetch(channel, "Pente")
prof_chute <- sqlFetch(channel, "Prof_chute")
rugosite <- sqlFetch(channel, "Rugosite")
tdbv_stations_aval_l93_20200612 <-
  sqlFetch(channel, "TDBV_stations_aval_L93_20200612")


# sauvegarde des données au format RData
save.image(file = "processed_data/tables_access.RData")
