

# chargement des packages
library(tidyverse)
library(RODBC)

# connexion à la base Access
my_file <- "//sp000035/partages_$/dr35/8_Stagiaires/2022TeteDeBassin_BOETON_Julien/2022_07_21_Tête_BV.accdb"
channel <- RODBC::odbcConnectAccess2007(access.file = my_file)

#channel <- RODBC::odbcConnectAccess2007(access.file = "//ad.intra/dfs/COMMUNS/REGIONS/BRE/DR/dr35/donnees_partage/6Stagiaires/2022TeteDeBassin_BOETON_Julien/2022_07_21_Tête_BV.accdb")

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
