## code to prepare `DATASET` dataset goes here

url_jeu_de_donnees <- "https://explore.data.gouv.fr/fr/datasets/57868eb7a3a7295d371add08/#/resources/e86064d6-0b0b-4e1b-90fb-fe016fd8e117"

library(readr)
library(dplyr)
library(usethis)

df_poissons <- read_csv2(
  "https://opendata.hauts-de-seine.fr/api/explore/v2.1/catalog/datasets/diversite-des-poissons-dans-la-seine/exports/csv?lang=fr&timezone=Europe%2FBerlin&use_labels=true&delimiter=%3B"
)

df_poissons <- df_poissons %>%
  distinct()

use_data(df_poissons, overwrite = TRUE)
