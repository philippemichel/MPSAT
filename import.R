

#  ------------------------------------------------------------------------
#
# Title : Import
#    By : PhM
#  Date : 2024-04-05
#
#  ------------------------------------------------------------------------

importph <- function(){
rm(list=ls())
library(readODS)
library(tidyverse)
library(janitor)
library(baseph)
library(labelled)
#
nan <- c("NA","ND",""," ", "Manquant")

mp <- read_ods("datas/MPSAT.ods", sheet = "mpsat", na = nan) |>
  janitor::clean_names() |>
  dplyr::mutate_if(is.character, as.factor)
bn <- read_ods("datas/MPSAT.ods", sheet = "bnom")
var_label(mp) <- bn$nom
mp <- mp |>
  dplyr::select(1,2,11:20)
#
tt <- read_ods("datas/questions.ods", sheet = "quest",na = nan) |>
  janitor::clean_names() |>
  dplyr::mutate_if(is.character, as.factor)
bn <- read_ods("datas/questions.ods", sheet = "noms")
var_label(tt) <- bn$Nom
  #BMI
tt <- tt |>
  mutate(bmi = 10000*poids/taille^2) |>
  mutate(bmi = baseph::bmiph(bmi,"fr")) |>
  relocate(bmi, .after = "taille") |>
# HEIQ
mutate(across(starts_with("heiq"), function(x) factor(x,
   c( "En accord", "Fortement en accord", "En désaccord", "Fortement en désaccord"
  )))) |>
mutate(across(starts_with("heiq"), function(x) fct_relevel(x,
                                                               "Fortement en désaccord","En désaccord", "En accord", "Fortement en accord"))) |>
  mutate(across(starts_with("vq11"), function(x) factor(x,levels(tt$vq11_3)))) |>
  mutate(across(starts_with("vq11"), function(x) fct_relevel(x,
 "Pas du tout","Un peu", "Moyennement", "Beaucoup","Extrêmement"))) |>
  #VQ11
  mutate(across(starts_with("vq11"), function(x) factor(x,levels(tt$vq11_3)))) |>
  mutate(across(starts_with("vq11"), function(x) fct_relevel(x,
 "Pas du tout", "Un peu", "Moyennement", "Beaucoup"))) |>
  ## Réordonnancement de tt$breq_2_1
  mutate(across(starts_with("breq"), function(x) fct_relevel(x,
    "Pas vrai du tout", "Moyennement vrai", "Tout à fait vrai"))) |>
## Réordonnancement de tt$satisfaction_2
mutate(satisfaction_2 = fct_relevel(satisfaction_2,
    "Dans la journée", "En 24 heures", "En 48 heures", "En 1 semaine",
    "Plus"))
#
tt <- left_join(tt, mp, by = "id")
#
dd <- read_ods("datas/MPSAT-ETP_dates.ods") |>
  clean_names() |>
  mutate(across(starts_with("date"),mdy )) |>
  mutate(across(starts_with("date_hosp"), function(x) as.numeric(x - date_integration))) |>
  mutate(across(starts_with("date_hosp"), function(x) as.factor(ifelse((x>365 | x<0),"non","oui"))))
#

save(tt, bn, dd, file = "datas/quest.RData")
}

load(file = "datas/quest.RData")
