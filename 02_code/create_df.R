### Kinder-Datensatz erstellen ###
# Ziel: Datensatz von in NTx eingeschlossenen Kinder & Jugendlichen,
# enthalten sein soll erstmal alles, was bisher im Rahmen des EvalBer ausgewertet wurde

library(tidyverse)
library(haven)

#### 1. Stammdaten ####
load('O:/U3823_geschlossene_Gruppe/Abschlussbericht/eFA Daten/efa_dates.rda')
load('O:/U3823_geschlossene_Gruppe/Abschlussbericht/eFA Daten/group.rda')
load('O:/U3823_geschlossene_Gruppe/Abschlussbericht/eFA Daten/covariates.rda')
# load('O:/U3823_geschlossene_Gruppe/Abschlussbericht/eFA Daten/code.rda')


# Grundgerüst
dat <- group %>% 
  filter(KiJ == 0) %>%
  select(episode_code, kvnr_hash_sec, episode_id, inz)

# Datumsangaben
tmp <- efa_dates %>% 
  select(episode_code, kvnr_hash_sec, transplantation_datum, behandlungseinrichtung_id,
         episode_therapie_beginn)

dat <- left_join(dat, tmp, by=c('episode_code', 'kvnr_hash_sec')) %>% 
  select(episode_code, kvnr_hash_sec, everything())

# restliche Kovariaten
tmp <- covariates %>%
  select(episode_code:episode_beendigungsart, hautfarbe:MCS12_t0, years_education_t0)

dat <- left_join(dat, tmp, by=c('episode_code', 'kvnr_hash_sec', 'episode_id')) 

#sortieren
dat <- dat %>% select(episode_code, kvnr_hash_sec, episode_id, ID_KK, everything())

## zwischenspeichern
kids <- dat
save(kids, file='01_data/kids.rda')


#### 2. PSO-Daten ####
load('01_data/pso.rda')
dat <- left_join(dat, pso, by='episode_code')


#### 3. IPV-Daten ####
load('01_data/ipv_KiJ.rda')
dat <- left_join(dat, ipv_KiJ, by=c('episode_code', 'episode_id', 'kvnr_hash_sec'))




#### 4. Routinedaten ####
# Datenbeschreibung hier:
# "O:\U3823_geschlossene_Gruppe\Krankenkassen\Abschlussdatenlieferung\Auswertungen Paul_U3823 Kinder\Datenbeschreibung Routinedaten Kinder.docx"

# Routinedaten (u.a. Adhärenz, Dialysebeginn)
load("O:/U3823_geschlossene_Gruppe/Paper_Kinder/01_data/routinedaten_kinder.Rdata")
# Hospitalisierungen
load("O:/U3823_geschlossene_Gruppe/Paper_Kinder/01_data/kh_daten_kinder.Rdata")
# OPS der Hospitalisierungen
load("O:/U3823_geschlossene_Gruppe/Paper_Kinder/01_data/ops_daten_kinder.Rdata")

# werden erstmal nicht an Primärdaten angehängt, weil nicht sinnvoll

## --> in dieser Form als .sav & xlsx ablegen
# change variable names (Variablennamen ungültig für .sav-Datensatz)
names(routinedaten_kinder) <- str_replace(names(routinedaten_kinder), '^0+|^1+', 'v')
haven::write_sav(routinedaten_kinder, "01_data/no/routinedaten_kinder.sav")
openxlsx::write.xlsx(routinedaten_kinder, '01_data/no/routinedaten_kinder.xlsx')

haven::write_sav(kh_daten_kinder, "01_data/no/kh_daten_kinder.sav")
openxlsx::write.xlsx(kh_daten_kinder, '01_data/no/kh_daten_kinder.xlsx')

haven::write_sav(ops_daten_kinder, "01_data/no/ops_daten_kinder.sav")
openxlsx::write.xlsx(ops_daten_kinder, '01_data/no/ops_daten_kinder.xlsx')

#### 5. save ####
## postprocessing

table(dat$hla_mismatches, useNA='always')

dat <- dat %>% mutate(hla_mismatches=case_when(
  hla_mismatches == 'EINS' ~ 1,
  hla_mismatches == 'ZWEI' ~ 2,
  hla_mismatches == 'DREI' ~ 3, 
  hla_mismatches == 'VIER' ~ 4, 
  hla_mismatches == 'FUENF' ~ 5, 
  hla_mismatches == 'SECHS' ~ 6, 
  TRUE ~ as.numeric(NA)
))

# remove wrong difftime units
dat$alter_bei_ntx <- as.numeric(dat$alter_bei_ntx, units="days")
dat$alter_ntx360 <- as.numeric(dat$alter_ntx360, units="days")

# change variable names
names(dat) <- str_replace(names(dat), 'ö', 'oe')
names(dat) <- str_replace(names(dat), 'ü', 'ue')
names(dat) <- str_replace(names(dat), 'ä', 'ae')
names(dat) <- str_replace(names(dat), '…', '')

kids <- dat
save(kids, file='01_data/kids_final.rda')

## SPSS
haven::write_sav(kids, "01_data/no/kids_final.sav")
## Excel
openxlsx::write.xlsx(kids, '01_data/no/kids_final.xlsx')




