### PSO-Teildatensatz der NTx-Kinder erstellen ###
# Ziel: Datensatz von in NTx eingeschlossenen Kinder & Jugendlichen,
# mit den PSO-Variablen, die bisher im Rahmen des EvalBer ausgewertet wurden: 
# BAASIS, KINDL (insb. körperliche und psychische Subskalen), 
# SF12 (für Jugendliche), Familiärer Belastungsfragebogen (FABEL)

library(tidyverse)
library(lubridate)

# Grundgerüst
load('01_data/kids.rda')
dat <- kids %>% select(episode_code) 


#### stammdaten ####
load('O:/U3823_geschlossene_Gruppe/Abschlussbericht/Psychosomatik/PSO_kij/stamm_kij.rda')

tmp <- stamm %>% select(Code, Kognition:Pflegegrad)
dat <- left_join(dat, tmp, by=c('episode_code' = 'Code'))


#### baasis ####
load("O:/U3823_geschlossene_Gruppe/Abschlussbericht/Psychosomatik/PSO_kij/baasis_long_kids.Rda")

tmp <- baasis_long %>% 
  group_by(Code_eFA) %>% 
  mutate(n_obs=seq_along(month)) %>% 
  rename('basis_pc'= baasis_prozent) %>% 
  select(Code_eFA, month, baasis, basis_pc, n_obs) %>% 
  pivot_wider(names_from = n_obs, values_from = c(month, baasis, basis_pc), names_prefix = "mzp") %>% 
  select(Code_eFA, contains('1'), contains('2'), contains('3'), contains('4'), contains('5'), contains('6')) 

names(tmp) <- str_replace(names(tmp), "month_", "month_baasis_")
  
# an Grundgerüst dranhängen
dat <- left_join(dat, tmp, by=c('episode_code' = 'Code_eFA'))



#### KINDL ####
# Der KINDL kann bei den Kindern im Selbstbericht oder bei den Eltern im Fremdbericht 
# erhoben werden. Zusätzlich gibt es eine Version für jüngere Kinde (Kiddy) und die 
# reguläre Version für ältere Kinder (KINDL)

load("O:/U3823_geschlossene_Gruppe/Abschlussbericht/Psychosomatik/PSO_kij/KINDL_mix.Rda")

tmp <- KINDL_mix %>% 
  group_by(Code) %>% 
  mutate(n_obs=seq_along(month)) %>%
  select(Code, month, koerper, seele, kindl_ges, n_obs) %>%
  pivot_wider(names_from = n_obs, values_from = c(month,  koerper, seele, kindl_ges), names_prefix = "mzp") %>% 
  select(Code, contains('1'), contains('2'), contains('3'), contains('4'), contains('5'), contains('6'), contains('7'))

names(tmp) <- str_replace(names(tmp), "month_", "month_kindl_")

# an Grundgerüst dranhängen
dat <- left_join(dat, tmp, by=c('episode_code' = 'Code'))



#### FABEL ####
# = familiärer Belastungsfragebogen
load("O:/U3823_geschlossene_Gruppe/Abschlussbericht/Psychosomatik/PSO_kij/fabel.Rda")

## retrieve study inclusion date
assign('tmp', get(load('01_data/kids.rda')))

fabel <- left_join(fabel, tmp[c('episode_code', 'episode_therapie_beginn')], by=c('Code'='episode_code'))

tmp <- fabel %>% 
  mutate(month = (interval(start=episode_therapie_beginn, Datum) %/% months(1))) %>%
  group_by(Code) %>% 
  mutate(n_obs=seq_along(month)) %>%
  select(Code, month, fabel_ges, n_obs) %>%
  pivot_wider(names_from = n_obs, values_from = c(month,  fabel_ges), names_prefix = "mzp") %>% 
  select(Code, contains('1'), contains('2'), contains('3'))

names(tmp) <- str_replace(names(tmp), "month_", "month_fabel_")

# an Grundgerüst dranhängen
dat <- left_join(dat, tmp, by=c('episode_code' = 'Code'))



#### save ####
pso <- dat
save(pso, file='01_data/pso.rda')


