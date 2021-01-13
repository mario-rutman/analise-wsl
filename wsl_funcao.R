# O trabalho para fazer um site com analises dos resultados dos campeonatos da 
# World Surf League terá 3 etapas: a raspagem dos dados; a análise; e, a apresentação
# no Shiny.
# Agora farei a raspagem dos dados encontrados no site. Será uma raspagem meio no braço,
# pois ainda estou conhecendo o pacote rvest.



# Vou usar o pacote “rvest”.
library(tidyverse)
library(rvest)
library(dplyr)
library(robotstxt)
library(xml2)
library(purrr)
library(lubridate)
library(tibble)

# Cada round tem um id diferente na url.
# id_round <- c(16,36,20,42,48,69,71)

# Cada phase, round (quartas, semi, final etc) é um url diferente.


func_etapas <- function(roundID, num_heat, athlete_per_heat, quart_semi_final,
                        ano, periodo,
                        lugar, evento, genero) {
  # Criando a coluna score.
  vju <- paste0('https://www.worldsurfleague.com/events/2008/mct/4/quiksilver-pro-gold-coast?roundId=',roundID)
  score <- read_html(vju)%>%
    html_nodes(".wave-wrap .score") %>%
    html_text() %>%
    str_squish()
  
  # Criando a coluna athlete.
  kol <- paste0('https://www.worldsurfleague.com/events/2008/mct/4/quiksilver-pro-gold-coast?roundId=',roundID)
  athlete <- read_html(kol)%>%
    html_nodes(".avatar--athlete") %>%
    html_text() %>%
    str_squish() %>% 
    rep(each = 15)
  
  # Criando a coluna heat.
  heat <- rep(c(1:num_heat), each = athlete_per_heat * 15)
  
  # Número de linhas do tibble.
  num_lin <- length(athlete)
  
  # Coluna year
  year <- rep(ano, num_lin)
  
  # Coluna when (quando foi o campeonato)
  when <- rep(periodo, num_lin)
  
  # Local do campeonato.
  local <- rep(lugar,num_lin)
  
  # Cammpeonato masculino ou feminino (Mens, Womens)
  gender <- rep(genero, num_lin)
  
  # Etapa do campeonato.
  stop <- rep(1, num_lin)
  
  # Nome do campeonato.
  event <- rep(evento, num_lin)
  
  # Round significa quartas, semi, final etc.
  round <- rep(quart_semi_final, num_lin)
  
  # É o número da onda do competidor.
  wave <- rep(1:15, num_lin/15)
  
  # Criando o tibble e salvando.
  tabela <- tibble(year, when, local, gender, event, stop,
                   round, heat, athlete, wave, score) %>%
    # Excluindo as células de score vazias. 
    filter(score != "") 
  write_rds(tabela, "tabela.rds")
}

func_etapas(roundID = 16, num_heat = 16, athlete_per_heat = 3, 
            quart_semi_final = "Round 1", ano = 2008,
            periodo = "Feb 23 - May 5, 2008", 
            lugar = "Gold Coast, Queensland, Australia",
            evento = "Quiksilver Pro Gold Coast", genero = "Men's")

# Esta função gera um DF para cada round. Depois é preciso juntar todos.

files <- list.files(path = "C:/Users/marru/OneDrive/Documentos/R projetos no Acer Aspire 3/world_surf_league_analise/mens_2008_quiksilver_pro_gold_coast",
                    pattern = "\\.rds$", full.names = TRUE)
mens_2008_quiksilver_pro_gold_coast <- do.call("rbind", lapply(files, readRDS))

# mergedat <- do.call('rbind', lapply(list.files("dat/", full.names = TRUE), readRDS))
# 
# library(raster)
# files <- list.files(path = path, pattern = "\\.rds$", full.names = TRUE)
# r <- lapply(files, readRDS)
# s <- stack(r)

getwd()