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


# Cada phase, round (quartas, semi, final etc) é um url diferente.


func_etapas <- function(base_url, 
                        evento_url, 
                        roundID,
                        num_heat, 
                        athlete_per_heat,
                        quart_semi_final, 
                        ano, periodo, stop,
                        lugar, evento, genero) {
  
  # As partes da url são:
  # A base_url que vai do começo até a mct
  # "https://www.worldsurfleague.com/events/2008/mct"
  # O evento_url que vai do travessão até o sinal de igual
  # "/48/boost-mobile-pro?roundId="
  # E o número do roundID, é só um número.
  
  
  # Criando a coluna score.
  vju <- paste0(base_url, evento_url, roundID)
  score <- read_html(vju)%>%
    html_nodes(".wave-wrap .score") %>%
    html_text() %>%
    str_squish()
  
  # Criando a coluna athlete.
  kol <- paste0(base_url, evento_url, roundID)
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
  stop <- rep(stop, num_lin)
  
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
  
    }


a <- list('https://www.worldsurfleague.com/events/2008/mct')
b <- list('/4/quiksilver-pro-gold-coast?roundId=')
c <- list(16,36,20,42,48,69,71)
d <- list(16,16,16,8,4,2,1)
e <- list(3,2,2,2,2,2,2)
f <- list('Round 1', 'Round 2', 'Round3', 'Round 4',
          'Quarterfinal', 'Semifinal', 'Final')
g <- list(2008)
h <- list("Feb 23 - May 5, 2008")
i <- list(1)
j <- list("Gold Coast, Queensland, Australia")
k <- list("Quiksilver Pro Gold Coast")
l <- list("men's")

# Agora criando o df do men's ct de 2008 etapa quiksilver pro gold coast.
mens_2008_quiksilver_pro_gold_coast <- pmap(list(a,b,c,d,e,f,g,h,i,j,k,l),
                                            func_etapas) %>% 
  bind_rows()

# Salvando o referido df em .rds.
write_rds(mens_2008_quiksilver_pro_gold_coast,
          "mens_2008_quiksilver_pro_gold_coast.rds")


# Esta função gera um arquivo .rds cada etapa. Depois é preciso juntar todas
# de modo a reunir todas etapas de 2008.

files <- list.files(path = "C:/Users/marru/OneDrive/Documentos/R projetos no Acer Aspire 3/wsl_analise",
                    pattern = "\\.rds$", full.names = TRUE)
mens_2008 <- do.call("rbind", lapply(files, readRDS))



