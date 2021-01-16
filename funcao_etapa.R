# O trabalho para fazer um site com analises dos resultados dos campeonatos da 
# World Surf League terá 3 etapas: a raspagem dos dados; a análise propriamente dita;
# e, a apresentação usando o Shiny num site.

# Agora farei a raspagem dos dados encontrados no site. Será uma raspagem meio no braço,
# pois ainda estou conhecendo o pacote rvest.

library(tidyverse)
library(rvest)
library(dplyr)
library(robotstxt)
library(xml2)
library(purrr)
library(lubridate)
library(tibble)

# A func_etapas usa uma série de informações para ao final obter um dataframe
# de uma etapa, por exemplo o quiksilver pro gold coast de homens de 2008 ou 
# oi rio pro masculino de 2017 etc.


func_etapas <- function(base_url, 
                        evento_url, 
                        roundID,
                        num_heat, 
                        athlete_per_heat,
                        quart_semi_final, 
                        ano, periodo, stop,
                        lugar, evento, genero) {
  
  # As partes da url são:
  # A base_url que vai do começo até  mct (men's CT)
  # "https://www.worldsurfleague.com/events/2008/mct".
  
  # O evento_url que vai do travessão até o sinal de igual.
  # "/48/boost-mobile-pro?roundId="
  # E o número do roundID (número na url que indica a parte da competição:
  # final, semifinal, quartas etc), é só um número.
  
  
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
    # Foi repetido 15 vezes porque cada atleta tem direito a 15 ondas.
    rep(each = 15)  
  
  # Criando a coluna heat. 
  # Cada round tem umaquantidade de baterias e cada bateria uma quantidade de surfista.
  heat <- rep(c(1:num_heat), each = athlete_per_heat * 15)
  
  # Número de linhas do tibble.
  # Este número irá ajudar a fazer as repetições dos itens que não mudam em cada etapa:
  # ano; local; genero; parada etc. 
   
  num_lin <- length(athlete)
  
  # Coluna year
  year <- rep(ano, num_lin)
  
  # Coluna when (quando foi o campeonato)
  when <- rep(periodo, num_lin)
  
  # Local do campeonato.
  local <- rep(lugar, num_lin)
  
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
  
  # Criando o tibble.
  tabela <- tibble(year, when, local, gender, event, stop,
                   round, heat, athlete, wave, score) %>%
    # Excluindo as células de score vazias.
    filter(score != "")
  
    }

# Se aplicarmos a função a um elemento de cada argumento obtemos uma tibble
# de cada round, mas cada etapa é composta em geral de 7 rounds até chegar 
# à final.
# Desse modo é conveniente passar uma lista com as informações de cada etapa,
# assim obteeremos um tibble daquela etapa inteira.

# Cada lista é composta pelos dados pertinentes.
# É importante notar que se for uma informação que não muda nas sete etapas
# basta ser escrita uma única vez, a funão faz a reciclagem.

a <- list('https://www.worldsurfleague.com/events/2008/mct')#url base
b <- list('/4/quiksilver-pro-gold-coast?roundId=')#url da etapa
c <- list(16,36,20,42,48,69,71)#roundID
d <- list(16,16,16,8,4,2,1)#número de baterias
e <- list(3,2,2,2,2,2,2)#nmúmero de atletas por bateria
f <- list('Round 1', 'Round 2', 'Round3', 'Round 4',
          'Quarterfinal', 'Semifinal', 'Final')#os nomes das baterias
g <- list(2008)#ano
h <- list("Feb 23 - May 5, 2008")#período que aconjteceu a etapa
i <- list(1)#número da etapa, em geral são 11 etapas por ano.
j <- list("Gold Coast, Queensland, Australia")#local
k <- list("Quiksilver Pro Gold Coast")#nome da etapa
l <- list("men's")#genero dos atletas

# Agora criando o tibble do men's ct de 2008 etapa quiksilver pro gold coast.
mens_2008_quiksilver_pro_gold_coast <- pmap(list(a,b,c,d,e,f,g,h,i,j,k,l),
                                            func_etapas) %>%
  #Até aqui temos uma list de tibbles, o bind_rows junta todas elas em uma só.
  bind_rows()

# Salvando o referido tibble em .rds.
write_rds(mens_2008_quiksilver_pro_gold_coast,
          "mens_2008_quiksilver_pro_gold_coast.rds")


# O resultado até aqui é uma tibble de uma etapa de um campeonato (men's CT)
# Depois será preciso reunir todas em um único .rds, para tal usa-se a função abaixo.
# Para definir a path usa-se o getwd().

files <- list.files(path = "C:/Users/marru/OneDrive/Documentos/R projetos no Acer Aspire 3/wsl_analise",
                    pattern = "\\.rds$", full.names = TRUE)
mens_2008 <- do.call("rbind", lapply(files, readRDS))



