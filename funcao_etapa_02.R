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
library(ralger)

########################################################################
########################################################################
# Vou mudar um pouco a concepção.
# Primeiro vou raspar round, heat, athlete, score e colocar uma coluna
# id. Terminada esta faze vou fazer um join com a tabela com as colunas
# event, location, tour_name, stop, year e id.
######################################################################## 
########################################################################

func_etapas <- function(base_url, 
                        evento_url, 
                        roundID,
                        num_heat, 
                        athlete_per_heat,
                        quart_semi_final,
                        id) {
  
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
  athlete <- read_html(vju)%>%
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
  
  # Round significa quartas, semi, final etc.
  round <- rep(quart_semi_final, num_lin)
  
  # É o número da onda do competidor.
  wave <- rep(1:15, num_lin/15)
  
  id <- id
  
  # Criando o tibble.
  tabela <- tibble(id, round, heat, athlete, wave, score) %>%
    # Excluindo as células de score vazias.
    filter(score != "") 
  
    }

# Se aplicarmos a função a um elemento de cada argumento obtemos uma tibble
# de cada round, mas cada etapa é composta em geral de 7 rounds até chegar 
# à final.
# Desse modo é conveniente passar uma lista com as informações de cada etapa,
# assim obteremos um tibble daquela etapa inteira.

# Cada lista é composta pelos dados pertinentes.
# É importante notar que se for uma informação que não muda nas sete etapas
# basta ser escrita uma única vez, a funão faz a reciclagem.

a <- list('https://www.worldsurfleague.com/events/2019/mct') #url base
b <- list('/3616/billabong-pipe-masters-presented-by-hydro-flask?roundId=16715') #url da etapa
# c <- weblink_scrap(link = "https://www.worldsurfleague.com/events/2021/mct/3616/billabong-pipe-masters-presented-by-hydro-flask?roundId=16715",
#                    contain = "roundId=",
#                    case_sensitive = FALSE) %>%
#   str_trunc(width = 5, side = 'left', ellipsis = "") %>%
#   unique() %>% as.numeric() %>% sort() %>% as.list()
c <- list(16715,16716,16717,16718,16719,16720,16721) #roundID
d <- list(12,4,16,8,4,2,1) #número de baterias
e <- list(3,3,2,2,2,2,2) #número de atletas por bateria
f <- list('Seeding Round', 'Elimination Round', 'Round of 32', 'Round of 16', 
          'Quarterfinal', 'Semifinal', 'Final') #os nomes das baterias
g <- list(202101) #id, que é ano seguido do stop.

# Agora criando o tibble do men's ct de 2008 etapa quiksilver pro gold coast.
gilberto <- pmap(list(a,b,c,d,e,f,g), func_etapas) %>%
  #Até aqui temos uma list de tibbles, o bind_rows junta todas elas em uma só.
  bind_rows()

mens_2021_01 <- bind_rows(gilberto, gil)

# Salvando em .rds.
  write_rds(mens_2019_11,
            "mens_2019_11.rds")

# # Juntando os .rds que tive que fazer separadamente.
#     mens_2016_04 <- mens_2016_04 %>% 
#       select(-athlete) %>% 
#       rename(athlete = atleta_02)
#     
# # Salvando em .rds.
#     write_rds(mens_2017_01,
#               "mens_2017_01.rds")

# O resultado até aqui é uma tibble de uma etapa de um campeonato (men's CT)
# Depois será preciso reunir todas em um único .rds, para tal usa-se a função abaixo.
# Para saber qual é a path usa-se o getwd().

files <- list.files(path = "C:/Users/marru/OneDrive/Documentos/R projetos no Acer Aspire 3/wsl_analise/mens_2019",
                    pattern = "\\.rds$", full.names = TRUE)

mens_2019 <- do.call("rbind", lapply(files, readRDS))

write_rds(mens_2019, "mens_2019.rds")
writexl::write_xlsx(mens_2019, "mens_2019.xlsx")

###################################################################################
###################################################################################
# Como foi no ano 2009 que mudei de estratégia, tive que alterar o 
# formato antigo para depois fazer o bind_rows com o novo.

novo_formato_2009 <- bind_rows(mens_2009_billabong_pipeline_masters,
                               mens_2009_rip_curl_search,
                               mens_2009_quiksilver_pro_france,
                               mens_2009_billabong_pro_mundaka)

formato_antigo_2009 <- bind_rows(mens_2009_quiksilver_pro_gold_coast,
                                 mens_2009_rip_curl_pro_bells_beach,
                                 mens_2009_hurley_pro_trestles, 
                                 mens_2009_hang_loose_santa_catarina_pro,
                                 mens_2009_billabong_pro_teahupoo,
                                 mens_2009_billabong_pro_jbay)

rrr <- formato_antigo_2009 %>% 
  mutate(stop = str_pad(stop, width = 2, side = 'left', pad = 0)) %>%
  # Unindo as colunas year e stop para formar a coluna id.
  unite("id", year, stop, sep = "") %>% 
  # Ordenando as colunas.
  select(id, round, heat, athlete, wave, score) %>% 
  # Transformando a coluna id em numétrica.
  mutate(id = as.numeric(id))

# Juntando tudo e salvando.
mens_2009 <- bind_rows(rrr, novo_formato_2009)

write_rds(mens_2009, "mens_2009.rds")
writexl::write_xlsx(mens_2009, "mens_2009.rds")
