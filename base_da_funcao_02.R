# Em alguns campeonatos ou etapas é quebrado algum tipo de regularidade,
# o que ferra com a func_etapas.
# Para estes casos temos que fazer no passo a passo, individualmente.

# Criando a coluna score.
vju <- 'https://www.worldsurfleague.com/events/2019/mct/2924/meo-rip-curl-pro-portugal?roundId=12939'
score <- read_html(vju)%>%
  html_nodes(".wave-wrap .score") %>%
  html_text() %>%
  str_squish()

# Criando a coluna athlete.
tutu <- read_html(vju)%>%
  html_nodes(".avatar--athlete") %>%
  html_text() %>%
  str_squish() 
  
  
 athlete <- c(rep(tutu[1:18], each = 15),
              rep(tutu[19], each =16),
              rep(tutu[20:24], each =15),
              rep(tutu[25], each = 16),
              rep(tutu[26:32], each = 15))

              
# Criando a coluna heat.
heat <- c(rep(1:9, each = 2*15), 
          rep(10, each = 16+15),
          rep(11:12, each = 2*15),
          rep(13, each = 16+15),
          rep(14:16, each = 2*15))


# Número de linhas do tibble.
num_lin <- length(score)

# Round significa quartas, semi, final etc.
round <- rep("Round of 32", num_lin)

# É o número da onda do competidor, depois farei uma soma acumulada.
wave <- rep(1, num_lin)

id <- 201910

# Criando o tibble.
gil <- tibble(id, round, heat, athlete, wave, score) %>%
  # Excluindo as células de score vazias.
  filter(score != "") %>% 
  group_by(athlete) %>% 
  mutate(wave = cumsum(wave))

write_rds(mens_2016_05_round_3, "mens_2016_05_round_3.rds")
