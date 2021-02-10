###############################################################################
######## O objetivo aqui é começar a conhecer os dados coletados.
###############################################################################

library(tidyverse)
glimpse(mens_ct_2008_2019)

# Quantos surfistas diferentes participaram destes campeonatos?
n_distinct(mens_ct_2008_2019$athlete_name) #R. 301

# Criando uma coluna para identificar o ano e o número do ct.
# mens_ct_2008_2019 <- mens_ct_2008_2019 %>%
#   # Criando o número do ct.
#   mutate(numero_ct = readr::parse_number(as.character(event_info))) %>%
#   mutate(numero_ct = stringr::str_pad(numero_ct, width = 2,
#                                       side = 'left', pad = 0)) %>%
#   # Criando a coluna ano.
#   mutate(ano = stringr::str_trunc(event_schedule, width = 4,
#                                   side = 'left', ellipsis = "")) %>%
#   tidyr::unite(ano_ct, ano, numero_ct, sep = "", remove = FALSE)

# Extraindo o nome das praias.
# mens_ct_2008_2019 <- mens_ct_2008_2019 %>%
#   # Usei a função rm_between do pacote qdapRegex para extrair o que está 
#   # entre "|" e ",".
# dplyr::mutate(beach = qdapRegex::rm_between(event_info, "|", ",",
#                                             extract = TRUE))

# Retirando os NAs da coluna wave_score.
# mens_ct_2008_2019 <- mens_ct_2008_2019 %>% 
#   dplyr::filter(!is.na(wave_score))

# Salvando o tibble.
saveRDS(mens_ct_2008_2019, "mens_ct_2008_2019.rds")
writexl::write_xlsx(mens_ct_2008_2019, "mens_ct_2008_2019.xlsx")

# Em quantas praias ocorrem as competições? R. 22
dplyr::n_distinct(mens_ct_2008_2019$beach)
# Quais são estas praias?
unique(mens_ct_2008_2019$beach)

# Qual a média e mediana das notas dos athletes? R. 3,39 e 2,83.
mean(mens_ct_2008_2019$wave_score, na.rm = TRUE)
median(mens_ct_2008_2019$wave_score, na.rm = TRUE)

# Qual a mediana por beach?
# R. Pipeline é a menor de todas, isto é, é uma praia difícil de surfar.
# Por este raciocínio lower trestles é a mais fácil.
mediana_praia <- mens_ct_2008_2019 %>% 
  group_by(beach) %>% 
  summarise(median_score = median(wave_score, na.rm = TRUE))

# Quias os melhores resultados por praia? 
# R. Trestles, J-Bay, Golden Coast, Bells Beach as australianas.
# Pipeline é que tem os piores resultados.
top_mean_score_result_beach <- mens_ct_2008_2019 %>% 
  group_by(beach) %>% 
  summarise(mean_result = mean(athlete_total)) %>% 
  arrange(desc(mean_result))
  

# Qual a mediana por surfista? Quanto melhor o surfista maior a mediana de sua
# nota?
# R. Parece que não. Os com mais vitórias não têm as maiores medianas.
# Num certo sentido pode-se esperar isso pois contam-se as maiores notas e
# valores extremos não impactam muito a mediana. 
mediana_athlete <- mens_ct_2008_2019 %>% 
  group_by(athlete_name) %>% 
  summarise(median_score = median(wave_score, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(ranque = rank(-median_score))

# Qual a média das notas dos surfistas?
# R. A média das notas parece relacionar-se mmais com os resultados 
# das commpetições. Mas ainda assim, aparecem com maiores médias muitos
# surfistas desconhecidos antes dos grandes.
mean_athlete <- mens_ct_2008_2019 %>% 
  dplyr::group_by(athlete_name) %>% 
  dplyr::summarise(mean_score = mean(wave_score, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(ranque = rank(-mean_score))

# Olhando alguns números.
summary(mens_ct_2008_2019)

# Olhando as notas das praias.
library(ggplot2)
library(forcats)
library(dplyr)

# Existe correlação forte entre o número de ondas que o surfista pega por 
# bateria e seu resultado, isto é, a soma da duas maiores?
# R. Evidentemente o cara que só pega ummma onda terá resultados ruins, posto que
# este é a soma das 2 melhores ondas. Os que pegam 17 ondas ou mais não apresentam
# muitos bons resultados porque são poucos que faze isso.
# Em suma, o número de ondas surfada não parece correlacionar-se ao resultado.
# MAS... entre 4 e 9 ondas há um aumento do resultado, fora deste intervalo
# não dá para dizer muita coisa.

wave_number_x_result <- mens_ct_2008_2019 %>% 
  # Agrupando para sumarizar.
  group_by(ano_ct, athlete_name, round_name, heat_name) %>% 
  summarise(result = max(athlete_total),
            wave_number = max(wave_number))  
  
# Fazendo o gráfico.
  ggplot(wave_number_x_result, aes(x = wave_number, y = result)) +
  #geom_point(alpha = 0.1) +
  #geom_jitter() +
  geom_smooth(method = "lm", se = FALSE)+
  geom_smooth(method = "loess", se = FALSE)+
  scale_x_continuous(breaks = 1:20) +
  theme_bw()
  
# Para olhar melhor número de ondas X resultados.  
n_distinct(wave_number_x_result$wave_number)
unique(wave_number_x_result$wave_number)

# Vou fazer um perfil da frequência do número de ondas de cada athlete
# por praia. 

freq_numero_ondas <- mens_ct_2008_2019 %>% 
  group_by(ano_ct, beach, round_name, heat_name, athlete_name) %>% 
  summarise(numero_de_ondas = max(wave_number)) %>% 
  ungroup() %>% 
  select(c(5,2,6)) %>% 
  group_by(athlete_name, beach, numero_de_ondas) %>%  # Agrupando para contar número de casos.
  tally() %>% # Contando número de casos.
  mutate(freq_percentual = n/sum(n)) %>% # Calculando a frequência perrcentual por caso.
  select(-4) %>% 
  nest(num_ondas_e_freq = c(numero_de_ondas, freq_percentual))
  
# Agora vou criar um tibble que mostre os valores de todas as ondas 
# agrupadas por athlete e beach.

conjunto_wave_score <- mens_ct_2008_2019 %>%
  # Selecionando as colunas que interessam agora.
  select(athlete_name, beach, wave_score) %>% 
  nest(wave_score_list = wave_score)
  
  


# Quantos e quais surfistas chegaram às finais, semi e quartas?

# O rendimento do surfista aumenta conforme avança nas baterias?

# 