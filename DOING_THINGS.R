first_code = 1
library('swirl')

library(tidyverse)
library(tibble)
library(readr)



papers <- as_tibble(read_csv("CitesforSara.csv"))
papers_selected <-  select(papers, journal, year, cites, title, au1)

soma_eco <- papers %>%
  filter(journal == "Econometrica") %>%
  group_by(year) %>%
  summarise(soma_citacoes = sum(cites, na.rm = TRUE))

sum(soma_eco$soma_citacoes)

numero_autores_distintos <- papers %>%
  summarise(total_autores_distintos = n_distinct(au1, na.rm = TRUE))

print(numero_autores_distintos$total_autores_distintos)
