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

# Criando um conjunto fictício de dados (altura de indivíduos)
altura <- c(160, 165, 170, 172, 175, 178, 180, 182, 185, 188)

# Carregando a biblioteca necessária para visualização
library(ggplot2)

# Criando um histograma para visualizar a distribuição original
hist_plot <- ggplot(data.frame(altura), aes(x = altura)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightblue", color = "black") +
  labs(title = "Histograma da Altura",
       x = "Altura",
       y = "Densidade") +
  theme_minimal()

# Exibindo o histograma
print(hist_plot)
# Adicionando uma estimativa de densidade do kernel ao histograma
kde_plot <- hist_plot +
  geom_density(kernel = "gaussian", color = "red", size = 1) +
  labs(title = "Histograma com Estimativa de Densidade do Kernel")

# Exibindo o histograma com a estimativa de densidade do kernel
print(kde_plot)

