#questão 1

install.packages("rdrobust")
library(rdrobust)
data <- evaluation

treatment_data <- subset(data, treatment_locality == 1 & round == 1)

running_variable <- "poverty_index"
outcome_variable <- "health_expenditures"

cutoff <- 58


# Criar um novo conjunto de dados para os pontos de dados antes do cutoff
before_cutoff <- subset(treatment_data, treatment_data[[running_variable]] < cutoff)

# Criar um novo conjunto de dados para os pontos de dados após o cutoff
after_cutoff <- subset(treatment_data, treatment_data[[running_variable]] >= cutoff)

# Criar gráfico RD plot usando ggplot2
ggplot(treatment_data, aes_string(x = running_variable, y = outcome_variable)) +
  geom_point() +
  geom_smooth(data = before_cutoff, method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = after_cutoff, method = "lm", se = FALSE, color = "red") +
  geom_vline(xintercept = cutoff, linetype = "dashed") +
  labs(x = "Running Variable", y = "Outcome Variable") +
  theme_minimal()



#questão 2

ggplot(treatment_data, aes(x = poverty_index)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "Poverty Index", y = "Density") +
  theme_minimal()

# não parece haver muito alguma descontinuidade na variável utilizando um gráfico
# de kernel.

#questão 3

treatment_data <- subset(data, treatment_locality == 1 & round == 1)

ggplot(treatment_data, aes(x = poverty_index, y = enrolled)) +
  geom_point() +
  geom_vline(xintercept = cutoff, linetype = "dashed") +
  labs(x = "Poverty Index", y = "Probability of Enrollment") +
  theme_minimal()
# será um Sharp RDD pois a esquerda do limite do cutoff as pessoas tem probabilidade 1 de serem
# inscrita no tratamento e 0 de não estar inscrita.

#questão 4

install.packages("rdrobust")
library(rdrobust)

outcome_variable <- "health_expenditures"
running_variable <- "poverty_index"
cutoff <- 58
treatment_area <- "treatment_locality"
round_variable <- "round"
treatment_data <- subset(data, treatment_locality == 1 & round == 1)
rd_model <- rdrobust(y = treatment_data[[outcome_variable]], 
                     x = treatment_data[[running_variable]], 
                     c = cutoff)
summary(rd_model)

#uquestão 5
rd_model_epanechnikov <- rdrobust(y = treatment_data[[outcome_variable]], 
                                  x = treatment_data[[running_variable]], 
                                  c = cutoff,
                                  kernel = "epanechnikov",
                                  bwselect = "mserd")

summary(rd_model_epanechnikov)



