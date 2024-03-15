#questão 1

library(MASS)  # Para ajustar o modelo probit

# Passo 1: Ajustar o modelo probit
probit_model <- glm(enrolled ~ age_hh + educ_hh + educ_sp + poverty_index + female_hh + indigenous + dirtfloor + hospital_distance,
                    data = evaluation, family = binomial(link = "probit"))


evaluation$pscore <- predict(probit_model, type = "response")


den_tratado <- density(evaluation$pscore[evaluation$enrolled == 1])
den_controle <- density(evaluation$pscore[evaluation$enrolled == 0])

# Passo 4: Plotar as densidades
plot(den_controle, col = "blue", main = "Densidade das Pontuações de Propensão",
     xlab = "Pontuação de Propensão", ylab = "Densidade", xlim = c(0, 1))
lines(den_tratado, col = "red")
legend("topright", legend = c("Controle", "Tratado"), col = c("blue", "red"), lty = 1)

#questão 4 

set.seed(100)
u <- runif(nrow(evaluation))
sorted_indices <- order(u)
evaluation <- evaluation[sorted_indices, ]

#questão 5

library(MatchIt)

data <- evaluation

enrolled <- data$enrolled
health_expenditures <- data$health_expenditures

# Definir covariáveis de baseline (substitua por suas covariáveis reais)
baseline_covariates <- c("age_hh0", "educ_hh0", "educ_sp0", "poverty_index",
                         "female_hh", "indigenous", "dirtfloor", "hospital_distance")

# Gerar pscore
propensity_score_model <- lm(enrolled ~ baseline_covariates, data = data)
data$pscore <- predict(propensity_score_model, type = "response")

# Definir seed para reprodutibilidade (opcional)
set.seed(123)

# Matching usando nearest neighbor matching (substitua por outros métodos se desejar)
matched_data <- MatchIt(enrolled ~ pscore, data = data, method = "nearest")

# Analisar efeito do tratamento (substitua pelo seu estimador desejado)
treatment_effect <- mean(matched_data$enrolled * (matched_data$health_expenditures1 - matched_data$health_expenditures0))

cat("Efeito estimado do tratamento nas despesas de saúde:", treatment_effect)