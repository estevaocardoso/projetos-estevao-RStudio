library(foreign)  
library(lmtest)  
library(sandwich)

#Tabelas 1 e 2

results <- 'C:\\Users\\estev\\OneDrive\\Desktop\\projetos-estevao-RStudio\\Projeto artigo henrique'

BLCHAR <- c("yield_bl", "pesticide_cost_bl", "yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "opland", "wealthV2")

aggregate_data <- aggregate(Balance_Attrition[, BLCHAR], by = list(Balance_Attrition$Treatment), FUN = function(x) c(mean(x), sd(x), length(x)))
aggregate_data
write.csv(aggregate_data, file = file.path(results, "Tablettests.csv"))



model1 <- lm(Treatment ~ yearsedu_brinjalgrwer + age_brinjalgrwer + years_farmer + opland + wealthV2 + yield_bl + pesticide_cost_bl, data = Balance_Attrition)
coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
model2 <- lm(attrition ~ Treatment + yearsedu_brinjalgrwer + age_brinjalgrwer + years_farmer + opland + wealthV2 + yield_bl + pesticide_cost_bl, data = Balance_Attrition)
coeftest(model2, vcov = vcovHC(model2, type = "HC1"))
write.csv(summary(model1)$coefficients, file = file.path(results, "Table2.csv")) ss








