#questão 1
rm(list = ls())
library(dplyr)
data <- evaluation
treatment_data <- evaluation %>% filter(treatment_locality == 1)

t_test_result <- t.test(
  health_expenditures ~ enrolled,
  data = treatment_data,
  subset = round == 1
)

print(t_test_result)

#questão 2

rounds_01 <- evaluation %>% filter(round %in% c(0, 1))


diff_not_enrolled <- with(subset(rounds_01, enrolled == 0),
                          mean(health_expenditures[round == 1]) - mean(health_expenditures[round == 0]))

diff_enrolled <- with(subset(rounds_01, enrolled == 1),
                      mean(health_expenditures[round == 1]) - mean(health_expenditures[round == 0]))

diff_in_diff <- diff_enrolled - diff_not_enrolled

t_test_not_enrolled <- t.test(health_expenditures ~ round, data = subset(rounds_01, enrolled == 0))
t_test_enrolled <- t.test(health_expenditures ~ round, data = subset(rounds_01, enrolled == 1))


cat("Difference for not enrolled:", diff_not_enrolled, "\n")
cat("Difference for enrolled:", diff_enrolled, "\n")


print(t_test_not_enrolled)
print(t_test_enrolled)


#quarta questão
library(sandwich)
library(lmtest)


rounds_01$interaction_enrolled_round <- rounds_01$enrolled * rounds_01$round


did_model <- lm(health_expenditures ~ enrolled + round + interaction_enrolled_round, data = rounds_01)




did_model_clustered <- coeftest(did_model, vcov. = vcovHC(did_model, cluster = "locality"))


summary(did_model)
