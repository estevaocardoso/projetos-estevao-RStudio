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


did_model <- lm(health_expenditures ~ enrolled + round +
                  interaction_enrolled_round + poverty_index + age_hh + age_sp +
                  educ_hh, educ_sp, data = rounds_01)




did_model_clustered <- coeftest(did_model, vcov. = vcovHC(did_model, cluster = "locality"))


summary(did_model)

#quinta questão

did_model_control <- lm(health_expenditures ~ enrolled + round +
                          interaction_enrolled_round + poverty_index + age_hh + age_sp +
                          educ_hh + educ_sp + female_hh + indigenous + dirtfloor + hospital_distance,
                        data = rounds_01)

summary(did_model_control)

#sexta questão

library(plm)

rounds_01$round <- as.factor(rounds_01$round)

panel_data <- pdata.frame(rounds_01, index = c("household_identifier", "round"))

did_fe_model <- plm(health_expenditures ~ enrolled + round + 
                      interaction_enrolled_round + poverty_index + age_hh + age_sp + 
                      educ_hh + educ_sp + female_hh + indigenous + dirtfloor + hospital_distance, 
                    data = panel_data, model = "within")

summary(did_fe_model)

#oitava questão

rounds_01$round <- as.factor(rounds_01$round)

panel_data <- pdata.frame(rounds_01, index = c("household_identifier", "round"))

did_model <- plm(health_expenditures ~ enrolled * round,
                 data = panel_data, model = "within")


summary(did_model)
