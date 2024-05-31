library(foreign)  
library(lmtest)  
library(sandwich)
library(broom)
library(multiwayvcov)
library(stargazer)
library(fixest)
library(multcomp)
library(dplyr)
library(haven)


#Table 1 and 2
if(TRUE){

results <- 'C:\\Users\\estev\\OneDrive\\Desktop\\projetos-estevao-RStudio\\Projeto artigo henrique'

BLCHAR <- c("yield_bl", "pesticide_cost_bl", "yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "opland", "wealthV2")

aggregate_data <- aggregate(Balance_Attrition[, BLCHAR], by = list(Balance_Attrition$Treatment), FUN = function(x) c(mean(x), sd(x), length(x)))
aggregate_data
write.csv(aggregate_data, file = file.path(results, "Tablettests.csv"))



model1 <- lm(Treatment ~ yearsedu_brinjalgrwer + age_brinjalgrwer + years_farmer + opland + wealthV2 + yield_bl + pesticide_cost_bl, data = Balance_Attrition)
coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
model2 <- lm(attrition ~ Treatment + yearsedu_brinjalgrwer + age_brinjalgrwer + years_farmer + opland + wealthV2 + yield_bl + pesticide_cost_bl, data = Balance_Attrition)
coeftest(model2, vcov = vcovHC(model2, type = "HC1"))
write.csv(summary(model1)$coefficients, file = file.path(results, "Table2.csv"))
}

#Table 3
if(TRUE){
data <- impact_compiled_data_yield_AJAE
data2 <- Balance_Attrition
hhid_both <- intersect(data$hhid, data2$hhid)
data <- data[data$hhid %in% hhid_both, ]
data2 <- data2[data2$hhid %in% hhid_both, ]
HHCHAR1 <- data2[, c("yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "wealthV2", "opland")]
data <- cbind(data, HHCHAR1)
HHCHAR1 <- c("yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "wealthV2", "opland")

model1 <- lm(net_yieldHA ~ Treatment + net_yieldHA_b, data = data)
coeftest(model1, vcov. = vcovHC(model1, cluster = ~village))

formula2 <- paste("net_yieldHA ~ Treatment + net_yieldHA_b +", paste(HHCHAR1, collapse = " + "), collapse = " ")
model2 <- lm(formula, data = data)
coeftest(model2, vcov. = vcovHC(model2, cluster = ~village))

formula3 <- paste("net_yieldHA_win ~ Treatment + net_yieldHA_b +", paste(HHCHAR1, collapse = " + "), collapse = " ")
model3 <- lm(formula3, data = data)
coeftest(model3, vcov. = vcovHC(model3, cluster = ~village))

formula4 <- paste("lnet_yieldHA ~ Treatment + lnet_yieldHA_b +", paste(HHCHAR1, collapse = " + "), collapse = " ")
model4 <- lm(formula4, data = data)
coeftest(model4, vcov. = vcovHC(model4, cluster = ~village))

formula5 <- paste("net_yieldHA_IHS ~ Treatment + net_yieldHA_IHS_b +", paste(HHCHAR1, collapse = " + "), collapse = " ")
model5 <- lm(formula5, data = data)
coeftest(model5, vcov. = vcovHC(model5, cluster = ~village))


data3 <- Pesticides
HHCHAR2 <- c("hhid", "yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "wealthV2.x", "opland")
data_subset <- data[HHCHAR2]
data3 <- merge(data_subset, data3, by = "hhid")


formula1 <- "pesticide_cost_el ~ Treatment + pesticide_cost_bl"
model1 <- lm(formula1, data = data3)
coeftest(model1, vcov. = vcovHC(model1, cluster = ~village))


formula2 <- paste("pesticide_cost_el ~ Treatment + pesticide_cost_bl +", paste(HHCHAR2[-1], collapse = " + "))
model2 <- lm(formula2, data = data3)
coeftest(model2, vcov. = vcovHC(model2, cluster = ~village))


formula3 <- paste("pesticide_cost_el_win ~ Treatment + pesticide_cost_bl_win +", paste(HHCHAR2[-1], collapse = " + "))
model3 <- lm(formula3, data = data3)
coeftest(model3, vcov. = vcovHC(model3, cluster = ~village))

formula4 <- paste("pesticide_lcost_el ~ Treatment + pesticide_lcost_bl +", paste(HHCHAR2[-1], collapse = " + "))
model4 <- lm(formula4, data = data3)
coeftest(model4, vcov. = vcovHC(model4, cluster = ~village))


formula5 <- paste("pesticide_cost_el_ihs ~ Treatment + pesticide_cost_bl_ihs +", paste(HHCHAR2[-1], collapse = " + "))
model5 <- lm(formula5, data = data3)
coeftest(model5, vcov. = vcovHC(model5, cluster = ~village))
}

#Table 4
if(TRUE){
selected_columns <- impact_compiled_data_yield_AJAE[, c("brinjal_plot_areaHA_e" ,"retained", "paid_out", "lost", "harvested", "hhid", "harvested_b", "lost_b", "paid_out_b", "retained_b", "brinjalarea_hectare")]
data3 <- merge(data3, selected_columns, by = "hhid", all.x = TRUE)

clustered_lm <- function(formula, data, cluster) {
  model <- lm(formula, data = data)
  vcov_cluster <- cluster.vcov(model, cluster)
  coeftest(model, vcov_cluster)
}


HHCHAR1 <- c("yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "wealthV1", "opland")
controls <- c("yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "wealthV1", "opland")
formula1 <- as.formula(paste("harvested ~ Treatment + harvested_b +", paste(controls, collapse = " + ")))
formula2 <- as.formula(paste("lost ~ Treatment + lost_b +", paste(controls, collapse = " + ")))
formula3 <- as.formula(paste("paid_out ~ Treatment + paid_out_b +", paste(controls, collapse = " + ")))
formula4 <- as.formula(paste("retained ~ Treatment + retained_b +", paste(controls, collapse = " + ")))
formula5 <- as.formula(paste("brinjal_plot_areaHA_e ~ Treatment + brinjalarea_hectare +", paste(controls, collapse = " + ")))
BT1 <- clustered_lm(formula1, data = impact_compiled_data_yield_AJAE, cluster = impact_compiled_data_yield_AJAE$village)
BT2 <- clustered_lm(formula2, data = impact_compiled_data_yield_AJAE, cluster = impact_compiled_data_yield_AJAE$village)
BT3 <- clustered_lm(formula3, data = impact_compiled_data_yield_AJAE, cluster = impact_compiled_data_yield_AJAE$village)
BT4 <- clustered_lm(formula4, data = impact_compiled_data_yield_AJAE, cluster = impact_compiled_data_yield_AJAE$village)
BT5 <- clustered_lm(formula5, data = impact_compiled_data_yield_AJAE, cluster = impact_compiled_data_yield_AJAE$village)
stargazer(BT1, BT2, BT3, BT4, type = "text", digits = 2, out = "yield_mechanisms_July2020.txt")
stargazer(BT5, type = "text", digits = 4, out = "yield_mechanisms_July2020.txt", append = TRUE)
stargazer(BT1, BT2, BT3, BT4, BT5, type = "latex", digits = 2)
}

#Table 5
if(TRUE){

formula1 <- as.formula(paste("sold ~ Treatment + sold_baseline +", paste(controls, collapse = " + ")))
formula2 <- as.formula(paste("unit_price ~ Treatment + unit_price_baseline +", paste(controls, collapse = " + ")))
formula3 <- as.formula(paste("revenue ~ Treatment + revenue_baseline +", paste(controls, collapse = " + ")))
formula4 <- as.formula(paste("cashcost ~ Treatment + cashcost_baseline +", paste(controls, collapse = " + ")))
formula5 <- as.formula(paste("net_rev ~ Treatment + net_rev_baseline +", paste(controls, collapse = " + ")))
Mech1 <- clustered_lm(formula1, data = Mechanisms_Table_5_AJAE, cluster = Mechanisms_Table_5_AJAE$village)
Mech2 <- clustered_lm(formula2, data = Mechanisms_Table_5_AJAE, cluster = Mechanisms_Table_5_AJAE$village)
Mech3 <- clustered_lm(formula3, data = Mechanisms_Table_5_AJAE, cluster = Mechanisms_Table_5_AJAE$village)
Mech4 <- clustered_lm(formula4, data = Mechanisms_Table_5_AJAE, cluster = Mechanisms_Table_5_AJAE$village)
Mech5 <- clustered_lm(formula5, data = Mechanisms_Table_5_AJAE, cluster = Mechanisms_Table_5_AJAE$village)
stargazer(Mech1, Mech2, Mech3, Mech4, type = "text", digits = 2, out = "Mechanisms_Table5_July2020.txt")
stargazer(Mech5, type = "text", digits = 2, out = "Mechanisms_Table5_July2020.txt", append = TRUE)
stargazer(Mech1, Mech2, Mech3, Mech4, Mech5, type = "latex", digits = 2)
}

#Table 6
if(true){

controls <- c("yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "wealthV1", "opland")
formula1 <- as.formula(paste("pesticide_sprays_el ~ Treatment + pesticide_sprays_bl +", paste(controls, collapse = " + ")))
formula2 <- as.formula(paste("pesticide_quantity_el ~ Treatment + pesticide_quantity_bl +", paste(controls, collapse = " + ")))
formula3 <- as.formula(paste("feiq_el ~ Treatment + feiq_bl +", paste(controls, collapse = " + ")))
formula4 <- as.formula(paste("ceiq_el ~ Treatment + ceiq_bl +", paste(controls, collapse = " + ")))
formula5 <- as.formula(paste("weiq_el ~ Treatment + weiq_bl +", paste(controls, collapse = " + ")))
formula6 <- as.formula(paste("eceiq_el ~ Treatment + eceiq_bl +", paste(controls, collapse = " + ")))
formula7 <- as.formula(paste("puts_el ~ Treatment + puts_bl +", paste(controls, collapse = " + ")))
Col_1 <- clustered_lm(formula1, data = Pesticides, cluster = Pesticides$village)
Col_2 <- clustered_lm(formula2, data = Pesticides, cluster = Pesticides$village)
Col_3 <- clustered_lm(formula3, data = Pesticides, cluster = Pesticides$village)
Col_4 <- clustered_lm(formula4, data = Pesticides, cluster = Pesticides$village)
Col_5 <- clustered_lm(formula5, data = Pesticides, cluster = Pesticides$village)
Col_6 <- clustered_lm(formula6, data = Pesticides, cluster = Pesticides$village)
Col_7 <- clustered_lm(formula7, data = Pesticides, cluster = Pesticides$village)
stargazer(Col_1, Col_2, Col_3, Col_4, Col_5, Col_6, Col_7, type = "text", digits = 3, out = "Table6.txt")
stargazer(Col_1, Col_2, Col_3, Col_4, Col_5, Col_6, Col_7, type = "latex", digits = 3)
Pesticides <- Pesticides %>%
  rename(
    pesticide_sprays = pesticide_sprays_el,
    pesticide_quantity = pesticide_quantity_el,
    feiq = feiq_el,
    ceiq = ceiq_el,
    weiq = weiq_el,
    eceiq = eceiq_el,
    puts = puts_el
  )
}

#Table 7a
if(TRUE){
Illness <- Illness %>%
  mutate(across(everything(), ~ ifelse(labelled::is.labelled(.), haven::as_factor(.), .)))

PERCHAR1 <- c("age", "sex", "relHead_2", "relHead_3", "relHead_4")
HHCHAR1 <- c("yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "wealthV1", "opland")

formula1 <- as.formula(paste("anyIllness_End ~ Treatment + anyIllness_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
formula2 <- as.formula(paste("numIllness_End ~ Treatment + numIllness_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
formula3 <- as.formula(paste("anydays_lost_End ~ Treatment + anydays_lost_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
formula4 <- as.formula(paste("seek_treat_End ~ Treatment + seek_treat_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
formula5 <- as.formula(paste("any_medexp_End ~ Treatment + any_medexp_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))

# Estimar os modelos de regressão com erros padrão clusterizados
model1 <- feols(formula1, data = Illness, cluster = ~village)
model2 <- feols(formula2, data = Illness, cluster = ~village)
model3 <- feols(formula3, data = Illness, cluster = ~village)
model4 <- feols(formula4, data = Illness, cluster = ~village)
model5 <- feols(formula5, data = Illness, cluster = ~village)
etable(model1, model2, model3, model4, model5, tex = TRUE, file = "model_results.tex")
}

#Table 7b
if(TRUE){
  Illness_filtered <- Illness %>%
    filter(c1_03 == 1 | c1_04 == 1)  # Either persistent respiratory or skin problems

  PERCHAR1 <- c("age", "sex", "relHead_2", "relHead_3", "relHead_4")
  HHCHAR1 <- c("yearsedu_brinjalgrwer", "age_brinjalgrwer", "years_farmer", "wealthV1", "opland")
 
  formula1 <- as.formula(paste("anyIllness_End ~ Treatment + anyIllness_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
  formula2 <- as.formula(paste("numIllness_End ~ Treatment + numIllness_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
  formula3 <- as.formula(paste("anydays_lost_End ~ Treatment + anydays_lost_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
  formula4 <- as.formula(paste("seek_treat_End ~ Treatment + seek_treat_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
  formula5 <- as.formula(paste("any_medexp_End ~ Treatment + any_medexp_Base +", paste(PERCHAR1, collapse = " + "), "+", paste(HHCHAR1, collapse = " + ")))
  
  model1 <- feols(formula1, data = Illness_filtered, cluster = ~village)
  model2 <- feols(formula2, data = Illness_filtered, cluster = ~village)
  model3 <- feols(formula3, data = Illness_filtered, cluster = ~village)
  model4 <- feols(formula4, data = Illness_filtered, cluster = ~village)
  model5 <- feols(formula5, data = Illness_filtered, cluster = ~village)
  
  models <- list(model1, model2, model3, model4, model5)
  etable(models, tex = TRUE, file = "model_resultstable7b.tex")
}
