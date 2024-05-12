library(foreign)  
library(lmtest)  
library(sandwich)

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

