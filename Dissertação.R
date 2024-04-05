
merged_df_usa_excrate$ValorOperacao <- merged_df_usa_excrate$ValorOperacao / 1000000
plot(merged_df_usa_excrate$`Brazilian real   (BRL)`, merged_df_usa_excrate$ValorOperacao, 
     xlab = "Taxa de Câmbio (BRL)", ylab = "Valor da Operação (em milhões)", 
     main = "Gráfico de Dispersão com Valores em Milhões")
abline(model, col = "red")
summary(model)




correlation <- cor(merged_df_usa_excrate$`Brazilian real   (BRL)`, merged_df_usa_excrate$ValorOperacao)
plot(merged_df_usa_excrate$`Brazilian real   (BRL)`, merged_df_usa_excrate$ValorOperacao, 
     xlab = "Taxa de Câmbio (BRL)", ylab = "Valor da Operação (em milhões)", 
     main = paste("Correlação:", round(correlation, 2)))
abline(model, col = "red")
