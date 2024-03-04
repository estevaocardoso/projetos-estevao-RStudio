install.packages("rdd")
library(rdd)
indiv_final$same_party <- ifelse(indiv_final$difshare > 0, 1, 0)

proportion_same_party <- mean(indiv_final$same_party, na.rm = TRUE)
proportion_same_party <- round(proportion_same_party, 2)

rdd_object <- rdd(Y = indiv_final$myoutcomenext, X = indiv_final$difshare, cutpoint = 0)

density_result <- DCdensity(rdd_object)

library(ggplot2)

# Assuming difshare is your running variable
running_variable <- indiv_final$difshare

# Plot density around the cutoff
ggplot() +
  geom_density(aes(x = running_variable), fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Density Plot of Running Variable",
       x = "Running Variable",
       y = "Density")


reg_below <- lm(log(heights_variable) ~ difshare, data = indiv_final[indiv_final$difshare <= 0, ])
reg_above <- lm(log(heights_variable) ~ difshare, data = indiv_final[indiv_final$difshare > 0, ])
