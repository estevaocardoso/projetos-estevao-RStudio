# Preliminaries
#-------------------------------------------------
#install.packages('perm')
library(perm)
rm(list = ls())
setwd("")

# Questions 1 - 4
#-------------------------------------------------
perms <- chooseMatrix(8,4)
A <- matrix(c(0.462, 0.731, 0.571, 0.923, 0.333, 0.750, 0.893, 0.692), nrow=8, ncol=1, byrow=TRUE)
treatment_avg <- (1/4)*perms%*%A
control_avg <- (1/4)*(1-perms)%*%A
test_statistic <- abs(treatment_avg-control_avg)
test_statistic
rownumber <- apply(apply(perms, 1, 
                         function(x) (x == c(0, 1, 0, 0, 0, 1, 1, 1))), 
                   2, sum)
rownumber <- (rownumber == 8)
observed_test <- test_statistic[rownumber == TRUE]

larger_than_observed <- (test_statistic >= observed_test)
#numbers in which the statistic exceeds or is equal to the value in the observed date
sum(larger_than_observed)
mean_observed_test <- mean(observed_test)
fisher_exact_p_value <- 2 * sum(larger_than_observed) / length(larger_than_observed)
fisher_exact_p_value <- round(fisher_exact_p_value, 2)
fish


df <- data.frame(perms,control_avg,treatment_avg,test_statistic)

# Question 5 - 6
#-------------------------------------------------
simul_stat <- as.vector(NULL)
schools <- read.csv('teachers_final.csv')
set.seed(1001)
for(i in 1:100) {
  print(i)
  schools$rand <- runif(100,min=0,max=1)
  schools$treatment_rand <- as.numeric(rank(schools$rand)<=49)
  schools$control_rand = 1-schools$treatment_rand
  simul_stat <-append(simul_stat,
            sum(schools$treatment_rand*schools$open)/sum(schools$treatment_rand) 
            - sum(schools$control_rand*schools$open)/sum(schools$control_rand))
}

schools$control = 1-schools$treatment
actual_stat <- sum(schools$treatment*schools$open)/sum(schools$treatment) - sum(schools$control*schools$open)/sum(schools$control)

sum(abs(simul_stat) >= abs(actual_stat)) / length(simul_stat)

#Question 7 - 

treatment_avg <- mean(schools[schools$treatment == 1, ]$open)
control_avg <- mean(schools[schools$treatment == 0, ]$open)
ate <- treatment_avg - control_avg

# Print the ATE rounded to three decimal places
treatment_sd <- sd(schools[schools$treatment == 1, ]$open)
control_sd <- sd(schools[schools$treatment == 0, ]$open)
neyman_var <- (treatment_sd^2 / sum(schools$treatment)) + (control_sd^2 / sum(schools$control))

# Compute the upper bound of the standard error
upper_bound_se <- sqrt(neyman_var)

# Print the upper bound rounded to three decimal places
print(round(upper_bound_se, 3))
#---------------------------------------------------
#Printing the ATE
ate <- sum(schools$treatment * schools$open) / sum(schools$treatment) - sum(schools$control * schools$open) / sum(schools$control)
ate

control_mean <- sum(schools$control*schools$open)/sum(schools$control)
treatment_mean <- sum(schools$treatment*schools$open)/sum(schools$treatment)

s_c <- (1/(sum(schools$control)-1))*sum(((schools$open-control_mean)*schools$control)^2)
s_t <- (1/(sum(schools$treatment)-1))*sum(((schools$open-treatment_mean)*schools$treatment)^2)

Vneyman <- (s_c / sum(schools$control) + s_t / sum(schools$treatment))
print(sqrt(Vneyman))
print(actual_stat/sqrt(Vneyman))

print(actual_stat-1.96*sqrt(Vneyman))
print(actual_stat+1.96*sqrt(Vneyman))

#Question 15
#---------------------------------------------------
attach(schools)
library(np)
# Assuming you have already attached the dataset with attach(schools)

# Plot for bandwidth = 20
plot_bandwidth_20 <- npreg(xdat = schools$open, ydat = schools$pctpostwritten, bws = 20, bandwidth.compute = FALSE)
plot(plot_bandwidth_20)

# Plot for bandwidth = 0.001
plot_bandwidth_0.001 <- npreg(xdat = schools$open, ydat = schools$pctpostwritten, bws = 0.001, bandwidth.compute = FALSE)
plot(plot_bandwidth_0.001)

# Plot for bandwidth = 1
plot_bandwidth_1 <- npreg(xdat = schools$open, ydat = schools$pctpostwritten, bws = 1, bandwidth.compute = FALSE)
plot(plot_bandwidth_1)

# Plot for bandwidth = 0.04
plot_bandwidth_0.04 <- npreg(xdat = schools$open, ydat = schools$pctpostwritten, bws = 0.04, bandwidth.compute = FALSE)
plot(plot_bandwidth_0.04)
library(ggplot2)

# Assuming 'schools' is your data frame
ggplot(schools, aes(x = open, color = factor(treatment))) +
  stat_ecdf(geom = "step") +
  labs(title = "Empirical Cumulative Distribution Functions",
       x = "Fraction of Days Open",
       y = "Cumulative Probability",
       color = "Group") +
  theme_minimal()

