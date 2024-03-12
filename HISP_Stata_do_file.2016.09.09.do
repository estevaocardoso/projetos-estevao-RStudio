# Clear memory and environment
rm(list = ls())

# Install required packages (if not already installed)
# These packages may need to be installed from external sources
# uncomment the following lines to install them
# install.packages("psmatch2")  # For matching methods
# install.packages("st0366")   # Might be needed for some commands
# install.packages("sg65")     # Might be needed for some commands
# install.packages("sxd4")     # Might be needed for some commands

# Set working directory (replace with your actual path)
setwd("C:/My Documents/HISP")  # Modify this path to your data folder

# Open log file for recording outputs
sink("solution_log.txt")

# Open the cleaned data set
data <- read.csv("evaluation.dta", header = TRUE)  # Assuming data is in CSV format

# Define explanatory variables for regressions
controls <- c("age_hh", "age_sp", "educ_hh", "educ_sp", "female_hh", 
               "indigenous", "hhsize", "dirtfloor", "bathroom", "land", 
               "hospital_distance")

# ----------------------------------------------------
# Method 1: No Design - Before and After (Chapter 3)

# Select enrolled households in treatment villages at follow-up
data <- subset(data, treatment_locality == 1 & enrolled == 1)

# Table 3.1: Before-After Comparison of Means (t-test)
t.test(health_expenditures ~ round, data = data, paired = TRUE)

# Table 3.2: Before-After Regression Analysis (Linear Regression)
lm1 <- lm(health_expenditures ~ round, data = data)
summary(lm1)

# Table 3.2: Before-After Regression Analysis (Multivariate Regression)
lm2 <- lm(health_expenditures ~ round + controls, data = data)
summary(lm2)

# ----------------------------------------------------
# Method 2: No Design - Enrolled-Nonenrolled (Chapter 3)

# Select households in treatment villages at follow-up
data <- subset(data, treatment_locality == 1 & round == 1)

# Table 3.3: Enrolled-Nonenrolled Comparison of Means (t-test)
t.test(health_expenditures ~ enrolled, data = data)

# Table 3.4: Enrolled-Nonenrolled Regression Analysis (Linear Regression)
lm3 <- lm(health_expenditures ~ enrolled, data = data)
summary(lm3)

# Table 3.4: Enrolled-Nonenrolled Regression Analysis (Multivariate Regression)
lm4 <- lm(health_expenditures ~ enrolled + controls, data = data)
summary(lm4)

# ----------------------------------------------------
# Method 3: Randomized Assignment (Chapter 4)

# Select eligible households
data <- subset(data, eligible == 1)

# Table 4.1: Baseline Balance Between Treatment and Comparison Villages (Health Expenditures)
t.test(health_expenditures ~ treatment_locality, data = subset(data, round == 0))

# Table 4.1: Baseline Balance Between Treatment and Comparison Villages (Controls)
for (var in controls) {
  cat("**", var, "**\n")
  summary(data[var] ~ treatment_locality)
  t.test(data[var] ~ treatment_locality, data = subset(data, round == 0))
}

# Table 4.2: Treatment Impact at Baseline (t-test)
t.test(health_expenditures ~ treatment_locality, data = subset(data, round == 0))

# Table 4.2: Treatment Impact at Follow-up (t-test)
t.test(health_expenditures ~ treatment_locality, data = subset(data, round == 1))

# Table 4.3: Treatment Impact Regression Analysis (Linear Regression)
lm5 <- lm(health_expenditures ~ treatment_locality, data = subset(data, round == 1))
summary(lm5)

# Table 4.3: Treatment Impact Regression Analysis (Multivariate Regression)
lm6 <- lm(health_expenditures ~ treatment_locality + controls, data = subset(data, round == 1))
summary(lm6)

# ----------------------------------------------------
# Method 4: Instrumental Variables (Chapter 5)

# Not applicable in R (code assumes full compliance)

# ----------------------------------------------------
# Method 5: R


