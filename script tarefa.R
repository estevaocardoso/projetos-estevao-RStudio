# Clear memory and environment
rm(list = ls())

# Install required packages (if not already installed)
# These packages may need to be installed from external sources
# uncomment the following lines to install them
install.packages("psmatch2")  # For matching methods
install.packages("st0366")   # Might be needed for some commands
install.packages("sg65")     # Might be needed for some commands
install.packages("sxd4")     # Might be needed for some commands



#First Question

# Open the cleaned data set
data <- evaluation  # Assuming data is in CSV format

# Define explanatory variables for regressions
controls <- c("age_hh", "age_sp", "educ_hh", "educ_sp", "female_hh", 
              "indigenous", "hhsize", "dirtfloor", "bathroom", "land", 
              "hospital_distance")


# Select enrolled households in treatment villages at follow-up
eligible_baseline_data <- subset(evaluation, eligible == 1 & round == 0)
variables_of_interest <- c("health_expenditures", "poverty_index", "age_hh", 
                           "educ_hh", "educ_sp", "female_hh", "indigenous", 
                           "dirtfloor", "hospital_distance")

t_test_results <- lapply(variables_of_interest, function(variable) {
  t_test_result <- t.test(get(variable) ~ treatment_locality, 
                          data = eligible_baseline_data)
  return(t_test_result)
})

for (i in seq_along(variables_of_interest)) {
  cat("Variable:", variables_of_interest[i], "\n")
  print(t_test_results[[i]])
  cat("\n")
}



#Second Question

eligible_data <- subset(data, eligible == 1 & treatment_locality == 1)

eligible_data <- subset(data, eligible == 1)

percentage_enrolled <- tapply(eligible_data$enrolled, eligible_data$treatment_locality, function(x) sum(x) / length(x) * 100)

cat("Percentage of people enrolled in treatment area:", percentage_enrolled[1], "%\n")
cat("Percentage of people enrolled in control area:", percentage_enrolled[0], "%\n")

#third question: I think we shouldn't be worried about spillovers because there is full compliance
#to the treatment in the treated areas and no one in the control group in the treatment area.

#fourth question

ttestdetermination <- subset(data, eligible == 1 & round == 1)


