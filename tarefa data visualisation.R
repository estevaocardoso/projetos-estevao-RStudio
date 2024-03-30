summary(census80)

num_working_mothers <- sum(census80$workedm == 1)
total_observations <- nrow(census80)
fraction_working_mothers <- num_working_mothers / total_observations
fraction_working_mothers <- round(fraction_working_mothers, 2)
fraction_working_mothers

q3_weeks_worked <- quantile(census80$weeksm, 0.75)

# Print the third quartile of weeks worked
print(paste("Third quartile of weeks worked:", round(q3_weeks_worked, 2)))

prop_hispanic <- mean(census80$hispm)

# Print the proportion of Hispanic mothers
print(paste("Proportion of Hispanic mothers:", round(prop_hispanic * 100, 2), "%"))

median_age_second_child <- median(census80$ageq2nd)

# Print the median age of the second child in quarters
print(paste("Median age of the second child in quarters:", median_age_second_child))
