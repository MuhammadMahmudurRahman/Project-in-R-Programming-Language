# SCB Data:
years <- c(2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
           2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021,
           2022, 2023)
cars <- c(517569, 522636, 529049, 537687, 548832, 559398, 563246, 569202, 574107, 581199,
          586880, 594937, 604755, 614845, 629359, 639700, 645028, 651510, 660896, 666617,
          663102, 662586)

# Load necessary packages
library(psych)

# Measures of Central Tendency
# Mean
mean_cars <- mean(cars)
cat("Mean number of cars:", mean_cars, "\n")

# Median
median_cars <- median(cars)
cat("Median number of cars:", median_cars, "\n")

# Mode
mode_cars <- unique(cars)[which.max(tabulate(match(cars, unique(cars))))]
cat("Mode number of cars:", mode_cars, "\n")

# Measures of Dispersion
# Range
range_cars <- range(cars)
cat("Range of cars:", range_cars[2] - range_cars[1], "\n")

# Standard Deviation
sd_cars <- sd(cars)
cat("Standard Deviation of cars:", sd_cars, "\n")

# Variance
var_cars <- var(cars)
cat("Variance of cars:", var_cars, "\n")

# Visualization
# Line Plot
plot(years, cars, type = "l", xlab = "Year", ylab = "Number of Cars", main = "Total Number of Cars in Skåne County")

# Bar Plot
barplot(cars, names.arg = years, xlab = "Year", ylab = "Number of Cars", main = "Total Number of Cars in Skåne County")

# Trend Analysis
# Linear Regression
trend_model <- lm(cars ~ years)
summary(trend_model)

# Predict number of cars for 2024
predicted_cars_2024 <- predict(trend_model, newdata = data.frame(years = 2024))
cat("Predicted number of cars for 2024:", round(predicted_cars_2024), "\n")

# Predicted number of cars for 2024
predicted_cars_2024 <- 684619

# Mean value of cars
mean_cars <- mean(cars)

# Print predicted number of cars for 2024
cat("Predicted number of cars for 2024:", predicted_cars_2024, "\n")

# Print mean value of cars
cat("Mean value of cars:", mean_cars, "\n")

# Compare with other years
cat("Comparison with number of cars in other years:\n")
for (i in 1:length(years)) {
  if (cars[i] > predicted_cars_2024) {
    cat("Number of cars in", years[i], "is higher than the predicted value.\n")
  } else if (cars[i] < predicted_cars_2024) {
    cat("Number of cars in", years[i], "is lower than the predicted value.\n")
  } else {
    cat("Number of cars in", years[i], "is equal to the predicted value.\n")
  }
}

# Fit linear regression model
trend_model <- lm(cars ~ years)

# Predict number of cars for 2024
predicted_cars_2024 <- predict(trend_model, newdata = data.frame(years = 2024))

# Calculate standard error
standard_error <- sqrt(sum(resid(trend_model)^2) / trend_model$df.residual)

# Degrees of freedom
df <- length(cars) - 2

# Critical value for 95% confidence level
critical_value <- qt(0.975, df)

# Calculate confidence interval
lower_ci <- predicted_cars_2024 - (critical_value * standard_error)
upper_ci <- predicted_cars_2024 + (critical_value * standard_error)

# Print confidence interval
cat("Confidence Interval (95%): [", round(lower_ci), ", ", round(upper_ci), "]\n")

# Fit linear regression model
trend_model <- lm(cars ~ years)

# Summary of the regression model
summary(trend_model)

# Extra code for my better understanding
# Predicted values from the regression model
predicted_values <- predict(trend_model)

# Plotting the actual values and predicted values
plot(years, cars, type = "o", col = "blue", pch = 16, xlab = "Year", ylab = "Number of Cars", main = "Comparison of Actual vs. Predicted Values")
lines(years, predicted_values, col = "red", type = "o", pch = 16)
legend("topright", legend = c("Actual Values", "Predicted Values"), col = c("blue", "red"), pch = 16, cex = 0.8)

# Thank you! :)
