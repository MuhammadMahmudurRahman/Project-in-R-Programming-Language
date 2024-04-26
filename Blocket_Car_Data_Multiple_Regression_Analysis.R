
# Car Price analysis, data taken from www.blocket.se

# Load the readxl package
library(readxl)

# Load Excel file
car_data <- read_excel("C:/Rumon/DS23EC/6. R/Kunskapskontroll/Final/Submission for Kunskapskontroll/blocket_car_data.xlsx")

# View dimension and head 
dim(car_data)
head(car_data)

# View summary statistics
summary(car_data)


#EDA
# Load necessary libraries
library(dplyr)

# Structure of the dataset
str(car_data)

# Summary of numeric variables
summary(car_data$Car_Price)
summary(car_data$Mileage)
summary(car_data$Model_Year)
summary(car_data$Horsepower)

# Summary of categorical variables
table(car_data$County)
table(car_data$Fuel)
table(car_data$Gearbox)
table(car_data$Car_Type)
table(car_data$Drivetrain_System)
table(car_data$Color)
table(car_data$Brand)
table(car_data$Model)

# Checking missing values
sum(is.na(car_data))

# Confirmation no missing values
missing_values <- colSums(is.na(car_data))
print(missing_values)

# Load necessary libraries
library(ggplot2)

# Summary statistics
summary(car_data)

# Distribution of numeric variables
numeric_vars <- select_if(car_data, is.numeric)
summary(numeric_vars)

# Distribution of categorical variables
categorical_vars <- select_if(car_data, is.factor)
sapply(categorical_vars, table)

# Visualization of numeric variables

# Histogram for Car Price and Milage

par(mfrow=c(2, 2))
hist(car_data$Car_Price, main = "Car Price", xlab = "Price") # Car Price
hist(car_data$Mileage, main = "Mileage", xlab = "Mileage") # Mileage

# Horsepower
class(car_data$Horsepower)
unique(car_data$Horsepower)
car_data$Horsepower <- as.numeric(car_data$Horsepower)
sum(is.na(car_data$Horsepower))

summary(car_data$Horsepower)
str(car_data$Horsepower)
summary(car_data$Horsepower)
str(car_data$Horsepower)

# Majority horsepower between 100-200
car_data <- car_data[complete.cases(car_data$Horsepower), ]
hist(car_data$Horsepower, main = "Horsepower", xlab = "Horsepower")

# Car's model
# Most of the car's model year is between 2015-2020
hist(car_data$Model_Year, main = "Model Year", xlab = "Year")


# Load ggplot2 package
library(ggplot2)

# Boxplot of Car Price by Car Type
ggplot(car_data, aes(x = Car_Type, y = Car_Price, fill = Car_Type)) +
  geom_boxplot() +
  labs(title = "Car Price by Car Type")

# Scatter plot of Car Price vs Mileage
ggplot(car_data, aes(x = Mileage, y = Car_Price)) +
  geom_point() +
  labs(title = "Scatter plot of Car Price vs Mileage")

# Bar plot of Car Count by County
ggplot(car_data, aes(x = County)) +
  geom_bar() +
  labs(title = "Bar plot of Car Count by County")

# Bar plot of Car Count by Fuel Type
ggplot(car_data, aes(x = Fuel)) +
  geom_bar() +
  labs(title = "Bar plot of Car Count by Fuel Type")


# Load the dplyr package for the select function
library(dplyr)

# Select only numeric columns from car_data
numeric_cols <- select(car_data, where(is.numeric))

# Calculate correlation matrix
correlation_matrix <- cor(numeric_cols)

# View the correlation matrix
print(correlation_matrix)



# Multiple Regression Analysis

# Convert categorical variables to factors
car_data$County <- as.factor(car_data$County)
car_data$Fuel <- as.factor(car_data$Fuel)
car_data$Gearbox <- as.factor(car_data$Gearbox)
car_data$Car_Type <- as.factor(car_data$Car_Type)
car_data$Drivetrain_System <- as.factor(car_data$Drivetrain_System)
car_data$Color <- as.factor(car_data$Color)
car_data$Brand <- as.factor(car_data$Brand)

# Check the number of levels for each factor variable
summary(sapply(car_data, function(x) length(unique(x))))
str(car_data)
sum(is.na(car_data))

# Load required libraries
library(caret)

# Split data into training and testing sets
set.seed(456)  # For reproducibility
train_index <- sample(seq_len(nrow(car_data)), size = floor(0.7 * nrow(car_data)))
train_data <- car_data[train_index, ]
test_data <- car_data[-train_index, ]

# Build multiple regression models

# Model 1: Using all available predictors
model1 <- lm(Car_Price ~ ., data = train_data)

# Model 2: Using a subset of predictors
model2 <- lm(Car_Price ~ Mileage + Model_Year + Horsepower + Brand, data = train_data)

# Model 3: Using a different subset of predictors
model3 <- lm(Car_Price ~ Fuel + Drivetrain_System + Color + Car_Type, data = train_data)


# Print summary of the models
summary(model1)
summary(model2)
summary(model3)

# Calculate R-squared for Model 1
r_squared_model1 <- summary(model1)$r.squared

# Calculate R-squared for Model 2
r_squared_model2 <- summary(model2)$r.squared

# Calculate R-squared for Model 3
r_squared_model3 <- summary(model3)$r.squared

# Print R-squared for each model
cat("R-squared for Model 1:", r_squared_model1, "\n")
cat("R-squared for Model 2:", r_squared_model2, "\n")
cat("R-squared for Model 3:", r_squared_model3, "\n")


# Based on the R-squared values:
# Model 1: R-squared = 0.9373982 
# Model 2: R-squared = 0.7685589
# Model 3: R-squared = 0.3364559 
# A higher R-squared value indicates that the model explains a larger proportion of the variance in the dependent variable (Car_Price). 
# Therefore, Model 1 has the highest R-squared value, indicating that it provides the best fit to the data among the three models.


# Function to calculate RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}


# Function to calculate Adjusted R-squared
adjusted_r_squared <- function(model, data) {
  1 - (1 - summary(model)$adj.r.squared) * ((nrow(data) - 1) / (nrow(data) - length(coefficients(model)) - 1))
}

# Fit the three models (assuming model1, model2, and model3 are already created)

# Calculate RMSE for each model
rmse_model1 <- rmse(car_data$Car_Price, predict(model1))
rmse_model2 <- rmse(car_data$Car_Price, predict(model2))
rmse_model3 <- rmse(car_data$Car_Price, predict(model3))

# Calculate Adjusted R-squared for each model
adj_r_squared_model1 <- adjusted_r_squared(model1, car_data)
adj_r_squared_model2 <- adjusted_r_squared(model2, car_data)
adj_r_squared_model3 <- adjusted_r_squared(model3, car_data)

# Calculate BIC for each model
bic_model1 <- BIC(model1)
bic_model2 <- BIC(model2)
bic_model3 <- BIC(model3)

# Print the results
cat("RMSE for Model 1:", rmse_model1, "\n")
cat("RMSE for Model 2:", rmse_model2, "\n")
cat("RMSE for Model 3:", rmse_model3, "\n")

cat("Adjusted R-squared for Model 1:", adj_r_squared_model1, "\n")
cat("Adjusted R-squared for Model 2:", adj_r_squared_model2, "\n")
cat("Adjusted R-squared for Model 3:", adj_r_squared_model3, "\n")

cat("BIC for Model 1:", bic_model1, "\n")
cat("BIC for Model 2:", bic_model2, "\n")
cat("BIC for Model 3:", bic_model3, "\n")



# Predict Car Price using 3 models and compare with the original price
# given in the dataset which is 419 000

# New data for the car
new_car_data <- data.frame(
  County = "Stockholm",
  Fuel = "Miljöbränsle/Hybrid",
  Gearbox = "Automat",
  Mileage = 14452,
  Model_Year = 2017,
  Car_Type = "SUV",
  Drivetrain_System = "Fyrhjulsdriven",
  Horsepower = 412,
  Color = "Silver",
  Brand = "Volvo",
  Model = "XC90"
)

# Predict Car Price using Model 1
prediction_model1 <- predict(model1, newdata = new_car_data)

# Predict Car Price using Model 2
prediction_model2 <- predict(model2, newdata = new_car_data)

# Predict Car Price using Model 3
prediction_model3 <- predict(model3, newdata = new_car_data)

# Real original price
original_price <- 419000

# Compare predicted prices with original price
comparison <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  Predicted_Price = c(prediction_model1, prediction_model2, prediction_model3),
  Original_Price = original_price
)

# Print comparison

print(comparison)
# model1 gives more accurate result


# Statistics, CI and PI
# Calculate confidence intervals
conf_intervals <- predict(model1, newdata = new_car_data, interval = "confidence", level = 0.95)

# Calculate prediction intervals
pred_intervals <- predict(model1, newdata = new_car_data, interval = "prediction", level = 0.95)

# Print confidence intervals
print(conf_intervals)

# Print prediction intervals
print(pred_intervals)



# Extra
# Now, I would like to do some extra analysis as for example, predict car price using new data and my 3 models
# Create a new data frame with the new data
new_data <- data.frame(
  County = "Stockholm",
  Fuel = "Bensin",
  Gearbox = "Automat",
  Mileage = 9000,
  Model_Year = 2019,
  Car_Type = "Kombi",
  Drivetrain_System = "Tvåhjulsdriven",
  Horsepower = 150,
  Color = "Silver",
  Brand = "Volvo",
  Model = "V60"
)

# Predict Car Price using Model 1
prediction_model1 <- predict(model1, newdata = new_data)

# Predict Car Price using Model 2
prediction_model2 <- predict(model2, newdata = new_data)

# Predict Car Price using Model 3
prediction_model3 <- predict(model3, newdata = new_data)

# Print predictions
cat("Predicted Car Price using Model 1:", prediction_model1, "\n")
cat("Predicted Car Price using Model 2:", prediction_model2, "\n")
cat("Predicted Car Price using Model 3:", prediction_model3, "\n")


# Model 1 gives more accurate price among the 3 models for totally new unknown data
summary(model1)

# Thank you so much! :)