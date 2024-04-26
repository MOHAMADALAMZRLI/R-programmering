# Steg 1: Ladda packages och data
install.packages(c("readxl", "leaps", "broom", "caret", "MASS", "ggplot2"))
library(readxl)
library(randomForest)
library(caret)

# file path
file <- "C:/Users/46736/UC/R-programmering/Samlade_bil_data (1).xlsx"



# Read the Excel file
data_bil <- read_excel(file)

# Steg 2: Splitar datan till training och testing sets
set.seed(123)
index <- createDataPartition(data_bil$Price, p = 0.3, list = FALSE)
training_data <- data_bil[index, ]
testing_data <- data_bil[-index, ]

# Steg 3: Träna  Random Forest regression model
rf_model <- randomForest(Price ~ ., data = training_data)

# Step 4: Utvärdera
predictions <- predict(rf_model, newdata = testing_data)
mae <- mean(abs(predictions - testing_data$Price))
mse <- mean((predictions - testing_data$Price)^2)
rsquared <- 1 - (sum((testing_data$Price - predictions)^2) / sum((testing_data$Price - mean(testing_data$Price))^2))

# Steg 5: Cross-validation
num_folds <- 10
ctrl <- trainControl(method = "cv", number = num_folds)
model <- train(Price ~ ., data = training_data, method = "rf", trControl = ctrl)

# Print the results
print(model)

# Scatter plot of actual vs. predicted prices
plot(testing_data$Price, predictions, xlab = "Actual Price", ylab = "Predicted Price", main = "Actual vs. Predicted Prices")
abline(0, 1, col = "red")  # Add a line of equality (ideal prediction)

