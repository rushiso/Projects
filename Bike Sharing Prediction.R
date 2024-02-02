# Load libraries
library(randomForest)
library(ggplot2)
library(tidyr)
library(dplyr)

# Load dataset
df<-read.csv(file.choose(),header = T)
df
# Convert data types
df$dteday <- as.Date(df$dteday)
df$dteday
# Missing value analysis
missing_values <- colSums(is.na(df))
missing_values

# Monthly distribution of total bikes rented
monthly_distribution <- df %>%
  group_by(mnth) %>%
  summarise(total_bikes_rented = sum(cnt))

# Plot monthly distribution
ggplot(monthly_distribution, aes(x = factor(mnth), y = total_bikes_rented)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Monthly Distribution of Total Bikes Rented",
       x = "Month", y = "Total Bikes Rented")

# Yearly distribution of total bikes rented
yearly_distribution <- df %>%
  group_by(yr) %>%
  summarise(total_bikes_rented = sum(cnt))

# Plot yearly distribution
ggplot(yearly_distribution, aes(x = factor(yr), y = total_bikes_rented)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(title = "Yearly Distribution of Total Bikes Rented",
       x = "Year", y = "Total Bikes Rented")

# Boxplot for outliers' analysis
boxplot(df$cnt, main = "Boxplot for Total Bikes Rented", ylab = "Total Bikes Rented")

# Split the dataset into train and test
set.seed(123)
split_ratio <- 0.8
sample_indices <- sample(1:nrow(df), size = round(split_ratio * nrow(df)))
train_data <- df[sample_indices, ]
test_data <- df[-sample_indices, ]

# Create a model using random forest algorithm
rf_model <- randomForest(cnt ~ ., data = train_data, ntree = 100)
rf_model

# Predict the performance of the model on the test dataset
predictions <- predict(rf_model, newdata = test_data)
predictions

# Analyze the performance
#  compare predicted vs. actual values
results <- data.frame(Actual = test_data$cnt, Predicted = predictions)
print(results)

# Calculate performance metrics (e.g., Mean Absolute Error, Mean Squared Error)
mae <- mean(abs(results$Actual - results$Predicted))
mse <- mean((results$Actual - results$Predicted)^2)

cat("Mean Absolute Error:", mae, "\n")
cat("Mean Squared Error:", mse, "\n")

