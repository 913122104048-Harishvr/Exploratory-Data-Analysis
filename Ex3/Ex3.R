SalesData <- read.csv("C:/Users/Asus/Desktop/EDA/Ex3/SalesData.csv")

#1a
variable_info <- data.frame(Name = names(SalesData), Type = sapply(SalesData, class))
print(variable_info)

#1b
useless_vars <- c("Order Date", "Customer Name", "Company")
print(useless_vars)

useless_explanation <- c(
  "Order Date: Dates may not provide usable statistical information without context (e.g., time series analysis).",
  "Customer Name: Individual names do not provide aggregate information and can’t be analyzed statistically.",
  "Company: If it refers to individual companies without grouping, it may not yield usable insights."
)
print(useless_explanation)

#1c
missing_data_vars <- colnames(SalesData)[colSums(is.na(SalesData)) > 0]
print(missing_data_vars)


#2a
S1 <- na.omit(SalesData)
cat("Dataset after deleting rows with missing data (S1):\n")
print(S1)

#2b
mean_imputed_data <- SalesData
for (i in 1:ncol(mean_imputed_data)) {
  if (is.numeric(mean_imputed_data[[i]])) {
    mean_imputed_data[[i]][is.na(mean_imputed_data[[i]])] <- mean(mean_imputed_data[[i]], na.rm = TRUE)
  }
}
cat("\nDataset after mean imputation:\n")
print(mean_imputed_data)

#2c
random_imputed_data <- SalesData
for (i in 1:ncol(random_imputed_data)) {
  if (is.numeric(random_imputed_data[[i]])) {
    min_val <- min(random_imputed_data[[i]], na.rm = TRUE)
    max_val <- max(random_imputed_data[[i]], na.rm = TRUE)
    random_imputed_data[[i]][is.na(random_imputed_data[[i]])] <- runif(sum(is.na(random_imputed_data[[i]])), min_val, max_val)
  }
}
cat("\nDataset after random value imputation:\n")
print(random_imputed_data)

#2d
categorical_imputed_data <- SalesData
for (i in 1:ncol(categorical_imputed_data)) {
  if (is.factor(categorical_imputed_data[[i]]) || is.character(categorical_imputed_data[[i]])) {
    categorical_imputed_data[[i]][is.na(categorical_imputed_data[[i]])] <- sample(categorical_imputed_data[[i]][!is.na(categorical_imputed_data[[i]])], sum(is.na(categorical_imputed_data[[i]])), replace = TRUE)
  }
}
cat("\nDataset after categorical value imputation:\n")
print(categorical_imputed_data)


#3a
numeric_columns <- sapply(SalesData, is.numeric)
par(mfrow = c(2, 4))

for (col in names(SalesData)[numeric_columns]) {
  boxplot(SalesData[[col]], main = col, ylab = col)
}


outliers <- list()
for (col in names(SalesData)[numeric_columns]) {
  q1 <- quantile(SalesData[[col]], 0.25, na.rm = TRUE)
  q3 <- quantile(SalesData[[col]], 0.75, na.rm = TRUE)
  iqr <- IQR(SalesData[[col]], na.rm = TRUE)
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  outliers[[col]] <- SalesData[[col]][SalesData[[col]] < lower_bound | SalesData[[col]] > upper_bound]
}

cat("\nOutliers detected in numeric variables:\n")
print(outliers)

#3b
order_priority_counts <- table(SalesData$Order.Priority)
cat("\nOrder Priority value counts:\n")
print(order_priority_counts)


unexpected_values <- setdiff(unique(SalesData$Order.Priority), c("Low", "Medium", "High"))
cat("\nUnexpected values in Order Priority:\n")
print(unexpected_values)
