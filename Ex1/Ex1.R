#a
Age <- c(35, 65, 49, 30, 20, 40, 90, 54, 78, 45)
Systolic_pressure <- c(122, 120, 120, 115, 130, 131, 118, 122, 120, 115)
Diastolic_pressure <- c(83, 79, 78, 72, 90, 90, 82, 80, 82, 75)

calc_stats <- function(data) {
  n <- length(data)
  min_val <- min(data)
  max_val <- max(data)
  median_val <- median(data)
  mean_val <- mean(data)
  variance_val <- var(data)
  std_dev <- sd(data)
  
  return(c(n, min_val, max_val, median_val, mean_val, variance_val, std_dev))
}


Age_stats <- calc_stats(Age)
Systolic_stats <- calc_stats(Systolic_pressure)
Diastolic_stats <- calc_stats(Diastolic_pressure)

stats_table <- data.frame(
  Statistic = c("Number of Samples", "Minimum", "Maximum", "Median", "Mean", "Variance", "Standard Deviation"),
  Age = Age_stats,
  Systolic_Pressure = Systolic_stats,
  Diastolic_Pressure = Diastolic_stats
)

print(stats_table)

#b
library(dplyr)
library(ggplot2)

iris_data <- read.csv("C:/Users/Asus/Desktop/EDA/Ex1/Iris.csv")

head(iris_data)
tail(iris_data)

summary(iris_data)

mean_petal_width_versicolor <- iris_data %>%
  filter(Species == "versicolor") %>%
  summarize(mean_petal_width = mean(Petal.Width))

print(mean_petal_width_versicolor)


ggplot(iris_data, aes(x = Petal.Width)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black") +
  labs(title = "Frequency Distribution of Petal Width", x = "Petal Width", y = "Frequency")


sepal_length_stats <- iris_data %>%
  group_by(Species) %>%
  summarize(
    mean_sepal_length = mean(Sepal.Length),
    median_sepal_length = median(Sepal.Length),
    variance_sepal_length = var(Sepal.Length),
    std_dev_sepal_length = sd(Sepal.Length)
  )

print(sepal_length_stats)


