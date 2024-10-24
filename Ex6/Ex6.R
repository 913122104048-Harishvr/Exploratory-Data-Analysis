library(ggplot2)
library(dplyr)

data(iris)
head(iris)
summary(iris)

iris_data <- iris[, 1:4]  

iris_data_scaled <- scale(iris_data)  

set.seed(123)

wss <- sapply(1:10, function(k) {
  kmeans(iris_data_scaled, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, xlab = "Number of Clusters",
     ylab = "Total Within Sum of Squares", main = "Elbow Method")

kmeans_result <- kmeans(iris_data_scaled, centers = 3, nstart = 10)

print(kmeans_result)

iris_clustered <- iris %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))

head(iris_clustered)

ggplot(iris_clustered, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering of Iris Flowers",
       x = "Sepal Length",
       y = "Sepal Width") +
  theme_minimal()

ggplot(iris_clustered, aes(x = Petal.Length, y = Petal.Width, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering of Iris Flowers (Petal Dimensions)",
       x = "Petal Length",
       y = "Petal Width") +
  theme_minimal()
