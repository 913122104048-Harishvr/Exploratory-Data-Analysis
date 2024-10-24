install.packages(c("rpart", "rpart.plot", "ggplot2", "dplyr"))

library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
detach("package:arules", unload = TRUE)

data(iris)

sum(is.na(iris))

set.seed(123)
train_indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

decision_tree <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                       data = train_data, method = "class")


graphics.off() 


par(mar = c(4, 4, 2, 1))


rpart.plot(decision_tree, type = 3, extra = 101, fallen.leaves = TRUE,
           main = "Decision Tree for Iris Species Classification")


predictions <- predict(decision_tree, test_data, type = "class")

confusion_matrix <- table(test_data$Species, predictions)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy: ", round(accuracy * 100, 2), "%", sep = ""))
