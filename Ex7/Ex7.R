library(ggplot2)
library(dplyr)
library(corrplot)

data(mtcars)

#11
summary(mtcars)

#12
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Miles Per Gallon (mpg)", x = "Miles Per Gallon", y = "Frequency")

#13
ggplot(mtcars, aes(x = as.factor(cyl))) +
  geom_bar(fill = "orange") +
  labs(title = "Frequency of Cars with Different Numbers of Cylinders", 
       x = "Number of Cylinders", y = "Frequency")

#14
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation between mpg and hp", x = "Horsepower (hp)", y = "Miles Per Gallon (mpg)")

#15
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "mpg Variation Across Cars with Different Numbers of Cylinders", 
       x = "Number of Cylinders", y = "Miles Per Gallon (mpg)")

#16
ggplot(mtcars, aes(x = as.factor(gear), fill = as.factor(am))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("red", "green"), labels = c("Automatic", "Manual")) +
  labs(title = "Association between Transmission Type and Number of Gears", 
       x = "Number of Gears", y = "Proportion", fill = "Transmission Type")

#17
ggplot(mtcars, aes(x = hp, y = mpg, size = wt, color = wt)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Interaction between mpg, hp, and wt", x = "Horsepower (hp)", y = "Miles Per Gallon (mpg)")

#18
ggplot(mtcars, aes(x = hp, y = mpg, color = wt)) +
  geom_point(size = 3) + # Points colored by weight
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Add regression line
  labs(title = "Linear Regression: Predicting mpg Based on hp (Colored by wt)",
       x = "Horsepower (hp)", y = "Miles Per Gallon (mpg)") +
  scale_color_gradient(low = "blue", high = "red") # Gradient for weight

#19
correlation_matrix <- cor(mtcars)
correlation_matrix

#110
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, title = "Correlation Matrix Heatmap")

