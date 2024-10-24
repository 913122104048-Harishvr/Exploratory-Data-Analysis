install.packages("readr")
install.packages("arules")
install.packages("corrplot")

library(readr)
library(arules)
library(corrplot)

netflix_stock <- read.csv("C:/Users/Asus/Desktop/EDA/Ex10/netflixstock.csv")


#1
model <- lm(Close ~ Open + Volume, data = netflix_stock)

summary(model)

predictions <- predict(model, netflix_stock)

mse <- mean((netflix_stock$Close - predictions)^2)
r_squared <- summary(model)$r.squared

cat("Mean Squared Error:", mse, "\n")
cat("R-Squared:", r_squared, "\n")

#2
netflix_stock$Open_range <- cut(netflix_stock$Open, breaks = c(-Inf, 250, 260, 270, 280, Inf))
netflix_stock$Close_range <- cut(netflix_stock$Close, breaks = c(-Inf, 250, 260, 270, 280, Inf))
netflix_stock$Volume_range <- cut(netflix_stock$Volume, breaks = c(-Inf, 5000000, 10000000, 15000000, Inf))

transactions <- as(netflix_stock[, c("Open_range", "Close_range", "Volume_range")], "transactions")

rules <- apriori(transactions, parameter = list(supp = 0.1, conf = 0.8))

inspect(sort(rules, by = "lift")[1:5])

#3
price_correlation <- cor(netflix_stock[, c("Open", "High", "Low", "Close")])

print(price_correlation)

corrplot(price_correlation, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

#4
netflix_stock$Price_Change <- netflix_stock$Close - netflix_stock$Open

plot(netflix_stock$Volume, netflix_stock$Price_Change, xlab = "Volume", ylab = "Price Change", main = "Volume vs Price Change")

volume_change_model <- lm(Price_Change ~ Volume, data = netflix_stock)

summary(volume_change_model)

abline(volume_change_model, col = "red")
