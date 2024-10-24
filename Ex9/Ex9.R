library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)

sales_data <- read.csv("C:/Users/Asus/Desktop/EDA/Ex9/sales_data.csv")

#1
sales_data$Date <- as.Date(sales_data$Date, format = "%Y-%m-%d")
sales_data$Year <- year(sales_data$Date)
sales_data$Month <- month(sales_data$Date)
sales_data$Day <- day(sales_data$Date)

#2
missing_values <- colSums(is.na(sales_data))
duplicate_rows <- sales_data[duplicated(sales_data), ]

# Print the results
print("Missing values in each column:")
print(missing_values)
print("Duplicate rows:")
print(duplicate_rows)

#3
order_quantity_stats <- summarise(sales_data, 
                                  mean_order = mean(Order_Quantity, na.rm = TRUE),
                                  median_order = median(Order_Quantity, na.rm = TRUE),
                                  sd_order = sd(Order_Quantity, na.rm = TRUE))
profit_stats <- summarise(sales_data, 
                          mean_profit = mean(Profit, na.rm = TRUE),
                          median_profit = median(Profit, na.rm = TRUE),
                          sd_profit = sd(Profit, na.rm = TRUE))

print("Order Quantity Statistics:")
print(order_quantity_stats)
print("Profit Statistics:")
print(profit_stats)

#4
top_products <- sales_data %>%
  group_by(Product) %>%
  summarise(total_quantity = sum(Order_Quantity, na.rm = TRUE)) %>%
  arrange(desc(total_quantity)) %>%
  head(3)

print("Top 3 most frequent products:")
print(top_products)

#5
sales_data$Month_Year <- format(sales_data$Date, "%Y-%m")
monthly_sales <- sales_data %>%
  group_by(Month_Year) %>%
  summarise(total_sales = sum(Order_Quantity, na.rm = TRUE),
            total_profit = sum(Profit, na.rm = TRUE))


ggplot(monthly_sales, aes(x = Month_Year, y = total_sales, group = 1)) +
  geom_line() +
  labs(title = "Monthly Sales Trend", x = "Month-Year", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#6
monthly_sales$Month_Year <- as.yearmon(monthly_sales$Month_Year)
monthly_sales <- monthly_sales[order(monthly_sales$Month_Year), ]
monthly_sales$smoothed_sales <- rollmean(monthly_sales$total_sales, 12, fill = NA)

ggplot(monthly_sales, aes(x = Month_Year, y = smoothed_sales, group = 1)) +
  geom_line(color = "blue") +
  labs(title = "12-Month Moving Average of Sales", x = "Month-Year", y = "Smoothed Sales")

#7
adult_sales <- sales_data %>%
  filter(Age_Group == "Adults (35-64)") %>%
  group_by(Date) %>%
  summarise(total_sales = sum(Order_Quantity, na.rm = TRUE))

ggplot(adult_sales, aes(x = Date, y = total_sales)) +
  geom_line(color = "green") +
  labs(title = "Sales for Adults (35-64)", x = "Date", y = "Total Sales")

#8
highest_order_per_category <- sales_data %>%
  group_by(Product_Category, Sub_Category) %>%
  summarise(max_order = max(Order_Quantity, na.rm = TRUE))

ggplot(highest_order_per_category, aes(x = Product_Category, y = max_order, fill = Sub_Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Highest Order Quantity per Product Category and Sub-Category", x = "Product Category", y = "Max Order Quantity")

#9
correlation_matrix <- cor(sales_data %>% select(Unit_Cost, Order_Quantity, Profit), use = "complete.obs")

print("Correlation matrix:")
print(correlation_matrix)

ggplot(sales_data, aes(x = Unit_Cost, y = Profit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Scatter Plot of Unit Cost vs Profit", x = "Unit Cost", y = "Profit")
