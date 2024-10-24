library(arules)
library(arulesViz)
data("Groceries")

#1a
str(Groceries)          
summary(Groceries)      

#1b
dataset_dimensions <- dim(Groceries)  
cat("Dimensions of the Groceries dataset:\n")
print(dataset_dimensions)

#1c
distinct_items <- unique(unlist(as(Groceries, "list")))``
cat("List of distinct items in the Groceries dataset:\n")
print(distinct_items)

#2a

itemFrequencyPlot(Groceries, topN = 5, type = "relative", col = "lightblue", main = "Top 5 Items - Relative Frequency")
#3a
rules <- apriori(Groceries, parameter = list(supp = 0.002, conf = 0.5))

#3b
top_5_rules <- head(rules, n = 5)
inspect(top_5_rules)

#3c
sorted_rules <- sort(rules, by = "lift")
top_5_sorted_rules <- head(sorted_rules, n = 5)
inspect(top_5_sorted_rules)

#4a
plot(rules, method = "scatter", jitter = 0, main = "Support vs Confidence")
plot(rules, method = "grouped")
plot(rules, method = "matrix", jitter = 0, main = "Matrix Representation of Rules")
plot(rules, method = "graph", jitter = 0, main = "Graph Representation of Rules")

#5ai
rules_with_rice <- subset(rules, lhs %pin% "rice")
inspect(head(rules_with_rice, 5))

#5aii
plot(rules_with_rice)

#5bi
rules_with_milk <- subset(rules, rhs %pin% "whole milk")
inspect(head(rules_with_milk, 5))

#5bii
plot(rules_with_milk,jitter=0)



