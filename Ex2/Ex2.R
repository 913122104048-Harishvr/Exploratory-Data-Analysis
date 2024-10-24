CO2_data <- read.csv("C:/Users/Asus/Desktop/EDA/Ex2/CO2.csv")

#2
t_test_result_unequal <- t.test(uptake ~ Treatment, data = CO2_data, var.equal = FALSE)
print(t_test_result_unequal)

#3
t_test_result_equal <- t.test(uptake ~ Treatment, data = CO2_data, var.equal = TRUE)
print(t_test_result_equal)

#4
chilled_uptake <- CO2_data$uptake[CO2_data$Treatment == "chilled"]
nonchilled_uptake <- CO2_data$uptake[CO2_data$Treatment == "nonchilled"]
paired_t_test_result <- t.test(chilled_uptake, nonchilled_uptake, paired = TRUE)
print(paired_t_test_result)

#5
anova_result <- aov(uptake ~ Type, data = CO2_data)
summary(anova_result)