library(ggplot2)
library(dplyr)

admissions_data <- read.csv("C:/Users/Asus/Desktop/EDA/Ex8/admissions.csv")

#1
ggplot(admissions_data, aes(x = gpa, y = admitted)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5) +
  labs(title = "Jitter Plot of GPA vs Admission Status", 
       x = "GPA", 
       y = "Admitted (1 = Yes, 0 = No)") +
  theme_minimal()

#2
logistic_model <- glm(admitted ~ gpa, data = admissions_data, family = "binomial")

print(summary(logistic_model))

#3
ggplot(admissions_data, aes(x = gpa, y = admitted)) +
  geom_jitter(width = 0.1, height = 0.1, alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
  labs(title = "Jitter Plot with Logistic Regression Curve", 
       x = "GPA", 
       y = "Admitted (1 = Yes, 0 = No)") +
  theme_minimal()

#4
gpa_summary <- admissions_data %>%
  group_by(gpa) %>%
  summarise(admitted_count = sum(admitted), 
            total_count = n(), 
            admission_rate = admitted_count / total_count)


print(gpa_summary)

#5
ggplot(gpa_summary, aes(x = gpa, y = admission_rate)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of GPA vs Proportion of Admitted Students", 
       x = "GPA", 
       y = "Proportion Admitted") +
  theme_minimal()

#6
weighted_model <- glm(admission_rate ~ gpa, data = gpa_summary, weights = total_count, family = "binomial")

print(summary(weighted_model))

#7
ggplot(gpa_summary, aes(x = gpa, y = admission_rate, size = total_count)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
  labs(title = "Weighted Scatter Plot with Logistic Regression Curve", 
       x = "GPA", 
       y = "Proportion Admitted") +
  theme_minimal()
