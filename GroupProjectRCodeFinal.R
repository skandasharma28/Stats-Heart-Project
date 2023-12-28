# Install and load necessary packages
install.packages("readxl")
library(readxl)
library(ggplot2)
install.packages("pscl")
library(pscl)
install.packages("ResourceSelection")
library(ResourceSelection)
install.packages("")
library(lmtest)
#Read the data
data <- read_excel(file.choose())

# View the structure of the loaded data
str(data)

#Converting categorical variables into factors

data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exng <- as.factor(data$exng)
data$output <- as.factor(data$output)
data$thall <- as.factor(data$thall)

str(data)

#Checking for N/a Rows
rows <- nrow(data)
nrow(data[is.na(data$cp) | is.na(data$thall),]) #NO N/a rows!! YAYY

# Check to see if a categorical variable is biased
xtabs(~ output + sex, data=data)
xtabs(~ output + cp, data=data)
xtabs(~ output + fbs, data=data)
xtabs(~ output + restecg, data=data)
xtabs(~ output + exng, data=data)

# Perform logistic regression
logistic <- glm(output ~ ., data = data, family = "binomial")

# Summary of the logistic regression model
summary(logistic)

#Calculating Pseudo r^2
ll.null <- logistic$null.deviance/ -2
ll.proposed <- logistic$deviance/ -2
rSqOrignal <- (ll.null - ll.proposed) / ll.null


#Deleting columns that are not statistically significant
columns_to_delete <- c("age", "chol", "fbs","restecg","slp","thall", "trtbps") #"thalachh","exng")
data <- data[, !(names(data) %in% columns_to_delete)]
head(data)

#Logistic Regression with updated/significant columns
logistic_2 <- glm(output ~ ., data = data, family = "binomial")
summary(logistic_2)

#Calculating Pseudo r^2 again
ll.null2 <- logistic_2$null.deviance/ -2
ll.proposed2 <- logistic_2$deviance/ -2
rSqUpdated <- (ll.null2 - ll.proposed2) / ll.null2


#View Data ONE MORE TIME!
summary(data)

#Bar Plots of categorical variables and Output

print("OUTPUT: 0 = HEALTHY and 1 = AT RISK!!")
print("Sex: 0 = Female and 1 = Male!!")

#Bar - 1
colors <- c("green", "red")
plot <- ggplot(data, aes(x = sex, fill = output)) +
  geom_bar(position = "dodge", stat = "count") +
  
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    plot.title = element_text(face = "bold", hjust = 0.5), 
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Bar Plot of Age and Output",
    x = "Sex (0:F, 1:M)",
    y = "Count",
    fill = "Output"
  )
print(plot)


#Bar - 2
plot <- ggplot(data, aes(x = cp, fill = output)) +
  geom_bar(position = "dodge", stat = "count") +
  
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    plot.title = element_text(face = "bold", hjust = 0.5), 
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Bar Plot of Type of Chest Pain and Output",
    x = "Chest Pain (1:typical, 2: atypical, 3: non-anginal)",
    y = "Count",
    fill = "Output"
  )
print(plot)

#Bar - 3
plot <- ggplot(data, aes(x = exng, fill = output)) +
  geom_bar(position = "dodge", stat = "count") +
  
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(
    text = element_text(size = 16), 
    plot.title = element_text(face = "bold", hjust = 0.5), 
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Bar Plot of Chest Pain during exercise and Output",
    x = "Chest Pain during exercise (0:N, 1:Y)",
    y = "Count",
    fill = "Output"
  )
print(plot)


#Box plots of Numerical Variables with output

#Box Plot - 1
plot <- ggplot(data, aes(x = oldpeak, y = factor(output), fill = factor(output))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  theme_minimal() +

  theme(
    text = element_text(size = 14), 
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Boxplot of Previous Old Peak and Output",
    x = "Old Peak",
    y = "Output"
  )
print(plot)

#Box Plot - 2
plot <- ggplot(data, aes(x = caa, y = factor(output), fill = factor(output))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  
  theme(
    text = element_text(size = 14), 
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Boxplot of # of Major Blood vessels and Output",
    x = "Major Blood Vessels",
    y = "Output"
  )
print(plot)

#Box Plot - 3
plot <- ggplot(data, aes(x = thalachh, y = factor(output), fill = factor(output))) +
  geom_boxplot() +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  
  theme(
    text = element_text(size = 14), 
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Boxplot of Max Heart Rate and Output",
    x = "Max Heart Rate",
    y = "Output"
  )
print(plot)


#Come up with a model to predict risk
coefficients <- coef(logistic_2)

# Print the coefficients
print(coefficients)


# Calculate the probability
calculate_probability <- function(sex1, cp1, cp2, cp3, thalachh, exng, oldpeak, caa) {
  # Coefficients
  b0 <- -1.4121152
  b1 <- -1.5244929
  b2 <- 1.2214219
  b3 <- 1.9518797
  b4 <- 1.8507347
  b5 <- 0.0220054
  b6 <- -1.0204647
  b7 <- -0.7014139
  b8 <- -0.7225997
  
  log_odds <- b0 + b1 * sex1 + b2 * cp1 + b3 * cp2 + b4 * cp3 + b5 * thalachh +
    b6 * exng + b7 * oldpeak + b8 * caa
  
  probability <- 1 / (1 + exp(-log_odds))
  percentage <- round(probability * 100, 2)
  
  return(percentage)
}

# Example usage
sex1 <- 1
cp1 <- 0
cp2 <- 0
cp3 <- 0
thalachh <- 185
exng <- 0
oldpeak <- 1
caa <- 3

predicted_probability <- calculate_probability(sex1, cp1, cp2, cp3, thalachh, exng, oldpeak, caa)
cat(predicted_probability, "%")




















