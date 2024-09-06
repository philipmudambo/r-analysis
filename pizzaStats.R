#Loading tidyverse 
suppressPackageStartupMessages(library(tidyverse))
#Loading lm.beta 
suppressPackageStartupMessages(library(lm.beta))
#Loading the pizza.csv file
pizza <- read_csv('pizza.csv')
pizza
#Rounding all float/dbl values to two decimal places 
pizza <- pizza %>% mutate_if(is.numeric, function(x) round(x, 2))
pizza

#QUESTION 1
#Correlation matrix for temperature, bill, pizzas, and got_wine
Q1 <- cor(pizza[, c("temperature", "bill", "pizzas", "got_wine")])
Q1

#QUESTION 2
#Filter data for Laura in the East branch
filtered_pizza <- pizza %>% filter(operator == "Laura" & branch == "East")
filtered_pizza
# Correlation matrix for the specified columns
Q2 <- cor(filtered_pizza[, c("time", "temperature", "bill", "pizzas")])
Q2

#QUESTION 3
#Taking 'got_wine' as the dependent variable, and 'temperature', 'bill', 'pizzas' as independent variables
#Logistic regression model
Q3 <- glm(got_wine ~ temperature + bill + pizzas, data = pizza, family = "binomial")
Q3
summary(Q3)

#QUESTION 4
#Taking 'bill' as the dependent variable, and 'temperature', 'pizzas', 'got_wine' as independent variables
#Linear regression model
r_model <- lm(bill ~ temperature + pizzas + got_wine, data = pizza)
r_model
#Standardized coefficients using lm.beta()
Q4 <- lm.beta(r_model)
Q4

#QUESTION 5
#Taking 'bill' as the dependent variable, and 'temperature', 'pizzas', 'got_wine', 'operator' as independent variables
#Model without 'operator'
model_wo <- lm(bill ~ temperature + pizzas + got_wine, data = pizza)
model_wo
#Model with 'operator'
model_w <- lm(bill ~ temperature + pizzas + got_wine + operator, data = pizza)
model_w
#Calculate AIC for each model
AICmodel_wo <- AIC(model_wo)
AICmodel_wo
AICmodel_w <- AIC(model_w)
AICmodel_w
#Determining the better model based on AIC
Q5 <- ifelse(AICmodel_wo < AICmodel_w, AICmodel_wo, AICmodel_w)
Q5 #from this output, the better model based on the AIC is the one without the operator

