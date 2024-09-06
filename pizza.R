#Loading tidyverse
suppressPackageStartupMessages(library(tidyverse))
#Loading the pizza.csv file
pizza <- read_csv('pizza.csv')
pizza
#Rounding all float/dbl values to two decimal places 
pizza <- pizza %>% mutate_if(is.numeric, function(x) round(x, 2))
pizza

#QUESTION 1
library(dplyr)
dataframe1 <- pizza %>% filter(free_wine == 1, discount_customer == 1, pizzas > 4) %>% pull(driver)
#Converting to data frame 
dataframe2 <- as.data.frame(dataframe1)
dataframe2
#Convertion of the CodeGrade output from factor <fct> to <chr>
Q1 <- as.character(dataframe2$dataframe1)
Q1

#QUESTION 2
#Creating a variable that is the ratio of bill to pizza
Q2 <- pizza %>% mutate(ratio = bill / pizzas) %>% summarize(mean_ratio = mean(ratio)) %>% pull(mean_ratio)
Q2

#Question 3
#Variance in pizzas for each day
Q3 <- pizza %>% group_by(day) %>% summarize(var_pizzas = var(pizzas)) %>% ungroup() %>% pull(var_pizzas)
Q3

#QUESTION 4
#Operator with the higher average bill
Q4 <- pizza %>% group_by(operator) %>% summarize(mean_bill = mean(bill)) %>% arrange(desc(mean_bill)) %>%head(1) %>% pull(operator)
Q4 #Melissa was the operator with the highest average bill

#QUESTION 5
# Assuming your data is in a tibble or data frame named 'pizza'
Q5 <- pizza %>% group_by(day, driver) %>% summarize(max_free_wine = max(free_wine)) %>% arrange(desc(max_free_wine)) %>% head(1) %>% pull(max_free_wine)
Q5