  #                                                       IDS Assignment 2 
#                                                         PART - 1
#                                                   Exploratory Data Analysis
library(psych)
library(dplyr) 
library(ggplot2) 
library(caret)
library(tidyverse)
library("ROCR")
library(kknn)

library(gridExtra)
library(grid)
library(lattice)

#Retrieving and storing the data from Lending.csv file.

lending_Data <- read.csv("/Users/sivanagmaddineni/Downloads/Lending.csv")
table = as_tibble(lending_Data)

glimpse(table)

df <- table[, colSums(is.na(table)) < length(table)/2]
head(df)

factor_vars <- df[, sapply(df, is.factor)]
head(factor_vars)
numerical_vars <- df[, sapply(df, is.numeric)]
head(numerical_vars)

# We can see the distribution of Loan Amount using 2 different plots.
p1 <- ggplot(data = df, aes(loan_amnt)) + geom_histogram(binwidth = 1000)
p2 <- ggplot(data = df, aes(loan_amnt)) + geom_density(fill = "gray")
grid.arrange(p1, p2, nrow = 2)


summary(df$loan_amnt)
lending_Data = lending_Data %>% mutate(residence_property = I(residence_property == "Own") %>% as.numeric())

#Plotting histogram to analyze the average loan amount taken by the customers.
ggplot(lending_Data, aes(x = loan_amnt))+ geom_histogram( fill = "blue", col = "white" ,na.rm = TRUE)+ ggtitle("Loan amount Vs Number of Customers")+xlab("Loan Amount")+ylab("Number of Customers who took the loan") 

df1 = df[,c(2,6)]

#Plotting histogram to analyze the customers who took loan has a owned property or not
ggplot(df1, aes(x = residence_property, y =loan_amnt))+geom_col(fill = "yellow", col = "yellow" ,na.rm = TRUE)
