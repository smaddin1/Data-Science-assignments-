#                                                       IDS Assignment 2 
#                                                         PART - 1
#                                             Logistic Regression & Naive Bayes
########################### Installing all the necessary packages#################################

library(psych)
library(dplyr) 
library(e1071)  #This package in R is mainly used for naive Bayes classifier
library(ggplot2) 
library(caret)
library(tidyverse)
library("ROCR")
library(kknn)

#Retrieving and storing the data from Lending.csv file.

lending_Data <- read.csv("/Users/sivanagmaddineni/Downloads/Lending.csv")


#Converting the data into numeric form.

lending_Data = lending_Data %>% mutate(residence_property = I(residence_property == "Own") %>% as.numeric())



#Dividing the data into 2 groups one group in which the loan_default==1 and the other in which loan_default==0 and we are binding the 2 groups using a rbind function..

lending_Data= rbind(sample_n(filter(lending_Data, loan_default==1),11000), sample_n(filter(lending_Data,loan_default==0), 11000))

#printing number of rows in the data.
nrow(lending_Data)

# The set.seed() function is used to generate the sample of random numbers
set.seed(123)

#Here we are dividing the data into two sets 70% of the data is taken as sample data and remaining 30% of the data is taken as the test data.
size_sample = floor(0.70 * nrow(lending_Data))

#We are randomly dividing the data into two sets i,e we are dividing the given data into training data and testing data.
indexes_train = sample(seq_len(nrow(lending_Data)), size = size_sample)

training_data_lendingData <- lending_Data[indexes_train, ]
testing_data_lendingData <- lending_Data[-indexes_train, ]

cor.test(lending_Data$loan_amnt,lending_Data$dti)


#we are building a logistic regression model for the data by using the glm() model.
logisticRegression_LendingData = glm(loan_default ~ pct_loan_income+loan_amnt+residence_property+inq_last_6mths+dti , data = training_data_lendingData,
                             family = "binomial")

#Summary of the logistic regression.
summary(logisticRegression_LendingData)


#Measuring the probability using the testing data.

prob_estimate_testData = predict(logisticRegression_LendingData,newdata =testing_data_lendingData,type = "terms")

#It gives the information of the testing data when the response is predicted.
testing_data_lendingData = testing_data_lendingData %>% 
  mutate(EstimatedProb = predict(logisticRegression_LendingData,
                                 newdata = testing_data_lendingData, type = "response"))

#Prediction summary of the test data.
summary(testing_data_lendingData$EstimatedProb)

#printing number of rows in the testing data.
nrow(testing_data_lendingData)

# Predicting Y by setting a threshold value of 0.5 and taking all the values which are greater than 0.5

testing_second <- testing_data_lendingData %>% mutate(LogitPredicited = I(EstimatedProb > 0.5) %>% as.numeric())
lendingDataset <-table(testing_second$LogitPredicited ,testing_second$loan_default)

#printing the confusion matrix
lendingDataset
confusionMatrix(lendingDataset)


###### plotting the ROC curve for the above classification ################################

ROC_ref<-prediction(as.numeric(testing_second$LogitPredicited),as.numeric(testing_data_lendingData$loan_default))
ROC_statistic<-performance(ROC_ref,measure = "tpr",x.measure = "fpr")
par(mar=c(4,4,2,1))
plot(ROC_statistic,col="green")
abline(a=0,b=1)
AUC_curve<-performance(ROC_ref,measure="auc")
AUC_output<-AUC_curve@y.values[[1]]
AUC_output

###########################Naive Bayes Model for the Lending Dataset###########################################

# Building the Naive Bayes model for the Lending Dataset.

NaiveBayes_model_LendingData <- naiveBayes(loan_default ~ pct_loan_income+loan_amnt+residence_property+inq_last_6mths+dti, data = training_data_lendingData)

#Making predictions on the test data set for the Naive Bayes model

predictions_naiveBayes <- predict(NaiveBayes_model_LendingData, newdata = testing_data_lendingData)
confusion_matrix_lendingData <- table(predictions_naiveBayes, testing_data_lendingData$loan_default)


#creating and printing the confusion matrix........................

confusion_matrix_lendingData
confusionMatrix(confusion_matrix_lendingData)

###### plotting the ROC curve for the above classification ################################

ROC_ref<-prediction(as.numeric(predictions_naiveBayes),as.numeric(testing_data_lendingData$loan_default))
ROC_statistic<-performance(ROC_ref,measure = "tpr",x.measure = "fpr")
par(mar=c(4,4,2,1))
plot(ROC_statistic,col="red")
abline(a=0,b=1)
AUC_curve<-performance(ROC_ref,measure="auc")
AUC_output<-AUC_curve@y.values[[1]]
AUC_output
