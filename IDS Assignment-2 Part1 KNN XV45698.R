#                                                       IDS Assignment 2 
#                                                         PART - 1
#                                                           KNN
########################### Installing all the necessary packages#################################
library(psych)
library(dplyr) 
library(e1071)
library(ggplot2) 
library(caret)
library(tidyverse)
library("ROCR")
library(kknn)

#Retrieving and storing the data from Lending.csv file.

lending_Data <- read.csv("C:/Users/hp/OneDrive/Desktop/IDS SPRING 2023 Assignments/A-2/Lending.csv")


#Converting the data into numeric form.

lending_Data = lending_Data %>% mutate(residence_property = I(residence_property == "Own") %>% as.numeric())



#Dividing the data into 2 groups one group in which the loan_default==1 and the other in which loan_default==0 and we are binding the 2 groups using a rbind function..

lending_Data= rbind(sample_n(filter(lending_Data, loan_default==1),11000), sample_n(filter(lending_Data,loan_default==0), 11000))
lending_Data$loan_default = factor(lending_Data$loan_default)

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
###########################KNN Model for the Lending Dataset###########################################

#finding number of rows in the training data set.
nrow(training_data_lendingData)

install.packages('class')
library(class)

#Creating separate data frame for 'loan default' feature which is our target.
train_labels <- training_data_lendingData[,1]
test_labels <-testing_data_lendingData[,1]

#Finding number of observations
NROW(train_labels)

#Square root of 15399 is 124.29, hence creating two models with k as 124,125
KNN_Pred124 <-knn(train=training_data_lendingData[,-1],test=testing_data_lendingData[,-1],cl=train_labels,k=124)
KNN_Pred125 <-knn(train=training_data_lendingData[,-1],test=testing_data_lendingData[,-1],cl=train_labels,k=125)

#Calculate the accuracy of created models
acc.124 <- 100 * sum(test_labels == KNN_Pred124)/NROW(test_labels)
acc.125 <- 100 * sum(test_labels == KNN_Pred125)/NROW(test_labels)

acc.124
acc.125

#creating and printing the confusion matrix.
confusion_matrix_KNN = table(KNN_Pred124 ,test_labels)
confusionMatrix(confusion_matrix_KNN)

###### plotting the ROC curve for the above classification for K = 124 model################################

ROC_ref<-prediction(as.numeric(KNN_Pred124),as.numeric(testing_data_lendingData$loan_default))
ROC_statistic<-performance(ROC_ref,measure = "tpr",x.measure = "fpr")
par(mar=c(4,4,2,1))
plot(ROC_statistic,col="blue")
abline(a=0,b=1)
AUC_curve<-performance(ROC_ref,measure="auc")
AUC_output<-AUC_curve@y.values[[1]]
AUC_output
