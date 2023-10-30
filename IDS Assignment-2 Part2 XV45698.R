#                                                       IDS Assignment 2 
#                                                         PART - 2

# Importing the libraries which are required.


library(ggplot2)
library(tidyverse)
library(readxl)
library(arules)
library(arulesViz)
library(knitr)
library(plyr)
library(dplyr)


#retrieving and storing the data from Breadbasket.csv file.

BreadBasketCsv_data = read.csv("C:/Users/hp/OneDrive/Desktop/IDS SPRING 2023 Assignments/A-2/BreadBasket_DMS.csv")

#Number of rows in the data set information.
nrow(BreadBasketCsv_data)

#Here we are pre processing the data and we are removing if any missed values present in the given data set.
distinct_items = unique(BreadBasketCsv_data$Item)
print(distinct_items)
BreadBasketCsv_data <- subset(BreadBasketCsv_data,BreadBasketCsv_data$Item != "NONE")
BreadBasketCsv_data <- BreadBasketCsv_data[complete.cases(BreadBasketCsv_data), ]
BreadBasketCsv_data$Item = as.factor(BreadBasketCsv_data$Item)

# Here we are extracting the attribute time from the given dataset and converting the type of the transactions to numeric type.
BreadBasketCsv_data$Time<- BreadBasketCsv_data$Time
BreadBasketCsv_data$Transaction <- as.numeric(as.character(BreadBasketCsv_data$Transaction))
glimpse(BreadBasketCsv_data)
BreadBasket_transaction_Analysis <- ddply(BreadBasketCsv_data,c("Transaction","Date"),
                                    function(df1)paste(df1$Item, collapse = ","))

#Here we are setting the column Invoice No,Date of data frame transactions and renaming the column.
BreadBasket_transaction_Analysis$Transaction <- NULL
BreadBasket_transaction_Analysis$Date <- NULL
colnames(BreadBasket_transaction_Analysis) <- c("products")

#here we are displaying the data frame information.
BreadBasket_transaction_Analysis

#Here we are Writing the data to the new CSV file.

write.csv(BreadBasket_transaction_Analysis,"BasketAnalysis_transactions.csv", quote = FALSE, row.names = FALSE)
read_transactions_BreadBasket <- read.transactions("BasketAnalysis_transactions.csv", format = 'basket', sep=',')

#here it gives the summary of the read transactions.

summary(read_transactions_BreadBasket)

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}


par(mar=c(1,1,1,1))

#Here we are performing the plot on the read transactions.
itemFrequencyPlot(read_transactions_BreadBasket,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Frequency Plot for top 20 Items")


#Here we are applying the association rules where lhs = "Coffee" and inspecting the results of apriori algorithm.
rules_association<-apriori(read_transactions_BreadBasket,parameter=list(supp=0.001,conf=0.4,maxlen=5))
rules_association.top<- sort(rules_association, by = 'support', decreasing = TRUE)
summary(rules_association)
inspect(rules_association)

#Top 5 Rules
#Here we are applying the association rules where lhs = "Coffee" and inspecting the results of apriori algorithm.
rules_association<-apriori(read_transactions_BreadBasket,parameter=list(supp=0.001,conf=0.4,maxlen=5),appearance = list(none=(rhs="Coffee")))
SubRulesNoCoffee<- sort(SubRulesNoCoffee, by = 'support', decreasing = TRUE)
summary(rules_association)
inspect(rules_association[1:5])

#Filtering rules such that we don't have coffee in the rhs
SubRulesNoCoffee = subset(rules_association, subset = !(rhs %in% "Coffee"))
summary(SubRulesNoCoffee)
inspect(SubRulesNoCoffee)

