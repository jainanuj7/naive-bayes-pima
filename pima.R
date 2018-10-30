#This R script applies Naive Bayes Classifer on the Pima Indian Diabetes dataset
#You can apply the same on any dataset, make sure you change the `Outcome` column in the script (the class column) as required

#Clear current environment
rm(list=ls())

#Import the pima.csv dataset using entire path
#Make sure you use / and NOT \ for windows
#Alternatively you can also use "Import Dataset > From Text(base)" in RStudio
pima <- read.csv("C:/Users/.../pima.csv", header = TRUE) 

#Install the package and include it
#install.packages("e1071")
library("e1071")

#Display Pima Indian Diabetes Dataset
View(pima)

#90% training data, 10% test data
smp_size <- floor(0.90 * nrow(pima))

#nrow(pima) will return number of rows in pima dataset i.e. 768
#seq_len(768) returns a vector from 1:768
#sample(1:768, size) will pick 90% of rows from pima dataset
train_ind <- sample(seq_len(nrow(pima)), size = smp_size)

#prepare training data
train <- pima[train_ind, ]

#prepare testing data, that is pima minus traindata
test <- pima[-train_ind, ]

#If outcome is in form of yes/no, convert it into form of integral factors like 0,1
train$Outcome <- as.factor(train$Outcome)
test$Outcome <- as.factor(test$Outcome)

#Apply Naive bayes, train the model
#`Outcome ~ .` means Outcome is a function of all the other attributes in the dataset
#You can also select specific 
nb <- naiveBayes(Outcome~., data = train)

#Apply the trained model on test data
Predictions<-predict(nb, test)

#Prepare confusion matrix
ConfusionMatrix<-table(test$Outcome,Predictions)

#Display confusion matrix
ConfusionMatrix

#Calucate accuracy of model
#NOTE: this will always come out to be different since sample is randomly picked
#declare variable Accuracy_in_percentage
Accuracy_in_percentage<-numeric(10)
Accuracy_in_percentage <- sum(diag(ConfusionMatrix))/sum(ConfusionMatrix)*100

#Display Accuracy in percentage
Accuracy_in_percentage
