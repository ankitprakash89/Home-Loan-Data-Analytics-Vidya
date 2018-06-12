library(dplyr)
library(zoo)
library(caret)
library(randomForest)

# Loading the training and test data 
loan_train <- read.csv("C:/Users/Administrator/Downloads/Loan Analytics Vidya/train.csv", stringsAsFactors = FALSE )
loan_test <- read.csv("C:/Users/Administrator/Downloads/Loan Analytics Vidya/test.csv")

# Creating a New variable by the name of Marker in both the data set 
# It will help in dividing the full date set combining them for cleanon
loan_train$Marker <- TRUE
loan_test$Marker <- FALSE

# Adding Column in loan_test as it is not available 
loan_test$Loan_Status <- NA

# Combining both train and test dataset columwise
loan_full <- rbind(loan_train, loan_test)

# To replace all empty data with NA
loan_full[loan_full == ""] <- NA

# Imputing all missing value in the dataframe
# na.locf return list so to convert list into dataframe
loan_full <- as.data.frame(lapply(loan_full, na.locf0))

# To imputing missing data in LoanAmount 
loan_full$LoanAmount <- ifelse(is.na(loan_full$LoanAmount)==TRUE, 
                              mean(loan_full$LoanAmount,na.rm = TRUE),
                              loan_full$LoanAmount) 

loan_train <- loan_full[loan_full$Marker==TRUE,]
loan_new <- select(loan_train, - Marker) 

loan_test <- loan_full[loan_full$Marker==FALSE,]
test <- select(loan_test, -Marker)

# Converting Loan_status into numerical variable
loan_new$Loan_Status <- ifelse(loan_new$Loan_Status=="N",0,1)

# Splitting loan data into training and tesing dataframes in 70-30 percent
loan_train = loan_new[sample(seq(1,nrow(loan_new)),(0.7*nrow(loan_new))),]
loan_test = loan_new[sample(seq(1,nrow(loan_new)),(0.3*nrow(loan_new))),]

# Getting mtry value for randomForest model
mtry = round(sqrt(length(colnames(loan_train))-1))

# Creating model for traing data usning random forest 
model_rf = randomForest(Loan_Status~.,data=select(loan_train,-Loan_ID), ntree=400,mtry=mtry)

# Predicting Values for the test dataset from traing dataset
loan_test$predicted = predict(model_rf,loan_test)
loan_test$pred <- ifelse(loan_test$predicted>=0.5,1,0)


##Confusion Matrix for the random forest model
cm = confusionMatrix(loan_test$pred, loan_test$Loan_Status,positive='1')
cm

##Calculating accuracy of the random forest model
accuracy_rf = cm$overall[["Accuracy"]]*100
accuracy_rf

#To get the sensitivity of the random forest model
Sensitivity_rf <- cm$byClass[["Sensitivity"]]*100
Sensitivity_rf

# Loading the test data
summary(test)
test$predicted = predict(model_rf,test)
test$Loan_Status<- ifelse(test$predicted>=0.5,"Y","N")

df <- select(test, c(Loan_ID,Loan_Status))
write.csv(df, "C:/Users/Administrator/Downloads/Loan Analytics Vidya/sample_submission.csv", row.names = FALSE)
