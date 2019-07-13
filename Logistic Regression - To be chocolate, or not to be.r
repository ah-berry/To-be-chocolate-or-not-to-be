
#Insert your working directory path as an argument of the setwd function
setwd()

# Import the data file "candy-data" into R
candy <- read.csv()

#Viewing the structure of the dataset
str(candy)
head(candy)

#Changing the values for chocolate from data type 'int' to 'Factor'
candy$chocolate <- as.factor(candy$chocolate)
str(candy)

#Viewing the summary statistics of the candy dataset
summary(candy)

#Creating the training and testing data
set.seed(100)
ind <- sample(2, nrow(candy), replace = T, prob = c(0.8,0.2))
train <- candy[ind==1,]
test <- candy[ind==2,]

#Creating a logistic regression model on the training set
chocolate_logistic_model <- glm(chocolate ~  pricepercent + winpercent, 
                                 data = train, family = 'binomial')
summary(chocolate_logistic_model)

#Making predictions on the testing set and setting a prediction cutoff
predictTest <- predict(chocolate_logistic_model, newdata=test, type = "response")
prediction_cutoff <- ifelse(predictTest>0.5,1,0)

#Creating the confusion matrix
confusion_matrix <- table(Predicted = prediction_cutoff, Actual = test$chocolate)
confusion_matrix 

#Determining the accuracy of our model as a percentage
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
accuracy
