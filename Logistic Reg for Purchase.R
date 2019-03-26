# Logistic Regression : Predict Purchase
# Import the mydata - male,age,salary given and binary classification of purchase or not
url="https://docs.google.com/spreadsheets/d/1Md_ro2t3M7nA9JMH1DsE12jfeX7qq-UPw6p8WQd6A2Y/edit#gid=120271978"
library(gsheet)
mydata = as.data.frame(gsheet2tbl(url))
View(mydata)
mydata$gender = factor(mydata$gender)
# Split the mydata into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(2000)
split = sample.split(mydata$purchased, SplitRatio = 0.75)
training_set = subset(mydata, split == TRUE) 
test_set = subset(mydata, split == FALSE)
#For above, instead of True-Plus, could have gone for - usage
dim(mydata)
dim(training_set)
dim(test_set)
names(mydata)
# Logisitic Model on Training Set
logM1 = glm(purchased ~ gender + age + salary, family = binomial,  data = training_set)
summary(logM1)
# gender not insignificant so dropped here as high Pr and insignificant number of stars
logM2 = glm(purchased ~ age + salary, family = binomial, data = training_set)
summary(logM2)

head(training_set)
#predict on sample data with logM1
trial = data.frame(age=c(40,65,30,80,25), gender=c('Male', 'Female','Female','Female','Male'), salary=c(40000, 50000,35000,60000,80000))
trial
prob_pred1 = predict(logM1, type = 'response', newdata = trial)
cbind(trial, prob_pred1)
#predict on sample data with logM2
prob_pred2 = predict(logM2, type = 'response', newdata = trial)
cbind(trial,prob_pred2)

# Predicting the class of test test from model 1 and model 2
head(test_set)
prob_pred1 = predict(logM1, type = 'response', newdata = test_set)
summary(prob_pred) 
head(cbind(test_set,prob_pred1 ),10)
prob_pred2=predict(logM2,type='response',newdata=test_set)
head(cbind(test_set,prob_pred2),10)

#We need either 1 and 0 binary values
#if prob > 0.5 make it 1, else 0
y_pred1 = ifelse(prob_pred1 > 0.5, 1, 0)
y_pred2 = ifelse(prob_pred2>0.2,1,0)
head(cbind(test_set$purchased, y_pred1),100)
head(cbind(test_set$purchased, y_pred2),100)

# Making the Confusion Matrix
cm1 = table(test_set[,5], y_pred1)
cm1
cm2=table(test_set[,5],y_pred2)
cm2
#Compares the 5th row of test set (0/1) with that of prediction
library(caret)
confusionMatrix(cm1)
confusionMatrix(cm2)
#caret library gives the sensitivity and specificity of confusion matrix
#Model 2 gives a greater sensitivity but a lower specificity
