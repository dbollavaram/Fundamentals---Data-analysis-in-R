# Read in the dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.7)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE) 

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial) #using all variables to build logistics regression model
summary(framinghamLog)

#predicting on test data 
predTest = predict(framinghamLog, newdata=test, type="response")
table(test$TenYearCHD, predTest > 0.1)

#accuracy
(430+136)/(430+136+500+31)
#true positive rate
136/(136+31) 
#false positive rate 
500/(500+430) 


#baseline model 
#accuracy of baseline on test set 
table(test$TenYearCHD)
(930)/(930+167) #84.78% is accuracy 

# Prediction for the example new observation
pred.obs <- data.frame(male=1, age= 55, education='College', currentSmoker=1, cigsPerDay=10, BPMeds=0, prevalentStroke = 0, prevalentHyp = 1, diabetes = 0,totChol = 220,sysBP = 140, diaBP = 100, BMI = 30, heartRate = 60, glucose = 80  )
predict(framinghamLog, newdata=pred.obs, type="response") #0.2225573 

#roc-auc calculations 
predtest <- predict(framinghamLog, newdata=test, type="response")
rocr.test.pred <- prediction(predtest, test$TenYearCHD)
testPerformance <- performance(rocr.test.pred, "tpr", "fpr")
plot(testPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.test.pred, "auc")@y.values)


#(31 + 136)*250000/(1097) 
#(500*15000 + 136* 265000 + 430*0 + 31*250000)/(1097) 














