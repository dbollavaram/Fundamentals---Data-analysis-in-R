install.packages("e1071")
library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(e1071)

# Reading and splitting data
loans <- read.csv("C:/Users/deeps/App-data_R/loans.csv")
str(loans)
loans$not.fully.paid = as.factor(loans$not.fully.paid)
# Setting a seed
set.seed(144)

# splitting the dataset into the training and testing datasets
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)

loans.train <- filter(loans, split == TRUE) # is split a variable in loans?
loans.test <- filter(loans, split == FALSE)

summary(loans.train)
str(loans.train) #6661 
str(loans.test) #2855 
loss.mat <- cbind(c(0, 4000), c(1000, 0)) # cbind is column bind, rbind is row bind

# adding loss function to a list of "parms"
mod2 = rpart(not.fully.paid ~ .,data = loans.train, method="class", parms=list(loss = loss.mat), cp = 0.003)
prp(mod2)
# Make predictions 
pred <- predict(mod2, newdata = loans.test, type = "class")
table(loans.test$not.fully.paid, pred)

#cpVals = data.frame(cp = seq(0, .06, by=.002))

#train.cart <- train(not.fully.paid ~ installment + log.annual.inc + fico + revol.bal + inq.last.6mths + pub.rec,data = loans.train, method = "rpart",tuneGrid = cpVals,trControl = trainControl(method = "cv", number=10), metric = "Accuracy")
#train.cart$results
#train.cart

# plot the results
#ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point()
# We can increase the size of the points:
#ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3)
# We can change the default axis labels
#ggplot(train.cart$results, aes(x=cp, y=Accuracy)) + geom_point(size=3) +
#  xlab("Complexity Parameter (cp)") + geom_line()


mod3 = rpart(not.fully.paid ~ .,data = loans.train, method="class", parms=list(loss = loss.mat), cp = 0.004)
prp(mod3)
# Make predictions 
pred <- predict(mod3, newdata = loans.test, type = "class")
table(loans.test$not.fully.paid, pred)

mod4 = rpart(not.fully.paid ~ .,data = loans.train, method="class", parms=list(loss = loss.mat), cp = 0.005)
prp(mod4)
# Make predictions 
pred <- predict(mod4, newdata = loans.test, type = "class")
table(loans.test$not.fully.paid, pred)

# Extract the best model and make predictions
#train.cart$bestTune
#mod123 = train.cart$finalModel
#prp(mod123, digits=3)

#making predictions 
#loans.test.mm = as.data.frame(model.matrix(not.fully.paid~.+0, data=loans.test))
#pred = predict(mod123, newdata=loans.test.mm, type="class")
#table(loans.test$not.fully.paid, pred)
1754*1000-249*4000
1780*1000-257*4000
1756*1000-249*4000
(1756+207)/(1756 + 207+ 249+643)
(1756*1000-249*4000)/ (1756+249) 
249/(1756+249)
#TPR 
207/(207+249)
(643)/(643+1756)

##############################################################    bike ###############################################3

install.packages("caret")
library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(car)
library(rpart) # CART
library(rpart.plot) # CART plotting
library(caret) # cross validation

bike = read.csv('C:/Users/deeps/App-data_R/bikeshare.csv')
str(bike)
summary(bike)
#ggscatmat(bike, columns = 2:5, alpha = 0.8) # any correlations?
plot(bike$rentals)
boxplot(bike$rentals)
boxplot(bike$rentals~bike$season)#
boxplot(bike$rentals~bike$holiday)
boxplot(bike$rentals~bike$workingday)#
boxplot(bike$rentals~bike$hour)
boxplot(bike$rentals~bike$dayofweek)
boxplot(bike$rentals~bike$weather.intensity)#
plot(bike$windspeed, bike$rentals)
plot(bike$temp, bike$rentals)
plot(bike$humidity, bike$rentals)
plot(bike$temp.feel, bike$rentals)
#bike$year = as.factor(bike$year)
bike.train1 = subset(bike, year == 2011)
str(bike.train1) #8645 observations 
summary(bike.train1)
bike.test1 = subset(bike, year == 2012)
str(bike.test1) #8734 observations 
summary(bike.test1)
bike.train2 = subset(bike, year == 2011 | (year == 2012 & (month == 'January' | month == 'February' | month == 'March' | month == 'April' | month == 'May' | month == 'June')))
bike.test2 = subset(bike, year == 2012 & (month == 'July' | month == 'August' | month == 'September' | month == 'October' | month == 'November' | month == 'December'))
str(bike.train2) #13003 
str(bike.test2) #4376 
summary(bike.train2)
summary(bike.test2)
13003/(13003+4376)
8645/(8645+8734)

bike.train2.t = subset(bike, year == 2011 | (year == 2012 & (month == 'January' | month == 'February' | month == 'March')))
bike.train2.v = subset(bike, (year == 2012 & (month == 'April' | month == 'May' | month == 'June')))

cor(bike.train2.t$temp,bike.train2.t$temp.feel) #0.99 
x = bike[c('hour','holiday','workingday','temp','humidity','windspeed','temp.feel')]
cor(x) #more correlation between temp and temp.feel 

mod1 <- lm(rentals ~ year + season + month + dayofweek + hour + holiday + workingday + weather.intensity + temp + temp.feel + humidity + windspeed , data = bike.train2.t)
summary(mod1) #0.3818, 0.38 
vif(mod1)
mod2 <- lm(rentals ~ year + season + month + dayofweek + hour + holiday + workingday + temp + temp.feel + humidity + windspeed , data = bike.train2.t)
summary(mod2) #removed weather intensity #0.3789, 0.3773 
mod3 <- lm(rentals ~ year + season + month + dayofweek + hour + holiday + workingday + weather.intensity + temp + temp.feel + humidity, data = bike.train2.t)
summary(mod3) #removed windspeed from mod1 0.3817,0.3801 
mod4 <- lm(rentals ~ year + season + month + hour + holiday + workingday + weather.intensity + temp + temp.feel + humidity, data = bike.train2.t)
summary(mod4) #removed dayofweek from mod3 #0.3813, 0.3799  
mod5 <- lm(rentals ~ year + season + month + dayofweek + hour + holiday + weather.intensity + temp + temp.feel + humidity, data = bike.train2.t)
summary(mod5) #removed workingday from mod3, #0.3817, 0.3801
mod6 <- lm(rentals ~ year + season + month  + hour + holiday + weather.intensity + temp + dayofweek + humidity, data = bike.train2.t)
summary(mod6) #removed temp.feel from mod5, #0.3814, 0.3798
mod7 <- lm(rentals ~ year + season + month + hour + holiday  + dayofweek + weather.intensity + temp + humidity + windspeed , data = bike.train2.t)
summary(mod7) #removed temp.feel from mod6 , 0.3814, 0.3798 
#mod5 is selected
mod8 <- lm(rentals ~ year + season + month + hour + holiday + weather.intensity + temp + humidity+dayofweek, data = bike.train2.t)
summary(mod8) #removed windspeed, temp.feel, workingday #0.3814, 0.3798 #first removed temp.feel as highly correlated(0.3814, 0.3798), then removed workingday
#no change #then removed windspeed and still no change #then if dayofweek is removed (0.3809, 0.3797)-mod9 
mod9 <- lm(rentals ~ year + season + month + hour + holiday + weather.intensity + temp + humidity, data = bike.train2.t)
summary(mod9) 

finalmod2 <- lm(rentals ~ year + season + month + dayofweek + hour + holiday + workingday + weather.intensity + temp + humidity + windspeed , data = bike.train2.t)
summary(finalmod2)
finalmod3 <- lm(rentals ~ year + season + month + dayofweek + hour + holiday + weather.intensity + temp + humidity + windspeed , data = bike.train2.t)
summary(finalmod3)
vif(finalmod3)
finalmod4 <- lm(rentals ~ year + season + month + dayofweek + hour + holiday + weather.intensity + temp + humidity , data = bike.train2.t)
summary(finalmod4)
vif(finalmod4)
finalmod5 <- lm(rentals ~ year + month + hour + holiday +weather.intensity  + temp + humidity , data = bike.train2.t)
summary(finalmod5)  #0.3775, 0.3764
finalmod6 <- lm(rentals ~ year + season + hour + holiday +weather.intensity  + temp + humidity , data = bike.train2.t)
summary(finalmod6)  #0.3725, 0.3719

#predicting on validation set 
Predictions1 <- predict(mod1, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions1)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4590721

Predictions2 <- predict(mod2, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions2)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4559328

Predictions3 <- predict(mod3, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions3)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4589495

Predictions4 <- predict(mod4, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions4)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4589717

Predictions5 <- predict(mod5, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions5)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4589495 

Predictions6 <- predict(mod6, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions6)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4590655 

Predictions7 <- predict(mod7, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions7)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.458972 

Predictions8 <- predict(mod8, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions8)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4590655

Predictions9 <- predict(mod9, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions9)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4590679  - highest 

Predictions_finalmod2 <- predict(finalmod2, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions_finalmod2)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.458972

Predictions_finalmod3 <- predict(finalmod3, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions_finalmod3)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.458972

Predictions_finalmod4 <- predict(finalmod4, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions_finalmod4)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4590655

Predictions_finalmod5 <- predict(finalmod5, newdata=bike.train2.v)
SSE = sum((bike.train2.v$rentals - Predictions_finalmod5)^2)
SST = sum((bike.train2.v$rentals - mean(bike.train2.t$rentals))^2)
OSR2 = 1 - SSE/SST #0.4590679  #0.4598424 


cart.mod <- rpart(rentals ~ year + season + month + dayofweek + hour + holiday + workingday + weather.intensity + temp + temp.feel + humidity + windspeed, data = bike.train2, method = "anova", cp = 0.02)
prp(cart.mod)
cpVals = data.frame(cp = seq(0, .06, by=.002))

train.cart <- train(rentals ~  year + season + month + dayofweek + hour + holiday + workingday + weather.intensity + temp + temp.feel + humidity + windspeed,data = bike.train2,method = "rpart",tuneGrid = cpVals,trControl = trainControl(method = "cv", number=10),metric = "logLoss")

# look at the cross validation results, stored as a data-frame
train.cart$results 
train.cart #cp = 0 
cart.mod.best <- rpart(rentals ~year + season + month + dayofweek + hour + holiday + workingday + weather.intensity + temp + temp.feel + humidity + windspeed, data = bike.train2, method = "anova", cp = 0.001)
prp(cart.mod.best) 

#for linear regression model
finalmod5 <- lm(rentals ~ year + month + hour + weather.intensity + holiday + temp + humidity , data = bike.train2)
summary(finalmod5) ##0.4046 0.4037 

Predictions <- predict(finalmod5, newdata=bike.test2)
SSE = sum((bike.test2$rentals - Predictions)^2)
SST = sum((bike.test2$rentals - mean(bike.train2$rentals))^2)
OSR2 = 1 - SSE/SST #0.4101661  #0.4041805 (month)


# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}
error_linreg = bike.test2$rentals - Predictions
rmse(error_linreg) #182.6778
mae(error_linreg) #137.9957

#for CART model 
CartPredictions <- predict(cart.mod.best, newdata=bike.test2)
SSE = sum((bike.test2$rentals - CartPredictions)^2)
SST = sum((bike.test2$rentals - mean(bike.train2$rentals))^2)
OSR2 = 1 - SSE/SST   #0.8012

error_cart = bike.test2$rentals - CartPredictions
rmse(error_cart) #105.27
mae(error_cart) #74.13 



