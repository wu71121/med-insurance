## Read data 
data = read.csv("insurance.csv")
data = read.csv('/Users/wuweihan/Desktop/Wu\'s Folder/MSIE/Spring 22/Regression/Project/insurance.csv')
obs = nrow(data)
colnames(data) = c("region", "sex", "children", "smoker", "bmi", "age", "charges")

## Attach data to automatically recognize the columns in the data as individual vectors
attach(data)


##### EXPLORATORY DATA ANALYSIS

## Check the distribution of the response (charges)
hist(data$charges, main="", xlab="Charges", col = "darkblue", border = "orange")

## Evaluate relationship between qualitative predicting variables and charges
# Check the response (charges) against region and region's distribution
barplot(summary(data$region), main = "Histogram of Region", xlab = "Region", col = blues9)
boxplot(charges~region, main="", xlab="Region",ylab="Charges",col=blues9,data=data)
# Check the response (charges) against smoker status and smoking status distribution
barplot(summary(data$smoker), main = "Histogram of Smokers/Non-Smokers", xlab = "Smoking Status", col = blues9)
boxplot(charges~smoker, main="", xlab="Smoker/Non-Smoker",ylab="Charges",col=blues9,data=data)
# Check the response (charges) against sex and sex distribution
barplot(summary(data$sex), main = "Histogram of Sex", xlab = "Sex", col = blues9)
boxplot(charges~sex, main="", xlab="Sex",ylab="Charges",col=blues9,data=data)
# Check the response (charges) against number of children and children distribution
barplot(summary(data$children), main = "Histogram of Number of Children", xlab = "Number of Children", col = blues9)
boxplot(charges~children, main="", xlab="Number of Children",ylab="Charges",col=blues9,data=data)


## Evaluate quantitative predicting variables and the relationship between these variables and charges
# age
hist(age, main = "Histogram of Age", xlab = "Age", col = blues9)
plot(data$age, data$charges, xlab='Age', ylab='Charges', main="",col=blues9)
abline(lm(charges ~ age, data=data), lty=2, lwd=2)

#bmi
hist(bmi, main = "Histogram of Body Mass Index", xlab = "BMI", col = blues9)
plot(data$bmi, data$charges, xlab='BMI', ylab='Charges', main="",col=blues9)
abline(lm(charges ~ bmi, data=data), lty=2, lwd=2)

# Scatter plot matrix of charges and numeric predicting variables 
plot(data[,5:6], col=blues9)

## Explore the correlation between predicting variables
#age and bmi
cor(data[,5:6])
#qualitative predictors using Polychoric Correlation
install.packages("polycor")
library(polycor)
polychor(smoker, region)
polychor(smoker, sex)
polychor(smoker, children)
polychor(region, sex)
polychor(region, children)
polychor(sex, children)
##qualitative predictors using Cramer’s V
install.packages("rcompanion")
library(rcompanion)
cramerV(smoker, region)
cramerV(smoker, sex)
cramerV(smoker, children)
cramerV(region, sex)
cramerV(region, children)
cramerV(sex, children)

##### PREPARING THE DATA

## Converting the numerical categorical variables to predictors
data$sex = as.factor(data$sex)
data$smoker = as.factor(data$smoker)
data$region = as.factor(data$region)
data$children = as.factor(data$children)

## Divide data into train and test data
# Set seed for reproducibility
set.seed(9)
# Test Train split
sample_size = floor(0.8*nrow(data))
picked = sample(seq_len(nrow(data)),size = sample_size)
train = data[picked,]
n = nrow(train)


#### MODEL 0. Multiple linear regression

## Applying multiple linear regression model
model0 = lm(charges ~ region + sex + children + smoker + bmi + age, data=train)
summary(model0)

## Checking asumptions and fitting the model
library(car)
resids0 = rstandard(model0)
fits0 = model0$fitted
cook0 = cooks.distance(model0)
par(mfrow = c(2,2))

##Variance inflation factors for the quantitative predicting variables(check for multicollinearity)
vif(model0)

# Scatter plot of residuals vs fitted values (constant variance, independence)
plot(fits0, resids0, xlab="Fitted Values", ylab="Residuals", main="Scatterplot", col=blues9)
abline(2,0, col = "red")
abline(-2,0, col = "red")

# Scatter plot of residuals vs numeric predictors (linearity)
plot(train[,5], resids0, xlab="BMI", ylab="Residuals",col=blues9)
plot(train[,6], resids0, xlab='Age', ylab='Residuals', main="",col=blues9)

# QQ normal plot & histogram (normality)
qqPlot(resids0, ylab="Residuals", main = "")
hist(resids0, xlab="Residuals", main = "", nclass=10, col=blues9)

#Checking for outliers
plot(cook0,type="h",lwd=3,col=blues9, ylab = "Cook's Distance")
summary(cooks.distance(model0))
n = nrow(train)
sum(cook0>4/n)
train[which(cooks.distance(model0)>4/n), ]

## Box Cox transformation to transform y's (attempt to fix not constant variance/non-normality)
bc = boxCox(model0)
lambda = bc$x[which(bc$y==max(bc$y))]
lambda

#### MODEL 1. Multiple linear regression with sq.root transformation

## Applying multiple linear regression model
model1 = lm(sqrt(charges) ~ region + sex + children + smoker + bmi + age, data=train)
summary(model1)

## Checking asumptions and fitting the model
resids1 = rstandard(model1)
fits1 = model1$fitted
cook1 = cooks.distance(model1)
par(mfrow = c(2,2))

##Variance inflation factors for the quantitative predicting variables(check for multicollinearity)
vif(model1)

# Scatter plot of residuals vs fitted values (constant variance, independence)
plot(fits1, resids1, xlab="Fitted Values", ylab="Residuals", main="Scatterplot", col=blues9)
abline(2,0, col = "red")
abline(-2,0, col = "red")

# Scatter plot of residuals vs numeric predictors (linearity)
plot(train[,5], resids1, xlab="BMI", ylab="Residuals",col=blues9)
plot(train[,6], resids1, xlab='Age', ylab='Residuals', main="",col="blue")

# QQ normal plot & histogram (normality)
qqPlot(resids1, ylab="Residuals", main = "")
hist(resids1, xlab="Residuals", main = "", nclass=10, col=blues9)

#Checking for outliers
plot(cook1,type="h",lwd=3,col=blues9, ylab = "Cook's Distance")
summary(cooks.distance(model1))
n = nrow(train)
sum(cook1>4/n)
train[which(cooks.distance(model1)>4/n), ]

##Remove outliers from train data
train1 = train[-which(cooks.distance(model1)>4/n),]
n1 = nrow(train1)


#### MODEL 2. Model without outliers and with transform

## Applying multiple linear regression model
model2 = lm(sqrt(charges) ~ region + sex + children + smoker + bmi + age, data=train1)
summary(model2)

## Checking asumptions and fitting the model
resids2 = rstandard(model2)
fits2 = model2$fitted
cook2 = cooks.distance(model2)
par(mfrow = c(2,2))

# Scatter plot of residuals vs fitted values (constant variance, independence)
plot(fits2, resids2, xlab="Fitted Values", ylab="Residuals", main="Scatterplot", col=blues9)
abline(2,0, col = "red")
abline(-2,0, col = "red")

# Scatter plot of residuals vs numeric predictors (linearity)
plot(train1[,5], resids2, xlab="BMI", ylab="Residuals",col=blues9)
plot(train1[,6], resids2, xlab='Age', ylab='Residuals', main="",col=blues9)

# QQ normal plot & histogram (normality)
qqPlot(resids2, ylab="Residuals", main = "")
hist(resids2, xlab="Residuals", main = "", nclass=10, col=blues9)

#Checking for outliers
plot(cook2,type="h",lwd=3,col=blues9, ylab = "Cook's Distance")
summary(cooks.distance(model2))
n1 = nrow(train1)
sum(cook2>4/n1)
train1[which(cooks.distance(model2)>4/n1), ]

#### MODEL 3. Weighted least squares regression
#Perform weighted least squares regression
model = lm(charges ~ region + sex + children + smoker + bmi + age, data=train)
wt = 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
model3=lm(1/sqrt(charges) ~ region + sex + children + smoker + bmi + age, data=train, weights=wt)
bc = boxCox(model3)
lambda = bc$x[which(bc$y==max(bc$y))]
lambda

##Checking asumptions and fitting the model
resids3 = rstandard(model3)
fits3 = model3$fitted
cook3 = cooks.distance(model3)
par(mfrow = c(2,2))

# Scatter plot of residuals vs fitted values (constant variance, independence)
plot(fits3, resids3, xlab="Fitted Values", ylab="Residuals", main="Scatterplot", col=blues9)

# Scatter plot of residuals vs numeric predictors (linearity)
plot(train[,5], resids3, xlab="BMI", ylab="Residuals",col=blues9)
abline(0,0, col = "red")
plot(train[,6], resids3, xlab='Age', ylab='Residuals', main="",col=blues9)
abline(0,0, col = "red")

# QQ normal plot & histogram (normality)
qqPlot(resids3, ylab="Residuals", main = "")
hist(resids3, xlab="Residuals", main = "", nclass=10, col=blues9)

#Checking for outliers
plot(cook3,type="h",lwd=3,col=blues9, ylab = "Cook's Distance")
summary(cooks.distance(model3))
n = nrow(train)
sum(cook3>4/n)
train[which(cooks.distance(model3)>4/n), ]

##Variance inflation factors for the quantitative predicting variables(check for multicollinearity)
vif(model3)

## Remove suspicious observations
train3 = train[-which(cooks.distance(model3)>4/n),]
n3 = nrow(train3)

##### MODEL 4. Weighted least squares regression without suspicious observations
## Applying multiple linear regression model
model = lm(charges^2 ~ region + sex + children + smoker + bmi^2 + age, data=train3)
wt1 = 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2
model4=lm(1/sqrt(charges) ~ region + sex + children + smoker + bmi + age, data=train3, weights=wt1)
summary(model4)

##Variance inflation factors for the quantitative predicting variables(check for multicollinearity)
vif(model4)

##Checking asumptions and fitting the model
resids4 = rstandard(model4)
fits4 = model4$fitted
cook4 = cooks.distance(model4)
par(mfrow = c(2,2))

# Scatter plot of residuals vs fitted values (constant variance, independence)
plot(fits4, resids4, xlab="Fitted Values", ylab="Residuals", main="Scatterplot", col=blues9)

# Scatter plot of residuals vs numeric predictors (linearity)
plot(train3[,5], resids4, xlab="BMI", ylab="Residuals",col=blues9)
plot(train3[,6], resids4, xlab='Age', ylab='Residuals', main="",col=blues9)

# QQ normal plot & histogram (normality)
qqPlot(resids4, ylab="Residuals", main = "")
hist(resids4, xlab="Residuals", main = "", nclass=10, col=blues9)

#Checking for outliers
plot(cook4,type="h",lwd=3,col=blues9, ylab = "Cook's Distance")
abline(h = 4*mean(cook4, na.rm=T), col="red") 
sum(cook4>4*mean(cook4, na.rm=T))
train[which(cooks.distance(model4)>4*mean(cook4, na.rm=T)), ]


############## Estimation ###############
test = data[-picked,]
confint4 = predict(model4, newdata = train3, interval = 'confidence')
confint4_trans = 1/confint4^2
summary(confint4_trans)

par(mfrow = c(1,2))
plot(confint4_trans[,1], xlab="Index", ylab="Transformed Estimated Charges", 
     main="Before removing unreasonable estimates", col=blues9)

# Remove Unreasonable Estimated values
confint4_trans = confint4_trans[confint4_trans[,1] < max(charges),]
plot(confint4_trans[,1], xlab="Index", ylab="Transformed Estimated Charges",
     main="After removing unreasonable estimates", col=blues9)
summary(confint4_trans)
sd(confint4_trans[,1])

df4 = data.frame(confint4_trans)
library('ggplot2')
ggp = ggplot(df4, aes(1:nrow(df4), df4[,1])) + geom_point(color = 'darkblue') + geom_ribbon(aes(ymin = df4[,3],ymax = df4[,2]))
ggp + ggtitle('Estimation with Confidence Interval') + xlab('Index') + ylab('Estimated Charges')


# Further Investigate Smokers
smokers4 = train3[train3[,4]=='yes',]
smokers4 = 1/predict(model4, newdata = smokers4, interval = 'confidence')^2
smokers4 = smokers4[smokers4[,1] < max(charges),]
summary(smokers4[,1])
summary(charges)

############## Prediction ###############
pred4 = predict(model4, test, interval = 'predict', level = 0.95) 
summary(pred4)
head(pred4, 10) 

#transform model4 results
pred4_trans = (1/pred4)^2 
par(mfrow = c(1,2))
plot(pred4_trans[,1], xlab="Index", ylab="Transformed Predicted Charges", 
     main="Before removing unreasonable prediction", col=blues9)
test_removed = test[pred4_trans[,1] < max(charges),]
pred4_trans = pred4_trans[pred4_trans[,1] < max(charges),]
plot(pred4_trans[,1], xlab="Index", ylab="Transformed Predicted Charges", 
     main="After removing unreasonable prediction", col=blues9)
summary(pred4_trans)

df4a = data.frame(pred4_trans)
ggp = ggplot(df4a, aes(1:nrow(df4a), df4a[,1])) + geom_point(color = 'darkblue') + geom_ribbon(aes(ymin = df4a[,3],ymax = df4a[,2]))
ggp + ggtitle('Predction with Confidence Interval') + xlab('Index') + ylab('Predicted Charges')

##prediction of model0
pred0 = predict(model0, test, interval = 'predict', level = 0.9)
summary(pred0)
head(pred0, 10)

#save predictions
test.pred0 = pred0[,1]
test.lwr0 = pred0[,2]
test.upr0 = pred0[,3]

#model0 accuracy
MSPE0 = mean((test.pred0-test$charges)^2)
MAE0 = mean(abs(test.pred0-test$charges))
MAPE0 = mean(abs(test.pred0-test$charges)/test$charges)
PM0 = sum((test.pred0-test$charges)^2)/sum((test$charges-mean(test$charges))^2)
num0 = sum(test$charges < test.lwr0 | test$charges > test.upr0)/nrow(test)


##prediction of model1
pred1 = predict(model1, test, interval = 'predict', level = 0.9)
summary(pred1) 
head(pred1, 10) 

#transform model1 results 
pred1_trans = (pred1)^2
summary(pred1_trans)
test_removed1 = test[pred1_trans[,1] < max(test$charges),] 
pred1_trans = pred1_trans[pred1_trans[,1] < max(test$charges),] 
summary(pred1_trans) 
test_removed1
test


#save predictions
test.pred1 = pred1_trans[,1]
test.lwr1 = pred1_trans[,2]
test.upr1 = pred1_trans[,3]

#model1 accuracy
MSPE1 = mean((test.pred1-test_removed1$charges)^2)
MAE1 = mean(abs(test.pred1-test_removed1$charges))
MAPE1 = mean(abs(test.pred1-test_removed1$charges)/test_removed1$charges)
PM1 = sum((test.pred1-test_removed1$charges)^2)/sum((test_removed1$charges-mean(test_removed1$charges))^2)
num1 = sum(test_removed1$charges < test.lwr1 | test_removed1$charges > test.upr1)/nrow(test_removed1)

##prediction of model2 
pred2 = predict(model2, test, interval = 'predict', level = 0.9)
summary(pred2) 
head(pred2, 10) 

#transform model2 results 
pred2_trans = (pred2)^2
test_removed2 = test[pred2_trans[,1] < max(test$charges),] 
pred2_trans = pred2_trans[pred2_trans[,1] < max(test$charges),] 
summary(pred2_trans) 

#save predictions
test.pred2 = pred2_trans[,1]
test.lwr2 = pred2_trans[,2]
test.upr2 = pred2_trans[,3]

#model2 accuracy
MSPE2 = mean((test.pred2-test_removed2$charges)^2)
MAE2 = mean(abs(test.pred2-test_removed2$charges))
MAPE2 = mean(abs(test.pred2-test_removed2$charges)/test_removed2$charges)
PM2 = sum((test.pred2-test_removed2$charges)^2)/sum((test_removed2$charges-mean(test_removed2$charges))^2)
num2 = sum(test_removed2$charges < test.lwr2 | test_removed2$charges > test.upr2)/nrow(test_removed2)


##prediction of model3 
pred3 = predict(model3, test, interval = 'predict', level = 0.9)
summary(pred3) 
head(pred3, 10) 

#transform model3 results 
pred3_trans = (1/pred3)^2
test_removed3 = test[pred3_trans[,1] < max(test$charges),] 
pred3_trans = pred3_trans[pred3_trans[,1] < max(test$charges),] 
summary(pred3_trans) 

#save predictions
test.pred3 = pred3_trans[,'fit']
test.lwr3 = pred3_trans[,'upr']
test.upr3 = pred3_trans[,'lwr']

#model3 accuracy
MSPE3 = mean((test.pred3-test_removed3$charges)^2)
MAE3 = mean(abs(test.pred3-test_removed3$charges))
MAPE3 = mean(abs(test.pred3-test_removed3$charges)/test_removed3$charges)
PM3 = sum((test.pred3-test_removed3$charges)^2)/sum((test_removed3$charges-mean(test_removed3$charges))^2)
num3 = sum(test_removed3$charges < test.lwr3 | test_removed3$charges > test.upr3)/nrow(test_removed3)


##prediction of model4 
pred4 = predict(model4, test, interval = 'predict', level = 0.9)
summary(pred4) 
head(pred4, 10)

#transform model4 results 
pred4_trans = (1/pred4)^2
par(mfrow = c(1,2)) 
plot(pred4_trans[,1], xlab="Index", ylab="Transformed Predicted Charges",
     main="Before removing unreasonable prediction", col=blues9) 
test_removed4 = test[pred4_trans[,1] < max(test$charges),] 
pred4_trans = pred4_trans[pred4_trans[,1] < max(test$charges),] 
plot(pred4_trans[,1], xlab="Index", ylab="Transformed Predicted Charges",
     main="After removing unreasonable prediction", col=blues9) 
summary(pred4_trans) 

#save predictions
test.pred4 = pred4_trans[,1]
test.lwr4 = pred4_trans[,3]
test.upr4 = pred4_trans[,2]

#model4 accuracy
MSPE4 = mean((test.pred4-test_removed4$charges)^2)
MAE4 = mean(abs(test.pred4-test_removed4$charges))
MAPE4 = mean(abs(test.pred4-test_removed4$charges)/test_removed4$charges)
PM4 = sum((test.pred4-test_removed4$charges)^2)/sum((test_removed4$charges-mean(test_removed4$charges))^2)
num4 = sum(test_removed4$charges < test.lwr4 | test_removed4$charges > test.upr4)/nrow(test_removed4)

test.R2_0 = cor(test$charges, test.pred0)^2
test.R2_0
test.R2_1= cor(test$charges, test.pred1)^2
test.R2_1
test.R2_2= cor(test$charges, test.pred2)^2
test.R2_2
test.R2_3= cor((test$charges)^-0.5, pred3[,1])^2
test.R2_3
test.R2_4= cor((test$charges)^-0.5, pred4[,1])^2
test.R2_4

#Predicting Team Members
g1 = list('region'='northeast','sex'='female','children'='0',smoker='no','bmi'=19.1,'age'=23)
g2 = list('region'='northeast','sex'='male','children'='0',smoker='no','bmi'=21,'age'=24)
g3 = list('region'='northwest','sex'='male','children'='3',smoker='yes','bmi'=23,'age'=25)
predict(model2, newdata = g1, interval = 'predict', level = 0.95)^2 
predict(model2, newdata = g2, interval = 'predict', level = 0.95)^2 
predict(model4, newdata = g3, interval = 'confidence', level = 0.95)^-2 

quantile(charges, probs = seq(0.1,0.15, 0.01))

