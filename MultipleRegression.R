setwd("documents/files/umich/si544")
cars = read.csv("cars.csv")
#R code for scatterplot and correlation matrices
install.packages("car")
library(car)
pairs(~ Y+X1+X2+X3, data = example)
cor(example[,c("Y","X1","X2","X3")], use = "complete.obs")
## Confidence bands and predictipon intervals
newdata = data.frame(x1 = 10,x2 = 4, x3 = 11)
conf.band = predict(model, newdata, interval = "confidence", level = 0.90)
pred.interval =  predict(model, newdata, interval = "prediction", level = 0.90)
# F test for nested models
model1 = lm(y~ x1+x2, data = example)
model2 = lm(y~ x1+x2+x3, data = example)
anova(model1 ,model2)

# Homework
View(cars)
model = lm( mpg ~ engine+horse+weight+accel, data = cars)
summary(model)
newdata = data.frame(
  engine = 200,
  horse = 100,
  weight = 3000,
  accel = 15
)
predict(model, newdata)
## Prediction interval
predict(model, newdata,interval="prediction", level = 0.90)
#scatterplot matrix
pairs(~mpg+engine+horse+weight+accel,data = cars)
#correlation matrix
cor(cars[,c("mpg","engine","horse","weight","accel")],use = "complete.obs")
#VIF
vif(model)#remove large vif variables
model1 = lm(mpg ~weight, data = cars)
summary(model1)
## delete missing values
complete = subset(cars, !is.na(mpg) & !is.na(weight) & !is.na(horse) & !is.na(engine) & !is.na(accel))
## added variable plot
model1 = lm(Salary ~ hits, data = strikeout)
model2 = lm(strikeouts ~ hits, data = strikeout)
plot(model1$residuals, model2$residuals, xlab = "Salary residual after adjusting for hits", ylab = "strikeout residual after adjusting for hits", pch = 16)
cor(model1$residuals, model2$residuals)

# Lecture 11 Transformation
## Square root transformation
model = lm(sqrt(y) ~ x, data = example)
## Log transformation
model = lm(I(log(saleprice)) ~ built, data = sales_48103)
### 95% Confidence interval for effect
c = 10*confint(model,level = 0.95)
100*(exp(c))
### hypothesis test whether percentage increases in outcome greater than 1% when (x = 10)
t = (observed_slope - log(1+percentage)/10)/standard_error
p = 1-pt(t, n)
## 95% confidence interval for the effect of a 1% increase in square feet on sales price
100*(1.01**0.7826-1.01**0.9226-1)

# Homework
## Squared regression
model1 = lm(sqrt(mpg) ~ engine + horse, data = cars)
newdata = data.frame(horse = 100, engine = 150)
predict(model1, newdata)**2
predict(model1, newdata, interval = "prediction")**2
## Log regression
model2 = lm(I(log(mpg))~ engine + horse, data = cars)
exp(predict(model2, newdata))
exp(predict(model2, newdata, interval = "prediction"))
exp(5*confint(model, level = 0.98))
## Log log regression
model3 = lm(I(log(mpg))~ engine + I(log(horse)), data = cars)
mpg = exp(5.536-0.475*log(horse)-0.00133*engine)
exp(predict(model, newdata))