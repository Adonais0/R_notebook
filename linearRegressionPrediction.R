setwd("documents/files/umich/si544")
#Lecture 6 linear regression
autorama = read.csv("autorama.csv")
seqx = seq(20000,100000,10000)
labx = c("$20K","$30K","$40K","$50K","$60K","$70K","$80K","$90K","$100K")
seqy = seq(5000,30000,5000)
laby = c("$5K","$10K","$15K","$20K","$25K","$30K")
#png("scatter.png")
with(autorama,plot(Income, Price, pch = 16, axes = FALSE))
axis(1,at = seqx, labels = labx)
axis(2,at = seqy, labels = laby)
cor(autorama$Income,autorama$Price)
dev.off()
model = lm(Price~Income, data = autorama)
summary(model)
##making predictions
###add Predict 
autorama$Predict = predict(model, autorama)
newdata = data.frame(Income = (c(30000,60000,90000)))
predict(model,newdata)
##Confidence Interval: [Estimate+-t*sd],t = qt(0.975,n-2)
##Homework
sales = read.csv("Sales_AnnArbor_48103.csv")
seqx = seq(0,8000,1000)
labx= c()
for(num in seqx){
  labx = c(labx, paste0(num,"sqft"))
}
seqy = seq(0,1000000,200000)
laby = c()
for(num in seqy){
  laby = c(laby,paste0("$",num/1000,"K"))
}
plot(x = sales$squarefeet,y = sales$saleprice,xlab = "Sq Ft", ylab = "Sales Price",pch = 16,las = 1,axes = FALSE)
axis(1,at = seqx, labels = labx)
axis(2,at = seqy, labels = laby)
model = lm(saleprice~squarefeet,data = sales)
abline(model)
summary(model)
sales$Predict = predict(model, sales)
sales$Residual = sales$saleprice-sales$Predict
plot(x = sales$Predict, y = sales$Residual,las = 1,xlab = "Predicted", ylab = "Residual",pch = 16, axes = FALSE)
abline(0,0)
dev.off()
laby = c()
seqy = seq(-400000,600000,100000)
for(num in seqy){
  laby = c(laby, paste0("$",num/1000,"K"))
}
seqx = seq(0,1200000,200000)
labx = c()
for (num in seqx){
  labx = c(labx, paste0("$", num/1000,"K"))
}

axis(1,at = seqx,labels = labx)
axis(2,at = seqy,labels = laby)
abline(0,0)

#Lecture 7 
##calculate 90% confidence interval for mean sale price for 1000 squarefoot 
newdata = data.frame(squarefeet = c(1000))
predict(model, newdata, interval = "confidence", level = 0.90)
predict(model, newdata, interval = "prediction", level = 0.95)
##Homework 
cars = read.csv("cars.csv")
with(cars, plot(x = cars$engine, y = cars$horse,xlab = "Engine Size", ylab = "Horse Power", pch = 16))
model = lm(horse~engine, data = cars)
abline(model)
dev.off()
cor(cars$engine, cars$horse, use = "complete.obs")
cars$engine2 = cars$engine
cars$engine2[is.na(cars$horse)] = NA
engine_mean = mean(cars$engine,na.rm = TRUE)
engine2_mean = mean(cars$engine2, na.rm=TRUE)
engine2_sd = sd(cars$engine,na.rm = TRUE)
horse_mean = mean(cars$horse,na.rm = TRUE)
horse_sd = sd(cars$horse,na.rm = TRUE)
slope = 0.8970699*horse_sd/engine2_sd
model = lm(horse~engine, data = cars)
summary(model)
##residual standard error/deviation
SSR = sum(model$residuals**2, na.rm = NA)
s = sqrt(SSR/model$df.residual)
##standard error for slope
sd_slope = s/sqrt(sum((model$engine-engine_mean)**2))
##standard error for predict mean
sd_predict_mean = s*sqrt(1/398+(180-engine_mean)**2/sum((cars$engine-engine_mean)**2))
##90% confidence interval for mean horse with engine = 180
model = lm(horse~engine, data = cars)
summary(model)
predict(model, newdata, level = 0.90,interval = "confidence")
##Hypothesis test engine = 180
#H0:mean horsepower = 90 
predict_mean = 41.002+0.3273*180
t_stats = (predict_mean-90)/sd_predict_mean
p_value = 1-pt(t_stats,398)
##standard error for predict value, engine = 180
sd_predict_value = s*sqrt(1+1/398+(180-engine_mean)**2/sum((cars$engine-engine_mean)**2))
sd_predict_value
##90% CI for predicted value, engine = 180
upper = predict_mean + qt(0.95,398)*sd_predict_value
lower = predict_mean - qt(0.95,398)*sd_predict_value
##confidence bands and prediction intervals
band = data.frame(engine2 = seq(min(cars$engine, na.rm = TRUE), max(cars$engine, na.rm = TRUE)))
y = predict(model, band, interval = "confidence", level = 0.90)
points(band$engine2,y[,2],pch = ".",cex = 2, col = "blue")
points(band$engine2,y[,3],pch = ".",cex = 2, col = "blue")
prection = predict.lm(model,band,interval = "prediction",level = 0.90)
points(band$engine2,prediction[,2],pch = ".",cex = 2,col = "green")
points(band$engine2, prediction[,3],pch = ".", cex = 2, col = "green")
with(cars, plot(engine2,horse, xlab = "Engine size", ylab = "Horsepower", pch = 16))
y = predict(model, band, interval = "confidence", level = 0.90)
points(band$engine2, y[,2], pch = ".", cex = 2, col = "blue")
points(band$engine2, y[,3], pch = ".", cex = 2, col = "blue")
points(band$engine2, y[,2], pch = ".", cex = 2, col = "green")
points(band$engine2, y[,3], pch = ".", cex = 2, col = "green")
