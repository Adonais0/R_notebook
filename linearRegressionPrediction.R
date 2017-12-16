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
