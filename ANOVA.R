setwd("documents/files/umich/si544")

#Lecture 8 ANOVA
tomato = read.csv('data.csv')
View(tomato)
summary(tomato$TomatoMeter)
e <- as.numeric(tomato$TomatoMeter,na.rm = TRUE)
#Compare Mean of different groups 
#Reshape Data
##Package.csv has two columns Pack1 and Pack2
##Pack1 has the sales for the districts using package 1
package = read.csv("package.csv")
View(package)
rep1 = data.frame(package$Pack1)
View(combined)
rep1$package = 1 #sub 'column' package of rep1
colnames(rep1) = c("sales","package")
rep2 = data.frame(package$Pack2)
rep2$package = 2
colnames(rep2) = c("sales","package")
combined = rbind(rep1, rep2)##Stack the two dataset vertically
combined$package = relevel(factor(combined$package),ref = 2)#set packaging 2 as the reference category for package
model = lm(sales~package, data = combined)
summary(model)
#ANOVA in R
model = aov(mpg~factor(origin), data = cars)
summary(model)
# ANCOVA
cars$origin = factor(cars$origin)#catigorical variable
levles(cars$origin) = c("American","European","Japanese")
model = lm(mpg~origin, data = cars)
summary(model)
ancova = lm(Time ~ Plant + Boxes + Boxes*Plant, Data = ca)
summary(anova)

#Homework
pizzasales = read.csv("pizzasales.csv")
pizzasales$Competitor = factor(pizzasales$Competitor)
levels(pizzasales$Competitor) = c("No","Yes")
model = lm(Sales ~ Competitor, data = pizzasales)
summary(model)
boxplot(Sales~Competitor,data = pizzasales, xlab = "Competitor within 0.5 miles", ylab = "Average sales per day")
##multi-variable plot
with(pizzasales,plot(Income, Sales, col=c("red","blue")[Competitor],pch = 16))
legend(x = "topleft",legent = levels(pizzasales$Competitor),col = c("red","blue"),pch = 16)#图例
model1 = lm(Sales ~ Income, Competitor == "No", data = pizzasales)
model2 = lm(Sales ~ Income, Competitor == "Yes", data = pizzasales)
abline(model1, col = "red", lwd = 2)
abline(model2, col = "blue", lwd = 2)
model3 = lm(Sales ~ Income + Competitor, data = pizzasales)
model4 = lm(Sales ~ Income + Competitor + Income*Competitor, data = pizzasales)
summary(model4)
#Interaction term: the effect of income on sales is significantly less for stores with a competitor as compared to stores without a competitor
#relevel to change the reference category
pizzasales$Competitor = relevel(pizzasales$Competitor, ref = "Yes")
model4 = lm(Sales ~ Income + Competitor + Income*Competitor, data = pizzasales)

# Lecture 9 Spurious Correlation
## Correlation is not causation
##Confounding Variables
##quadratic term
data$xsq = (data$x)**2
model = lm(y ~ x + xsq, data = data)
model = lm(y ~ x + I(x**2), data = data)
##Studentize the residual /estimate the residual standard deviation excepte for the "outlier"
student.del.resids = rstudent(model)
##cook's distance
cooks.distance(model)

#Homework
strikeout = read.csv("strikeouts3.csv")
with(strikeout, plot(x = strikeout$strikeouts,y = strikeout$Salary, xlab = "Strikeouts", ylab = "Salary"))
model = lm(Salary ~ strikeouts, data = strikeout)
summary(model)
##add confounding variable
model = lm(Salary ~ strikeouts + hits, data = strikeout)
cars = read.csv("cars.csv")
with(cars, plot(x = cars$weight, y = cars$horse) )
horse_model = lm(cars$horse ~ cars$weight, data = cars)
##Residual plots 
with(horse_model2, plot(fitted.values, residuals, pch = 16))
abline(0,0)#increase spread: heteroscedacity
## Add mean-centered quadratic term 
cars$weight_certered = cars$weight - mean(cars$weight)
horse_model2 = lm(horse ~ weight_certered + I(weight_certered**2), data = cars)
## Studentized deleted residuals
stud.del.resids = rstudent(horse_model)
### Rescale fitted values
zpred = (horse_model$fitted.values - mean(horse_model$fitted.values))/sd(horse_model$fitted.values)
with(horse_model,plot(zpred, stud.del.resids,xlab = "Scaled fitted values", ylab = "Studentized Deleted Residuals",pch = 16))
abline(0,0)
## Cook's distance
horse_model$model$cook = cooks.distance(horse_model)
n = nrow(horse_model$model)
outliers = subset(horse_model$model, cook>4/n)
nrow(outliers)
outliers
