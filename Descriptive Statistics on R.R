# Q1:
Auto <- read.csv("~/Downloads/Auto.csv")
View(Auto)
autos=read.csv("auto.csv",na.strings="?")
autos=na.omit(autos)
attach(autos)
range(mpg)
mean(mpg)
sd(mpg)
# Likewise for displacement,horsepower,weight,acceleration
autos2=autos[-(10:85),]
range(autos2$mpg)
mean(autos2$mpg)
sd(autos2$mpg)
# Likewise for displacement,horsepower,weight,acceleration
pairs(~mpg+horsepower+displacement+weight+acceleration)
# Scatterplot matrix for quantitative predictors
plot(cylinders,mpg)
# Better to define qualitative variables as factor variables. Then R automatically produces box plots
cylinders=as.factor(cylinders)
plot(cylinders,mpg)
plot(horsepower,mpg)

# You could create a CSV file separately, e.g., in excel and import into R as usual. Below I create a dataframe dreictly in R
X=c(2,3,5,1,8)
Y=c(25,25,20,30,16)
CDF=data.frame(X,Y)
CDFfit=lm(CDF$y~CDF$x)
summary(CDFfit)
plot(CDF$x,CDF$y)
b0=summary(CDFfit)$coefficients[1]
b1=summary(CDFfit)$coefficients[2]
abline(b0,b1,col="red")

# Q3:
# Import data as in Q1 and attach variable names
Auto.fit=lm(mpg ~ horsepower)
summary(Auto.fit)
plot(horsepower,mpg)
b0=summary(Auto.fit)$coefficients[1]
b1=summary(Auto.fit)$coefficients[2]
abline(b0,b1,col="red",lwd=2)
predict(Auto.fit,data.frame(horsepower=98),interval = "confidence")
predict(Auto.fit,data.frame(horsepower=98),interval = "prediction")
# Comments: 1. The low p-values of the coefficients indicate statistical significance of the linear fit.
# 2. The R^2 is reasonably large indicating a good fit
# 3. The prediction interval is wider, because it takes into account the irreducible error due to noise.
# 4. However, looking at the plot you see that there is a nonlinear trend in the data, which is not captured by the linear model.
# The regression could be improved by using, e.g., a polynomial function.
