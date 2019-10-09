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

# PCA ANALYSIS

autos.data=read.csv("Auto.csv",na.strings="?")
autos.data2=na.omit(autos.data)
attach(autos.data2)
head(autos.data2)
autos.data3=autos.data2[,c(1,3,4,5,6)]
head(autos.data2)
cor(autos.data3) # gives an error- x must be numeric. fixed with the following code.
cor(AutoData1[sapply(AutoData1, function(x) !is.factor(x))])
#mpg displacement     weight acceleration
#mpg           1.0000000   -0.8044430 -0.8317389    0.4222974
#displacement -0.8044430    1.0000000  0.9331044   -0.5441618
#weight       -0.8317389    0.9331044  1.0000000   -0.4195023
#acceleration  0.4222974   -0.5441618 -0.4195023    1.0000000

plot(horsepower,displacement)

#auto.pca=prcomp(autos.data3,scale=TRUE)
auto.pca=prcomp(AutoData1[sapply(AutoData1, function(x) !is.factor(x))],scale=TRUE)
auto.pca$rotation
                #PC1         PC2         PC3         PC4
#mpg           0.5169159  0.25728453 -0.81220899 -0.08318184
#displacement -0.5520656 -0.09166076 -0.45155782  0.69492257
#weight       -0.5413746 -0.28441295 -0.36262152 -0.70322714
#acceleration  0.3673351 -0.91897348 -0.07012652  0.12503993

pca.var=auto.pca$sdev^2
pca.var/sum(pca.var)
# 0.75758374 0.17441545 0.05447005 0.01353076
head(auto.pca$x)

#PC1       PC2          PC3       PC4
#1 -1.780207 0.7281919 -0.055993122 0.2097142
#2 -2.393255 0.6954922  0.001263114 0.3483949
#3 -1.928546 1.0755143 -0.048998333 0.2938764
#4 -1.851113 0.6888891  0.194921675 0.2698849
#5 -1.985063 1.2194140  0.131196094 0.1644668
#6 -3.425191 0.9100186 -0.579377786 0.2687130

output=data.frame(auto.pca$x)
plot(output$PC1,output$PC2)

output2=data.frame(output$PC1,output$PC2)

set.seed(10)
km=kmeans(output2,2,nstart=20)
plot(output2, col=km$cluster+1)
points(km$centers,pch=19)
km
                                 K-means clustering with 2 clusters of sizes 158, 239

Cluster means:
  output.PC1 output.PC2
1  -1.825529 -0.1893099
2   1.206835  0.1251505

Clustering vector:
  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 1 2 2 2
 [53] 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 2 1 1 1 1 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 1
[105] 1 1 1 1 2 2 2 2 2 2 2 1 1 2 2 2 2 1 2 2 1 2 2 1 1 2 2 2 2 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1
[157] 1 1 1 1 1 1 1 1 1 1 1 2 2 1 2 2 2 2 1 2 1 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 2 2 2 2 2 2 1 1 1 1 2 2 2 2 2
[209] 1 2 2 1 1 1 1 1 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 2 2 2 1 1 1 2
[261] 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 1 2 1 2 2 1 2 2 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2
[313] 2 2 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1
[365] 1 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2

Within cluster sum of squares by cluster:
[1] 270.4907 321.7544
 (between_SS / total_SS =  59.9 %)

Available components:

[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"    "size"        
[8] "iter"         "ifault"      
                                 
           
