# 28 July 2020
# Simple Linear Regression

setwd("/home/chrysanthemum/r_practice/assignment-one")

#  Depending on the size of area, the amount of lawn grass will be large or small.
# Proportion is independent of size of area.

#a) read the data in 
mydata <- read.csv("lawngrass.csv", skip = 16, nrows = 25)
# check the data head/tail/names/summary
head(mydata) 
names(mydata) 
str(mydata)


#b) Plot lawn grass area against number of fires.
plot(mydata$fireNo, mydata$lg)


#c) Trying to find a proportion of the two things
#Did it in LibreOffice AKA excel 
# Find the proportion of lawn grass in the entire park
7151 / (7151 + 77217)
# 7151 / 89480.5 this is wrong


#d) Make a scatter plot of the proportion of lawn grass against number of fires.
plot(mydata$fireNo, mydata$lg_prop)


#e) Create a histogram of the proportion of lawn grass
hist(mydata$lg_prop)
#Skewed to the right and positively skewed

# Find the proportion (to the nearest percentage) of lawn grass in the
# entire park (assuming the sum of the Areafire variable gives the total area of the park)? 
(7151 / 89480.5) * 100

# Find the correlation (Pearsons correlation) between proportion 
# of lawn grass and number of fires.
cor.test(mydata$fireNo, mydata$lg_prop)
# Interpretation of the the above correlation coefficient.
# Proportion of lawn grass seems to decrease with more frequent fires. 
# However, we need to be a bit careful in interpreting the value of the 
# coefficient, because the relationship is not linear and there seem to be 
# some points that do not conform to the general pattern.


#f) Fire return interval
#Used LibreOffice Calc to do 40 / fireNo


#g) Plot lawn grass proportion against fire return interval
attach(mydata)
plot(fire_int, lg_prop)


#h) fit a linear regression model to the above plot
m1 <- lm(lg_prop ~ fire_int, data=mydata)
#Look at the key points in the model
summary(m1)


#i) add the fitted regression line to the above plot
abline(m1, lwd = 2, col = 'red')


#j) Diagnostic plots of model
plot(m1)


#k) Exclude two data points 
mydata2 <- mydata[-c(1:2),] 


#l) two outlier data points behave differently from the rest
m2 <- lm(lg_prop ~ fire_int, data = mydata2)
summary(m2)
plot(m2)
plot(mydata2$fire_int, mydata2$lg_prop, pch=19, xlim=c(0,14), ylim = c(-0.02, 0.26), xlab="Fire return interval [years]",ylab="Proportion of lawn grass", cex.lab=1.5, main = "The proportion of lawn grass in relation to the fire return interval.")
abline(m2, lwd = 2, col = 'blue')

svg("lawngrassplot.svg")
par(mar = c(6, 6, 1, 1), mgp = c(4, 1, 0))
plot(lg_prop ~ fire_int, data = mydata, las = 1, pch = 19, cex.axis = 1.5,
     cex.lab = 1.5, xlab = "fire return interval (years)", cex = 1.5,
     ylab = "proportion of lawn grass out of total grass", col = "forestgreen")
abline(m1, lwd = 2)
abline(m2, col = "red", lwd = 2)
dev.off()

svg("lawngrassplot.svg")

# Question 11
confint(m2)
# p does NOT measure the P(null hypothesis)

#Q12 predict avg proportion of grass within fire_int of 20 years
pred.fire <- predict(m2, newdata = data.frame(fire_int = 20))

#Q13 95% prediction interval for 20 years
#Prediction intervals
pred.int =  predict(m2,newdata = data.frame(fire_int = 20),interval="prediction")
round(0.3896958, digits = 2)


#Confidence intervals
conf.int =  predict(m2,newdata = data.frame(fire_int = 20),interval="confidence")
fitted.values = pred.int[,1]

pred.lower = pred.int[,2]
pred.upper = pred.int[,3]


