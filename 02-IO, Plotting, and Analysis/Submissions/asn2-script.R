###############################
## Author: Ali Al-Musawi     ##
## Course: SS2864B           ##
## Assignment: 2             ##
###############################

## 1
##########################################################################
## create a vector that holds the elements -20^2, -19^2, ..., 19^2, 20^2.
vector <- I((-20:20)^2)
## create a data frame that holds the table of values of y = x^2.
df <- data.frame(-20:20, vector)
names(df) <- c("x", "x^2")

## save the vector to a file named "squares".
write(vector, file = "squares")
## save the table of values to a file named "quadratic".
write.table(df, "quadratic")

## scan the file "squares".
vector1 <- scan("squares")
## scan the file "quadratic".
df1 <- read.table("quadratic")

## check equivalence of the scanned files to the original ones.
vector1 == vector && df == df1
##########################################################################

## 2
##########################################################################
## create vectors to store powers of integers.
powers2 <- 2^(1:15)
cubics <- (1:15)^3
## store the length
n <- length(powers2)
## query for which indices the inequality holds.
(1:n)[powers2 > cubics]
##########################################################################

## 3
##########################################################################
## create R objects.
x <- 1:10
y <- as.factor(rep(c("M", "F"), 5))
z <- data.frame(x, y)
## dump above objects.
dump("x", "x.R")
dump("y", "y.R")
dump("z", "z.R")
## save above objects.
save("x", file= "x.RData")
save("y", file= "y.RData")
save("z", file= "z.RData")
## By examining the saved files, it turns out dump() saves the code as is,
## while save() writes it in binary-mode.
## rename the above objects.
xx <- x
yy <- y
zz <- z
rm(list = c("x", "y", "z"))
## source the dumped objects and compare.
source("x.R")
source("y.R")
source("z.R")
(x == xx) && (y == yy) && (z == zz)
rm(list = c("x", "y", "z"))
## load the saved objects and compare.
load("x.RData")
load("y.RData")
load("z.RData")
(x == xx) && (y == yy) && (z == zz)
## Based on the results here, both function similarly.
##########################################################################

##4
##########################################################################
library(MASS)
data(Pima.te)
## create a factor that saves all the Wikipedia categories.
fac1 <- cut(x = Pima.te$bmi, breaks = c(0, 15, 16, 18.5, 25, 30, 35, 40, 45, 50, 60, Inf))
levels(fac1)[1] <- "Very severely underweight"
levels(fac1)[2] <- "Severely underweight"
levels(fac1)[3] <- "Underweight"
levels(fac1)[4] <- "Normal (healthy weight)"
levels(fac1)[5] <- "Overweight"
levels(fac1)[6] <- "Obese Class I (Moderately obese)"
levels(fac1)[7] <- "Obese Class II (Severely obese)"
levels(fac1)[8] <- "Obese Class III (Very severely obese)"
levels(fac1)[9] <- "Obese Class IV (Morbidly obese)" 
levels(fac1)[10] <- "Obese Class V (Super obese)"
levels(fac1)[11] <- "Obese Class VI (Hyper obese)"
## display the frequency and plot
table(fac1)
plot(fac1)
## create a factor that saves 3 Wikipedia categories.
fac2 <- cut(x = Pima.te$bmi, breaks = c(0, 18.5, 25, Inf))
levels(fac2)
levels(fac2)[1] <- "Underweight"
levels(fac2)[2] <- "Normal (healthy weight)"
levels(fac2)[3] <- "Overweight"
## display the frequency and plot
table(fac2)
plot(fac2)
##########################################################################

##5
##########################################################################
## load the dataset.
data("pressure")
## plot the dataset.
plot(x = pressure$temperature, y = pressure$pressure)
## clearly, non-linear.

## calculate the residuals.
residuals <- with(pressure, pressure-(0.168+0.007*temperature)^(20/3))
## produce a QQ-plot of the residuals.
qqnorm(residuals)
qqline(residuals)
## based on the graph, the residuals are skewed and not normal.

## plot temperature vs pressure^(3/20)
plot(x = pressure$temperature, y = I(pressure$pressure^(3/20)))
## the relationship appears to be linear.

## compute the residuals of the transformed pressure and produce a qq-plot.
residuals <- with(pressure, pressure^(20/3)-(0.168+0.007*temperature)^(20/3))
qqnorm(residuals)
qqline(residuals)
## based on the curve, it is slightly skewed.
##########################################################################


##6
##########################################################################
## create a plot of temperature vs pressure, then plot a fitted curve.
plot(x = pressure$temperature, y = pressure$pressure,
     xlab = "Temperature", ylab = "Pressure",
     main = "Temperature VS Pressure")
curve((0.168+0.007*x)^(20/3), from = 0, to = 400, add = TRUE)
legend(x = "bottomright", legend = c("Pressure"))

## repeat above, except transform pressure to power 3/20.
plot(x = pressure$temperature, y = I(pressure$pressure^(3/20)), 
     xlab = "Temperature", ylab = "Pressure^(3/20)", 
      main = "Temperature VS Transformed Pressure")
fit <- lm(I(pressure^(3/20))~temperature, data = pressure)
abline(fit)
legend(x = "bottomright", legend = c("Transformed Pressure"))

## repeat above plots in a 2x1 plot matrix.
par(mfrow=c(2,1))
plot(x = pressure$temperature, y = pressure$pressure,
     xlab = "Temperature", ylab = "Pressure",
     main = "Temperature VS Pressure")
curve((0.168+0.007*x)^(20/3), from = 0, to = 400, add = TRUE)
legend(x = "bottomright", legend = c("Pressure"))
plot(x = pressure$temperature, y = I(pressure$pressure^(3/20)), 
     xlab = "Temperature", ylab = "Pressure^(3/20)", 
     main = "Temperature VS Transformed Pressure")
abline(fit)
legend(x = "bottomright", legend = c("Transformed Pressure"))

## repeat above plots in a 1x2 plot matrix.
par(mfrow=c(1,2))
plot(x = pressure$temperature, y = pressure$pressure,
     xlab = "Temperature", ylab = "Pressure",
     main = "Temperature VS Pressure")
curve((0.168+0.007*x)^(20/3), from = 0, to = 400, add = TRUE)
legend(x = "bottomright", legend = c("Pressure"))
plot(x = pressure$temperature, y = I(pressure$pressure^(3/20)), 
     xlab = "Temperature", ylab = "Pressure^(3/20)", 
     main = "Temperature VS Transformed Pressure")
abline(fit)
legend(x = "bottomright", legend = c("Transformed Pressure"))
##########################################################################
