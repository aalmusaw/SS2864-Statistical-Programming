##################################################
# Ali Al-Musawi                                  #
# SS 2864B                                       #
# Assignment 3                                   #
##################################################
# Question 1
x = seq(-pi, pi, length = 100)
y = x
f = function(x, y) {r = (2+sin(x))*cos(2*y)}
z = outer(x, y, f)
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "(2+sin(x))*cos(2y)",
      main = "A surface plot of a 3D trig function")

persp(x, y, z, theta = 45, phi = 10, expand = 0.5, col = "green",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "(2+sin(x))*cos(2y)",
      main = "A surface plot of a 3D trig function")

persp(x, y, z, theta = 130, phi = 45, expand = 0.5, col = "yellow",
      ltheta = 120, shade = 0.75, ticktype = "detailed",
      xlab = "X", ylab = "Y", zlab = "(2+sin(x))*cos(2y)",
      main = "A surface plot of a 3D trig function")

# Question 2
question2 = function(n) {
  if (!is.numeric(n)) stop("Input is not numeric")
  if (any(is.na(n))) stop("Input contain NA")
  m = sum(pmin(2^n, n^3))
  M = sum(pmax(2^n, n^3))
  r = list("Sum of minmums" = m, "Sum of minmums" = M)
  return(r)
}
question2(seq(200, 5000, by = 600))

# Question 3
IQR.outliers = function(x) {
  if (!is.numeric(x)) stop("Input is not numeric")
  if (any(is.na(x))) stop("Input contain NA")
  n = length(x)
  q3 = x[rank(x)==round(n*0.75)]
  q1 = x[rank(x)==round(n*0.25)]
  iqr = q3 - q1
  low.outliers = x[x < q1-1.5*iqr]
  high.outliers = x[x > q3+1.5*iqr]
  boxplot(x, col = "bisque", xlab = "Input Data")
  title(main = "A boxplot of the input data")
  return(list("IQR" = iqr, "Left (Lower) Outliers" = low.outliers,
              "Right (Upper) Outliers" = high.outliers))
}

# Question 4
GLB = read.csv("GLB.Ts_dSST.csv",header = T)
mydata = subset(GLB, select = colnames(GLB)[1:13])
mymean = function(x) {
  if (any(is.na(x))) stop("Input contains NAs")
  return (mean(x[-1]))
}
avg.temp = apply(X = mydata, MARGIN = 1 ,FUN = mymean)
avg.temp.tss <- ts(avg.temp, start=c(mydata[1,1]), end=c(mydata[140,1]), frequency=1)
plot(avg.temp.tss, type = "l",
     xlab= "Years", ylab = "Average Monthly Temperatures (normalized)",
     col= "green" , lwd=2)
mydata.2 = mydata[c(seq(1, 140, by = 20), 140), ]
plot(x = 1:12, y = mydata[1, -1], type = "l",
     xlab= "Months", ylab = "Monthly Temperatures (normalized)",
     col= mydata[1, 1] , lwd=2, lty = 1,
     ylim = c(-0.7, 0.2), main = "Temperatures over several decades")
lines(x = 1:12, y = mydata[2, -1], col= mydata[2, 1] , lwd=2, lty = 2)
lines(x = 1:12, y = mydata[3, -1], col= mydata[3, 1] , lwd=2, lty = 3)
lines(x = 1:12, y = mydata[4, -1], col= mydata[4, 1] , lwd=2, lty = 4)
lines(x = 1:12, y = mydata[5, -1], col= mydata[5, 1] , lwd=2, lty = 5)
lines(x = 1:12, y = mydata[6, -1], col= mydata[6, 1] , lwd=2, lty = 6)
lines(x = 1:12, y = mydata[7, -1], col= mydata[7, 1] , lwd=2, lty = 1)
lines(x = 1:12, y = mydata[8, -1], col= mydata[8, 1] , lwd=2, lty = 4)
legend(x = "bottomright", legend = as.character(mydata.2[ ,1]), col = mydata.2[ ,1], lty = c(1:6, 1, 4))

# Question 5
my.ecdf = function(x, y) {
  if (length(y)!= 1) stop("Length of input y must be 1.")
  x.yes = x[x<=y]
  n = length(x)
  return (sum(x.yes)/n)
}

