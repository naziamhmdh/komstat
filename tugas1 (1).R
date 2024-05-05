# Tugas 1
setwd("C:/Users/darre/Desktop/ASDOS KOMSTAT/W2")
getwd()

M = matrix(c(1:10),5,2,
           dimnames = list(c('a','b','c','d','e'),c('A','B')))
# nomor 1
M
M[2,]
M[,1]
M[5,1]
M['d','B']

# nomor 2
vec1 = c(4,5,8)
vec2 = c(7,10,14)
vec3 = c(1,3,4)
A = matrix(c(vec1, vec2, vec3),3,3);A

# nomor 3
B = matrix(seq(21,29),3,3)
B

# nomor 4
diag_A = diag(A)
diag_B = diag(B)

A[row(A)==col(A)]
B[row(B)==col(B)]

# nomor 5
C = A+6;C
D = round(C*2.5,0);D
A*D
B*D
A%*%D
B%*%D
inv_c = solve(C)
inv_c
inv_d = solve(D);inv_d
library(matlib)
inv(C)
inv(D)

# nomor 6
covar = function(x,y){
  n = length(x)
  xy = matrix(0,nrow=n,ncol=1)
  xbar = mean(x)
  ybar = mean(y)
  for(i in 1:n){
    xy[i] = (x[i]-xbar)*(y[i]-ybar)
    covar = sum(xy)/(n-1)
  }
  print(covar)
}

library(datasets)
data <- Orange
covar(Orange$age,Orange$circumference)
cov(Orange$age,Orange$circumference)

correl = function(x,y){
  n = length(x)
  xy = x*y
  xx = x*x
  yy = y*y
  correl = ((n*sum(xy))-(sum(x)*sum(y)))/
    ((sqrt(n*sum(xx)-(sum(x))^2))*(sqrt(n*sum(yy)-(sum(y))^2)))
  print(correl)
}
cor(Orange$age,Orange$circumference)
correl(Orange$age,Orange$circumference)

# sigmax = sqrt(var(Orange$age))
# sigmay = sqrt(var(Orange$circumference))
# cor1 = covar(Orange$age,Orange$circumference)/(sigmax*sigmay)
