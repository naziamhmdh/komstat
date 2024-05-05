#Komstat D - Praktikum
#28/8/2023
#Introduction to R

##### 
# Packages in R

#install.packages('ggplot2')
library('ggplot2')

#####
# Basic R Operations

a <- 1 # masukin nilai ke variable
a
class(a)

a <- c(1,2,3)
a
class(a)
length(a)
rev(a)
a[1]
a[2]
a[-1]
a[-2]

array(1,4)

seq(10)   # seq mulai dari 1 - 10
seq(3)
seq(3,10) # seq dari 3 - 10
seq(1,10, by=0.2)  # seq dari 1 - 10, jaraknya 0.2
seq(2,6, length.out=4) # seq dari 2 - 6, mengeluarkan hanya 4 angka dengan interval sama
seq(2,6, along.with = 1:4)
#?seq()

rep(a, times = 10)
rep(a, each = 3)
rep(a, len = 5)



#####
#vector operations
vec1 <- as.vector(seq(1,5))
vec1
class(vec1)
vec2 <- as.vector(seq(6,10))
vec2
class(vec2)

vec3 = vec1 + vec2
vec3
vec4 = vec1 + 20
vec4
vec5 = vec2 - 3
vec5
vec2-vec1
vec1*vec2
vec1/vec2

#####
#Mathematical Operations
35 + 19
26.6 + 78.991
98 - 67.9961
6 * 5
90 / 18
sqrt(144)
19^2
6%%2
5%%2 #keluarin hasil sisa pembagian
6%/%2 #keluarin hasil bagi
8%/%2


#####
#Rounding of Numbers
b <- 6.889
print(b)
floor(b) #pembulatan ke nilai terendah selanjutnya
ceiling(b) #selalu membulatkan ke atas
trunc(b) #selalu membulatkan ke hanya nilai di kiri desimal
round(b) #pembulatan biasa


#####
#Matrix Operations
matrix(1:12,nrow=3)
matrix(1:12,nrow=3,byrow=T)
matrix(1,nrow=2,ncol=2)
matrix(1:12,3,4)
matrix(0,nrow=5,ncol=5)

x <- matrix(1:10,2,5,byrow=T)
x
dim(x)
col(x)
row(x)

x2 <- matrix(1:12,3,3);x2
x2[row(x2)==col(x2)] #nyari diagonal
diag(x2)
k <- matrix(1:10,2,5);k
k[1:2,3:4]
k[1,4]
k[1:2,1:2]
k[,1]
k[1,]

xij <- matrix(seq(1:40),ncol=4)
dim(xij)
rownames(xij) <- paste('S',seq(1,dim(xij)[1]),sep = "")
colnames(xij) <- paste('V',seq(1,dim(xij)[2]),sep = "")
print(xij)

t(xij)

colMeans(xij)
rowMeans(xij)

#matrix inversion
x3 <- matrix(1:4,2,2);x3
x3.inv <- solve(x3)
x3.inv

x3.det <- det(x3);x3.det
x3.tran <- t(x3);x3.tran

#cross product
x_cross_k <- t(x)%*%k
x_cross_k
x_cross_k_1 <- crossprod(x, k)

#####
#Data Frame & Data Input
a=c(10,20,15,43,76,41);a
b=factor(c("m","f","m","f","m","f"));b
c=c(2,5,8,3,6,1);c
myframe=data.frame(a,b,c)
myframe
colnames(myframe)=c("Age","Sex","Siblings")
myframe

myframe[,1]
myframe["Age"]
myframe$Age
myframe[3,3]=2
myframe
myframe[,-2]

#write data
library('datasets')
data('Orange')
View(Orange)
getwd()
write.table(Orange, 'orange1.txt')
write.table(Orange, 'orange2.txt', col.names=F, row.names = F)
write.table(Orange, "orange3.txt", sep = "\t")
write.csv2(Orange, "orange4.csv")

#read txt
data = read.table("orange1.txt", header = T)

#read excel
library(readxl)
data_xl = read_excel("C:/Users/darre/Desktop/ASDOS KOMSTAT/data_tinggi.xlsx")

# read csv
data_csv=read.csv("orange4.csv", sep=";", header=TRUE)
