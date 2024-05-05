# Praktikum Komstat D 
# Selasa 5/9/2023
# IF & LOOP STATEMENT

#####
#SET WORKING DIRECTORY
getwd()
setwd("C:/Users/darre/Desktop/ASDOS KOMSTAT")
getwd()

#####
##EKSPORT
library('datasets')
data('Orange')
#write txt
write.table(Orange, 'orange1.txt')
write.table(Orange, 'orange2.txt', col.names=F, row.names = F)
write.table(Orange, "orange3.txt", sep = "\t")

#write csv
write.csv2(Orange, "orange4.csv")

#write excel
library(writexl)
write_xlsx(Orange,"orange6.xlsx")

#write spss
library(haven)
write_sav(Orange,"orange5.sav")
      
##IMPORT
#read txt
data = read.table("orange1.txt", header = T)

#read excel
library(readxl)
data_xl = read_excel("orange6.xlsx")

# read csv
data_csv = read.csv("orange4.csv", sep=";", header=TRUE)
data_csv

#read spss
library(foreign)
data_sav = read.spss("orange5.sav")

#####
# IF Statement
age <- 18
if(age >= 18){
  print("You are eligible to vote")
}else{
  print("You are not eligible to vote")
}

number <- -7
if (number > 0){
  print("number is a positive number")
}else if(number < 0){
  print("number is a negative number")
}else{
  print("number is zero")
}

# bilangannya genap / ganjil
if (number > 0){
  if (number %% 2 == 0){
    print("number is positive even number")
  }else{
    print("number is positive odd number")
  }
}else{
  if (number %% 2 == 0){
    print("number is negative even number")
  }else{
    print("number is negative odd number")
  }
}



#####
#FOR LOOP
week <- c('Sunday', 
          'Monday', 
          'Tuesday', 
          'Wednesday', 
          'Thursday', 
          'Friday', 'Saturday')

for (day in week){
  print(day)
}

my_list <- list(1, 2, 3, 4, 5)
for (i in seq_along(my_list)){
  current_element <- my_list[[i]]
  print(paste("The current element is:", current_element))
}

#hitung varians
data <- read.csv('klasifikasi.csv', sep=";", fileEncoding = "UTF-8-BOM")
n = nrow(data)
xbar = mean(data$Berat);xbar

xx = matrix(nrow=n, ncol=1)

for(i in 1:n){
  xx[i] = (data$Berat[i]-xbar)^2
}
varians = sum(xx)/(n-1)
varians



#####
#while loop
i = 0
while (i < 4){
  i = i + 1
  print(i)
}

#fungsi factorial
i <- 1
factorial <- 1
n <- 6
while(i <= n){
  factorial = factorial * i
  i = i + 1
  print(factorial)
}



#####
# Function & Descriptive Statistics
#rata rata
rata_rata = function(data){
  n = length(data)
  xx = sum(data)
  rata = xx/n
}
print(rata_rata(data))
mean(data)
#Varians
varians = function(data){
  n = length(data)
  xbar = mean(data)
  xx = matrix(nrow=n, ncol=1)
  
  for(i in 1:n){
    xx[i] = (data[i]-xbar)^2
  }
  varians = sum(xx)/(n-1)
}
print(varians(data$Berat))
#COVARIANCE
x=c(1:10);x
y=c(11:20);y
sxy=0
kov=function(x,y)
{
  n=length(x)
  xbar=mean(x)
  ybar=mean(y)
  xy=matrix(n,1)
  for(i in 1:n)
  {
    xy[i]=(x[i]-xbar)*(y[i]-ybar)
    sxy=sxy+xy[i]
  }
  kov=sxy/(n-1)
  list(covariance=kov)
}
kov(x,y)
cov(x,y)
kov(Orange$age,Orange$circumference)
cov(Orange$age,Orange$circumference)

