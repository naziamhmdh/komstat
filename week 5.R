#week 5 (Regresi Linear simultan dan partial)
install.packages("datarium")
library(datarium)
data = marketing;data


#Linear sederhana

Y <- data$sales
x <- data$youtube
#bawaan
regres = lm(Y~x,data=data)
summary(regres)
#manual
xb <- cbind(rep(1, length(x)), x) ;xb
beta <- solve(t(xb) %*% xb) %*% t(xb) %*% Y;beta

#Regresi linear berganda
Y <- data$sales
x1 <- cbind(data$youtube,data$facebook,data$newspaper)
x1
# Menambahkan kolom 1 untuk konstanta

X <- cbind(rep(1, length(Y)), x1) ;X
beta <- solve(t(X) %*% X) %*% t(X) %*% Y;beta
regresi = lm(Y~x1,data=data)
summary(regresi)
#simultan
n=length(Y)
k=ncol(X)-1 ;k
SSR=t(beta)%*%t(X)%*%Y-sum(Y)^2/n;SSR
SST=t(Y)%*%Y-sum(Y)^2/n;SST
SSRES=SST-SSR;SSRES
MSE=SSRES/(n-k-1)
MSREG=SSR/k
f0=MSREG/MSE;f0
p=2*(1-pf(f0,k,n-k-1));p
#buat Tabel ANOVA SIMULTAN
SS=c(SSR,SSRES,SST);SS
df=c(k,n-k-1,n-1);df
MS=c(MSREG,MSE,NA);MS
FHIT=c(f0,NA,NA)
pval=c(p,NA,NA)
source=c("regression","residual","total")
ANOVA=cbind(source,SS,MS,FHIT,pval)
View(ANOVA)
#CARA BIKIN TABEL ANOVA YANG LAIN
library(gdata)
SS=rbind(SSR,SSRES,SST)
df=as.matrix(rbind(k,n-k-1,n-1))
MS=rbind(MSREG,MSE)
anova=cbindX(SS,df,MS,f0,p);anova
rownames(anova)=source
colnames(anova)=c("SS","DF","MS","FHIT","P Value")
anova
View(anova)

#PARTIAL
CJJ=diag(solve(t(X) %*% X))
se=as.matrix(sqrt(CJJ%*%MSE));se
SE =as.matrix( t(se));SE
thit=beta/t(se);thit

#P value
j=k+1
for(i in 1:j){
  if(thit[i]<0){
    p[i] = 2*pt(thit[i],n-k-1)#df error
  } else{
    p[i] = 2*(1-pt(thit[i],n-k-1))
  }
}
pval=as.matrix(t(p));pval
pvalue=t(pval);pvalue

#Tabel ANOVA
library(gdata)
Anova = cbindX(beta,SE,thit,pvalue)
rownames(Anova) = c("Intercept","Youtube","Facebook","Newspaper")
colnames(Anova) = c("Estimate","SE","Tvalue","Pvalue")
Anova


#latihan week 5

library(readxl)
latihan <- read_excel("C:/Users/Nazia Mahmudah/Downloads/SEMESTER 5/NGASDOS KOMSTAT 2023/DATA LATIHAN SOAL KOMSTAT WEEK 5.xlsx",sheet = "data")
View(latihan)
x1=latihan$`Pend miskin (Ribu Jiwa)`
x2=latihan$`Upah Min (Ratus Ribu Rupiah)`
x3=latihan$TPT
Y = latihan$IPM;Y
X <- cbind(rep(1, length(Y)), x1, x2, x3);X
x = cbind(x1, x2, x3)
regres = lm(Y~x,data=latihan)
summary(regres)
#balik ke awal 

library(car)
library(datarium)
library(ggfortify)
data = marketing
head(data)

