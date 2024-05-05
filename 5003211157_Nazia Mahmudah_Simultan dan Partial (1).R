#Nazia Mahmudah 5003211157
#week 5 (gabungan simultan dan partial)
install.packages("datarium")
library(datarium)
data = marketing
data
Y <- data$sales
x1 <- data$youtube
x2 <- data$facebook
x3 <- data$newspaper
# Menambahkan kolom 1 untuk konstanta
X <- cbind(rep(1, length(Y)), x1, x2, x3) ;X
beta <- solve(t(X) %*% X) %*% t(X) %*% Y;beta
lm(Y~X,data=data)
#simultan
n=length(Y)
k=ncol(X) 
SSR=t(beta)%*%t(X)%*%Y-sum(Y)^2/n
SST=t(Y)%*%Y-sum(Y)^2/n
SSRES=SST-SSR;SSRES
MSE=SSRES/(n-k-1)
MSREG=SSR/k
f0=MSREG/MSE
p=2*(1-pf(f0,k,n-k-1));p
#buat Tabel ANOVA SIMULTAN
SS=c(SSR,SSRES,SST)
df=c(k,n-k-1,n-1)
MS=c(MSREG,MSE,NA)
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
se=sqrt(MSE%*%CJJ)
SE =as.matrix( t(se));SE
thit=beta/SE;thit

#P value
j=k+1
for(i in 1:j){
  if(thit[i]<0){
    p[i] = 2*pt(thit[i],n-k-1)
  } else{
    p[i] = 2*(1-pt(thit[i],n-k-1))
  }
}
pval=as.matrix(t(p))
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

#balik ke awal 