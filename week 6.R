
#A menyimpan data dalam excel, lalu baca file excel tersebut di R
library(readxl)
QUIZ <- read_excel("C:/Users/Nazia Mahmudah/Downloads/SEMESTER 5/NGASDOS KOMSTAT 2023/QUIZ.xlsx")
View(QUIZ)
summary(QUIZ)
#B Buat matriks A yaitu matriks yang berisi data "Usia" dan "Jml Anak" dan matriks B yang berisi data "BMI" dan "Biaya Asuransi"
A = as.matrix(cbind(QUIZ$Usia,QUIZ$`Jml Anak`));A
B = as.matrix(cbind(QUIZ$BMI,QUIZ$`Biaya Ansuransi`));B
solve(A%*%t(B),tol=1e-20)

#MEAN BOOTSTRAP
set.seed(100)
data=rnorm(100,0,1)
data
bootstrap=function(data, n, B)
{
  ulang=matrix(nrow=n,ncol=B);ulang
  rata=matrix(nrow=1, ncol=B);rata
  for (i in 1:B)
  {
    ulang[,i]=sample(data, n, replace=T)
    rata[,i]=mean(ulang[,i])
  }
  rata
  ulang
  hist(rata)
  meanB=mean(rata)
  bias=meanB-mean(data)
  seB=sd(rata)
  list(mean=mean(data), meanB=meanB, bias=bias, seB=seB)
}
bootstrap(data, 100, 1000)

#contoh
library(readxl)
data3 <- read_excel("C:/Users/Nazia Mahmudah/Downloads/SEMESTER 5/NGASDOS KOMSTAT 2023/data week 6.xlsx");data3
View(data3)
meansample50 = matrix(nrow = 50, ncol = 1)

for(i in 1:50){
  databoot = sample(data3$weight,1000, replace = T)
  meansample50[i] = mean(databoot)
}
View(meansample50)
hist(data3$weight)
hist(meansample50)
summary(data3$weight)
summary(meansample50)
bias = mean(meansample50)-mean(data3$weight);bias

####BOOTSTRAP REGRESSION DARI RESIDUAL####
y= c(174,140,175,156,177,180,180,176,166,167,160,165) 
x1= c(31 ,20, 19 ,25 ,19, 22, 23 ,23 ,32, 41, 32 ,23)
x2= c(1,2,3,4,5,6,7,8,9,10,11,12)
x=cbind(x1,x2)
x
####Coba pake lm####
model= lm(y~x);model
yhatlm= model$coefficients[1]+model$coefficients[2]*x1+model$coefficients[3]*x2
elm= y-yhatlm;elm
elm=model$residuals
elm
b0barulm=0
b1barulm=0
b2barulm=0
for (i in 1:1000) {
  e.samp= sample(elm,length(y),T)
  e.samp
  ybarulm= e.samp+yhatlm
  ybarulm
  modellm= lm(ybarulm~x)
  modellm  
  b0barulm[i]= modellm$coefficients[1]
  b1barulm[i]= modellm$coefficients[2]
  b2barulm[i]= modellm$coefficients[3]
}
lm

#nilai B0 B1 B2 yang sudah di bootstrap
b0barulm
b1barulm
b2barulm
#nilai taksiran koefisien regresi dari metode bootstrap
estb0barulm= mean(b0barulm)
estb1barulm= mean(b1barulm)
estb2barulm= mean(b2barulm)
rbind(estb0barulm,estb1barulm,estb2barulm)
#confidence interval koefisien regresi melalui nilai persentile
CI1=quantile(b0barulm,c(0.025,0.975))
CI2=quantile(b1barulm,c(0.025,0.975))             
CI3=quantile(b2barulm,c(0.025,0.975)) 
rbind(CI1,CI2,CI3)

##REGRESI BASED OBSERVATION
x=rnorm(100,0,1)
y=rchisq(100,1)
data=cbind(x,y)
bootkoef = function(data, m, R)
{
  x = data[,1]
  y = data[,2]
  n = length(x)
  b1 = matrix(nrow=R, ncol=1)
  b0 = matrix(R,1)
  for (i in 1:R)
  {
    urut = c(1:n)
    posisi = sample(urut,m,replace=T)
    datax = x[posisi]
    datay = y[posisi]
    regresi = lm(datay~datax)
    b0[i] = regresi$coefficients[1]
    b1[i] = regresi$coefficients[2]
  }
  par(mfrow=c(1,2))
  hist(b0)
  hist(b1)
  beta_boot0 = mean(b0)
  beta_boot1 = mean(b1)
  C_bawah0 = beta_boot0 - (1.96*sd(b0)/sqrt(R))
  C_atas0 = beta_boot0 + (1.96*sd(b0)/sqrt(R))
  C_bawah1 = beta_boot1 - (1.96*sd(b1)/sqrt(R))
  C_atas1  = beta_boot1 + (1.96*sd(b1)/sqrt(R))
  list(betaboot = beta_boot0, CI_bawah0=C_bawah0, CI_atas0=C_atas0, 
       betaboot1 = beta_boot1, CI_bawah1=C_bawah1, CI_atas1=C_atas1)
}
bootkoef(data,1000,100)
