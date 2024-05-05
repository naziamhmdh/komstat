#MEAN BOOTSTRAP
set.seed(100)
data=rnorm(100,0,1)
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
  list(mean=mean(data), meanboostrap=meanB, e=bias, seB=seB)
}
bootstrap(data, 100, 1000)

#Bootstrap Regression
y=rnorm(10,0,1)
x1=rchisq(10,9,0)
x2=rbinom(10,5,0.6)
x= cbind(x1,x2);x

reg = lm(y~x)
summary(reg)
e = reg$residuals;e

reg.b= function(y,x,B,n){
  n= nrow(x)
  X= cbind(1,x)
  m= ncol(X)
  xty= t(X)%*%y
  xtx= t(X)%*%X
  yty= t(y)%*%y
  b= solve(xtx)%*%xty
  ypred= X%*%b
  e= y-ypred
  
  boot= matrix(NA,nrow=B,ncol=m)
  for (i in 1:B) {
    esamp= sample(e,n,replace = T)
    ybaru= esamp+ypred
    xtybaru= t(X)%*%ybaru
    bstr= solve(xtx)%*%xtybaru
    for (j in 1:m) {
      boot[i,j]= bstr[j]
    }
  }
  bnew= 0
  for (i in 1:m) {
    bnew[i]= mean(boot[,i])
  }
  Mbnew= matrix(c(bnew[1],bnew[2],bnew[3]),nrow=1,ncol=3,byrow=T)
  
  df= nrow(X)-ncol(X)
  SSE= (yty-Mbnew%*%xty)
  MSE= SSE/df
  MSE= as.numeric(MSE)
  Vb= solve(xtx)*MSE
  sb= diag(Vb)
  ttab= qt(1-0.05/2,df)
  upper= bnew+ ttab*sqrt(sb)
  lower= bnew- ttab*sqrt(sb)
  CI= cbind(bnew,lower,upper)
  cat("CI  95%\t Bboots \t lower\t\tupper\n")
  cat("b0\t",bnew[1],"\t",lower[1],"\t",upper[1],"\n")
  cat("b1\t",bnew[2],"\t",lower[2],"\t",upper[2],"\n")
  cat("b2\t",bnew[3],"\t",lower[3],"\t",upper[3],"\n")
  cat(b,"\n")
  std.boot = sd(b-bnew)
  cat(std.boot)
}
reg.b(y,x,1000,10)

#UJI HIPOTESIS
boot = function(data,R,m,uji)
{
  N = length(data)
  rata_sample = matrix(nrow=R, ncol=1)
  
  for (i in 1:R)
  {
    data_sample = sample(data,m,replace=T)
    rata_sample[i] = mean(data_sample)
  }
  rata_bootstrap = mean(rata_sample)
  
  cat("===================================","\n")
  cat("hasil rata-rata bootstrap",rata_bootstrap,"\n")
  cat("===================================","\n")
  
  sd_boot = sd(rata_sample)
  N_boot = R
  
  BB = rata_bootstrap - 1.96*(sd_boot/sqrt(N_boot))
  BA = rata_bootstrap + 1.96*(sd_boot/sqrt(N_boot))
  
  if(uji >= BB & uji <= BA)
  {
    cat("Kesimpulan = Gagal Tolak H0","\n")
  }
  else
  {
    cat("Kesimpulan = Tolak H0","\n")
  }
  
  hist(as.vector(rata_sample))
  list(rata_bootstrap=rata_bootstrap, sd=sd_boot, batas_bawah=BB, batas_atas=BA)
  
}

#BOOTSTRAP CORRELATION
x=rnorm(100,0,1)
y=rchisq(100,1)
data=cbind(x,y)
head(data)

bootkor = function(data,m,R)
{
  n = nrow(data)
  kor = matrix(nrow=1, ncol=R)
  for (i in 1:R)
  {
    urut = c(1:n)
    posisi = sample(urut,m,replace=T)
    data_new=data[posisi,]
    kor[,i] = cor(data_new[,1],data_new[,2])
  }
  kor
  korelasi_boot = mean(kor)
  korelasi=cor(data[,1],data[,2])
  bias= korelasi_boot-korelasi
  hist(kor)
  list(korboot = korelasi_boot, korelasi=korelasi,bias=bias)
  
}
bootkor(data,100,1000)
