
####Nomor 1####
x1= rnorm(50,5,1)
x2= rchisq(50,49)
y <- matrix(0,nrow = 50, ncol = 1)
mean_x1 <- mean(x1)
mean_x2 <- mean(x2)
for(i in 1:50){
  if(x1[i] <= mean_x1 && x2[i] > mean_x2){
    y[i] = 0
  }
  else{
    y[i] = 1
  }
}
finale <- data.frame(x1,x2,y);finale

####Nomor 2####
yg0= sum(df$y==0);yg0
n= length(y);n
prop= yg0/n;prop

#rata-rata
jml1=0;jml2=0
for (i in 1:n) {
  jml1= jml1+x1[i]
  jml2= jml2+x2[i]
}
avgx1= jml1/n;avgx1
avgx2= jml2/n;avgx2
#cek
#mean(x1);mean(x2)

#varians
sig1=0;sig2=0
for (i in 1:n) {
  sig1= sig1+ (x1[i]-avgx1)^2
  sig2= sig2+ (x2[i]-avgx2)^2
}
varx1= sig1/(n-1);varx1
varx2= sig2/(n-1);varx2

#cek
var(x1);var(x2)


####Nomor 3####
#a
uji.prop= function(x,p0,alpha){
  n= length(x)
  tot=0
  for (i in 1:n) {
    tot= tot+x[i]
  }
  p= tot/n
  
  zstat= (p-p0)/sqrt(p0*(1-p0)/n)
  pvalue= 2*(1-pnorm(abs(zstat)))
  if (pvalue<alpha) {
    conc= "Tolak h0"
  } else{
    conc= "Gagal tolak H0"
  }
  cat("Uji proporsi 1 populasi\n")
  cat("H1 : p != ",p0,"\n")
  cat("z-value = ",zstat,"\n")
  cat("p-value = ",pvalue,"\n")
  cat(conc)
}
uji.prop(df$y,0.8,0.05)

#b
x2yg1= df[df$y==1,'x2'];x2yg1
x2yg0 = df[df$y==0,'x2'];x2yg0

two.sample.ttest.greater= function(pop1,pop2,alpha){
  #uji dulu variannya sama atau ngga
  s1= var(pop1);s2= var(pop2)
  n1= length(pop1); n2= length(pop2)
  f= s1/s2
  pvalue1= 2*(1-pf(f,df1= n1-1,df2= n2-1))
  if (pvalue1<alpha) {
    conc1= "tidak sama"
  } else{
    conc1= "sama"
  }
  mu1= mean(pop1);mu2= mean(pop2)
  
  #Sp
  pembilang= (n1-1)*s1+(n2-1)*s2
  Sp= sqrt(pembilang/(n1+n2-2))
  
  #thitung dan df
  selisih= mu1-mu2
  if(conc1== "tidak sama"){
    title= "Welch two sample t-test\n"
    t= selisih/sqrt((s1/n1)+(s2/n2))
    v= ((s1/n1+s2/n2)^2)/(((s1/n2)^2/(n1-1))+((s2/n2)^2/(n2-1)))
  } else{
    title= "Two sample t-test\n"
    t= selisih/(Sp*sqrt((1/n1)+(1/n2)))
    v= n1+n2-2
  }
  
  #pvalue
  pvalue2= pt(abs(t),df=v)
  
  #conclusion
  if (pvalue2<alpha) {
    conc2= "Tolak H0"
  } else{
    conc2= "Gagal tolak H0"
  }
  
  #output
  cat(title)
  cat("t= ",t,"\tdf= ",v,"\tp-value =",pvalue2,"\n")
  cat("H1 : rata-rata x2 dalam kategori1 tidak lebih besar daripada kategori0 pada y\n")
  cat(conc2)
}
two.sample.ttest.greater(x2yg1,x2yg0,0.05)

####Nomor 4####
inc= c(62,72,75,55,64,21,64,80,67,72,42,76,76,41,48,76,53,60,42,78,
       29,48,55,29,21,47,81,36,22,44,15,7,42,9,21,21,16,16,9,14,12,17,7,34,8)
edu= c(86,76,92,90,86,84,93,100,87,86,74,98,97,84,91,34,45,56,44,
       82,72,55,71,50,23,39,28,32,22,25,29,7,26,19,15,20,26,28,17,22,30,25,20,47,32)
pre= c(82,83,90,76,90,87,93,90,52,88,57,89,97,59,73,38,76,81,45,
       92,39,34,41,16,33,53,67,57,26,29,10,15,19,10,13,24,20,7,3,16,6,11,8,41,10)
yreg= pre;yreg
xreg= cbind(inc,edu);xreg

#a
reg.a= function(y,x){
  
  Mx= cbind(1,x)
  xty= t(Mx)%*%y
  xtx= t(Mx)%*%Mx
  b= solve(xtx)%*%xty
  
  #output
  cat("Persamaan regresi :\n")
  cat("y =",b[1],"+",b[2],"x1 +",b[3],"x2")
}
reg.a(yreg,xreg)

#b dan c
reg.b= function(y,x,berapa.repetisi,berapa.sample){
  n= nrow(x)
  Mx= cbind(1,x)
  m= ncol(Mx)
  xty= t(Mx)%*%y
  xtx= t(Mx)%*%Mx
  yty= t(y)%*%y
  b= solve(xtx)%*%xty
  
  ypred= Mx%*%b
  e= y-ypred
  
  #b
  boot= matrix(NA,nrow=berapa.repetisi,ncol=m)
  for (i in 1:berapa.repetisi) {
    esamp= sample(e,n,replace = T)
    ystr= esamp+ypred
    xtystr= xty= t(Mx)%*%ystr
    bstr= solve(xtx)%*%xtystr
    for (j in 1:m) {
      boot[i,j]= bstr[j]
    }
  }
  bnew= 0
  for (i in 1:m) {
    bnew[i]= mean(boot[,i])
  }
  cat("estimator  b0,b1,dan b2 hasil bootstrap :\n")
  cat(bnew[1],bnew[2],bnew[3],"\n")
  Mbnew= matrix(c(bnew[1],bnew[2],bnew[3]),nrow=1,ncol=3,byrow=T)
  
  #c
  df= nrow(Mx)-ncol(Mx)
  SSE= (yty-Mbnew%*%xty)
  MSE= SSE/df
  MSE= as.numeric(MSE)
  Vb= solve(xtx)*MSE
  sb= diag(Vb)
  ttab= qt(1-0.05/2,df)
  upper= bnew+ ttab*sqrt(sb)
  lower= bnew- ttab*sqrt(sb)
  CI= cbind(lower,upper)
  cat("CI  95% \tlower\t\tupper\n")
  cat("b0\t",lower[1],"\t",upper[1],"\n")
  cat("b1\t",lower[2],"\t",upper[2],"\n")
  cat("b2\t",lower[3],"\t",upper[3],"\n")
}
reg.b(yreg,xreg,100,20)

#d
print ('Perbedaan regresi konvensional dengan bootstrap : Regresi bootstrap memiliki bias tertentu terhadap regresi konvensional, karena hasil dari regresi bootstrap ini merupakan hasil rata-rata dari pengulangan random hasil regresi konvensional.')



mer <- factor(rep(c("Ya","Tidak"),each = 2))
kan <- factor(rep(c("Ya","Tidak"),times = 2))
count <- c(3,0,1,3)
df <- data.frame(mer,kan,count)
df
p <- matrix(c(3,0,1,3),nrow = 2,ncol = 2,byrow = T)
p
fisher.test(p)
