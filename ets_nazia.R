#### Number 1 ####
set.seed(13)
x1 <- rnorm(50, mean = 5, sd = 1)
x2 <- rchisq(50, df = 49)
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
#### Number 2 ####
proporsi <- function(input_data){
  n <- length(input_data)
  prop = 0
  for(i in 1:n){
    if(input_data[i] == 0){
      prop = prop + 1
    }
    else{
    }
  }
  proporsi = prop / n
  list(proporsi = proporsi)
}
proporsi(finale$y)

statdes <- function(input_data){
  n <- length(input_data)
  mean <- 0
  for(i in 1:n){
    mean = mean + input_data[i]
  }
  mean <- mean / n
  
  var <- 0
  for(j in 1:n){
    var = var + ((input_data[j]-mean)^2)
  }
  varians = var/(n-1)
  
  list(rata_rata = mean, varians = varians)
}
statdes(finale$x1)
statdes(finale$x2)

#### Number 3 ####
uji_proporsi <- function(p0,pbar,alpha,n){
  cat("H0: p <> p0","\n")
  cat("H1: p = p0","\n")

  z = (pbar-p0)/sqrt(p0*(1-p0)/n)
  
  z.half.alpha <- qnorm(1-alpha/2)
  
  pval <- 2*pnorm(z, lower.tail = T)
  
  if(pval < alpha){
    cat("=====================================================","\n")
    cat("Reject H0","\n")
  }
  else{
    cat("=====================================================","\n")
    cat("Fail to Reject H0","\n")
  }
  
  list(z.hitung = z, z.tabel = c(-z.half.alpha,z.half.alpha), pvalue = pval)
}
prop_0 <- proporsi(finale$y)
prop_1 <- 1-prop_0$proporsi
uji_proporsi(0.8,prop_1,0.05,nrow(finale))

x2_1 <- subset(finale$x2,finale$y == 1)
x2_0 <- subset(finale$x2,finale$y == 0)

uji_varians = function(data1,data2,alpha,pilihan){
  n1 = length(data1)
  n2 = length(data2)
  s1 = var(data1)
  s2 = var(data2)
  v1 = n1-1
  v2 = n2-1
  
  fhit = s1/s2
  
  if(pilihan==1){
    cat("Lower Tail Test","\n")
    cat("H0: sigma1 < sigma2","\n")
    cat("H1: sigma1 >= sigma2","\n")
    f.alpha = qf(1-alpha, df1 = v1, df2 = v2)
    pval = pf(fhit, df1 = v1, df2 = v2)
  }
  else if(pilihan==2){
    cat("Upper Tail Test","\n")
    cat("H0: sigma1 > sigma2","\n")
    cat("H1: sigma1 <= sigma2","\n")
    f.alpha = qf(1-alpha, df1 = v1, df2 = v2, lower.tail = F)
    pval = pf(fhit, df1 = v1, df2 = v2, lower.tail = F)
  }
  else if(pilihan==3){
    cat("Two Tail Test","\n")
    cat("H0: sigma1 <> sigma2","\n")
    cat("H1: sigma1 = sigma2","\n")
    f.alpha = c(qf(alpha/2, df1 = v1, df2 = v2),
                qf(1-alpha/2, df1 = v1, df2 = v2))
    pval = 2*(1-pf(fhit, df1 = v1, df2 = v2))
  }
  else{
    cat("Invalid choice")
  }
  
  if (pval <= alpha){
    cat("===========================","\n")
    cat("pvalue =",pval,"\n")
    cat("Keputusan : Tolak H0","\n")
    cat("===========================","\n")
  }
  else{
    cat("===========================","\n")
    cat("pvalue =",pval,"\n")
    cat("Keputusan : GagalTolak H0","\n")
    cat("===========================","\n")
  }
  
  list(f.hitung = fhit, f.tabel = f.alpha, pvalue = pval)
}
uji_varians(x2_1,x2_0,0.05,3)

#tolak H0
#varians sama

uji_t_varsama = function(data1,data2,d0,alpha,pilihan){
  s1 = var(data1)
  n1 = length(data1)
  s2 = var(data2)
  n2 = length(data2)
  xbar1 = mean(data1)
  xbar2 = mean(data2)
  
  df = n1+n2-2
  
  sp = (((n1-1)*s1)+((n2-1)*s2))/df
  
  t_hit <- ((xbar1-xbar2)-d0)/((sqrt(sp))*(sqrt((1/n1)+(1/n2))))
  
  if(pilihan==1){
    cat("Lower tail test","\n")
    cat("H0: mu1-mu2 < d0","\n")
    cat("H1: mu1-mu2 >= d0","\n")
    t.alpha = -(qt(1-alpha, df = df))
    pval = pt(t_hit, df = df)
  }
  else if(pilihan==2){
    cat("Upper tail test","\n")
    cat("H0: mu1-mu2 > d0","\n")
    cat("H1: mu1-mu2 <= d0","\n")
    t.alpha = qt(1-alpha, df = df)
    pval = pt(t_hit, df = df, lower.tail = F)
  }
  else if(pilihan==3){
    cat("Two Tailed Test","\n")
    cat("H0 : mu <> mu0","\n")
    cat("H0 : mu = mu0","\n")
    t.half.alpha = qt(1-alpha/2, df = df) 
    t.alpha = c(-t.half.alpha, t.half.alpha) 
    pval = 2*pt(t_hit, df = df, lower.tail = F) 
  }
  else{
    cat("invalid choice")
  }
  if (pval <= alpha){
    cat("===========================","\n")
    cat("pvalue =",pval,"\n")
    cat("Keputusan : Tolak H0","\n")
    cat("===========================","\n")
  }
  else{
    cat("===========================","\n")
    cat("pvalue =",pval,"\n")
    cat("Keputusan : GagalTolak H0","\n")
    cat("===========================","\n")
  }
  
  list(t.hitung = t_hit, t.tabel = t.alpha, pvalue = pval)
}
uji_t_varsama(x2_1,x2_0,0,0.05,2)

#### Number 4 ####
inc= c(62,72,75,55,64,21,64,80,67,72,42,76,76,41,48,76,53,60,42,78,
       29,48,55,29,21,47,81,36,22,44,15,7,42,9,21,21,16,16,9,14,12,17,7,34,8)
edu= c(86,76,92,90,86,84,93,100,87,86,74,98,97,84,91,34,45,56,44,
       82,72,55,71,50,23,39,28,32,22,25,29,7,26,19,15,20,26,28,17,22,30,25,20,47,32)
pre= c(82,83,90,76,90,87,93,90,52,88,57,89,97,59,73,38,76,81,45,
       92,39,34,41,16,33,53,67,57,26,29,10,15,19,10,13,24,20,7,3,16,6,11,8,41,10)
# variabel dependent
y= pre;y
# variabel independent
x= cbind(inc,edu);x

#a
reg.a= function(y,x){
  
  X= cbind(1,x)
  b= (solve(t(X)%*%X))%*%(t(X)%*%y)
  
  #output
  cat("Persamaan regresi :\n")
  cat("y =",b[1],"+",b[2],"x1 +",b[3],"x2")
}
reg.a(y,x)
# Bukti
lm(y~x)

#b dan c
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
  
  #b
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
  cat("estimator  b0,b1,dan b2 hasil bootstrap :\n")
  cat(bnew[1],bnew[2],bnew[3],"\n")
  Mbnew= matrix(c(bnew[1],bnew[2],bnew[3]),nrow=1,ncol=3,byrow=T)
  
  #c
  df= nrow(X)-ncol(X)
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
reg.b(y,x,100,20)

#d
print ('Perbedaan regresi konvensional dengan bootstrap : Regresi bootstrap memiliki bias tertentu terhadap regresi konvensional, karena hasil dari regresi bootstrap ini merupakan hasil rata-rata dari pengulangan random hasil regresi konvensional.')
