library(datasets)
data <- mtcars

attach(data)
x <- subset(mpg,am==1)
y <- subset(mpg,am==0)

##### uji z varians diketahui #####
uji_z <- function(data1,data2,d0,alpha,pilihan){
  s1 = var(data1)
  n1 = length(data1)
  s2 = var(data2)
  n2 = length(data2)
  xbar1 = mean(data1)
  xbar2 = mean(data2)
  
  zhit = ((xbar1-xbar2)-d0)/sqrt((s1/n1)+(s2/n2))
  
  if(pilihan==1){
    cat("Lower tail test","\n")
    cat("H0: mu1-mu2 < d0","\n")
    cat("H1: mu1-mu2 >= d0","\n")
    z.alpha = -(qnorm(1-alpha))
    pval = pnorm(zhit)
  }
  else if(pilihan==2){
    cat("Lower tail test","\n")
    cat("H0: mu1-mu2 < d0","\n")
    cat("H1: mu1-mu2 >= d0","\n")
    z.alpha = qnorm(1-alpha)
    pval = pnorm(zhit, lower.tail = F)
  }
  else if(pilihan==3){
    cat("Two Tailed Test","\n")
    cat("H0 : mu <> mu0","\n")
    cat("H0 : mu = mu0","\n")
    z.half.alpha = qnorm(1-alpha/2) 
    z.alpha = c(-z.half.alpha, z.half.alpha) 
    pval = 2*pnorm(zhit, lower.tail = F) 
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
  
  list(z.hitung = zhit, z.tabel = z.alpha, pvalue = pval)
}

uji_z(x,y,0,0.05,3)

##### uji t varians tdk diketahui, sama #####
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
    cat("H0: mu1-mu2 < d0","\n")
    cat("H1: mu1-mu2 >= d0","\n")
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
uji_t_varsama(x,y,0,0.05,3)
t.test(x, y, "two.sided",mu=0, paired = F,var.equal=T, 0.95)
##### uji t var tdk diketahui, beda #####
uji_t_varbeda = function(data1,data2,d0,alpha,pilihan){
  s1 = var(data1)
  n1 = length(data1)
  s2 = var(data2)
  n2 = length(data2)
  xbar1 = mean(data1)
  xbar2 = mean(data2)
  
  df = ((s1/n1+s2/n2)^2)/(((s1/n1)^2/(n1-1))+((s2/n2)^2/(n2-1)))
  
  t_hit <- ((xbar1-xbar2)-d0)/sqrt((s1/n1)+(s2/n2))
  
  if(pilihan==1){
    cat("Lower tail test","\n")
    cat("H0: mu1-mu2 < d0","\n")
    cat("H1: mu1-mu2 >= d0","\n")
    t.alpha = -(qt(1-alpha, df = df))
    pval = pt(t_hit, df = df)
  }
  else if(pilihan==2){
    cat("Upper tail test","\n")
    cat("H0: mu1-mu2 < d0","\n")
    cat("H1: mu1-mu2 >= d0","\n")
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
uji_t_varbeda(x,y,0,0.05,3)
t.test(x, y, "two.sided",mu=0, paired = F,var.equal=F, 0.95)


#### uji berpasangan ####
library(readxl)
setwd("C:/Users/Nazia Mahmudah/Downloads")
data4 <- read_excel("C:/Users/Nazia Mahmudah/Downloads/Data W4.xlsx")
x = subset(data4$Data,data4$Group==1);x
y = subset(data4$Data,data4$Group==2);y
selisih=x-y

uji_pasangan = function(data,d0,alpha,pilihan){
  n = length(data)
  sd = sd(data)
  dbar = mean(data)
  
  t_hit = (dbar-d0)/(sd/sqrt(n))
  
  if(pilihan == 1){
    cat("Lower Tail Test","\n")
    cat("H0 : mud < d0","\n")
    cat("H1 : mud >= d0","\n")
    t.alpha <- qt(1-alpha, df=n-1)
    pval <- pt(t_hit, df=n-1)
  }
  else if(pilihan == 2){
    cat("Upper Tail Test","\n")
    cat("H0 : mud > d0","\n")
    cat("H1 : mud <= d0","\n")
    t.alpha <- qt(1-alpha, df = n-1, lower.tail = F)
    pval <- pt(t_hit, df=n-1, lower.tail = F)
  }
  else if(pilihan == 3){
    cat("Two Tailed Test","\n")
    cat("H0 : mud <> d0","\n")
    cat("H0 : mud = d0","\n")
    t.half.alpha = qt(1-alpha/2, df = n-1)
    t.alpha <- c(-t.half.alpha, t.half.alpha)
    pval <- 2*pt(abs(t_hit), df=n-1,lower.tail = FALSE)
  }
  else{
    print("Invalid Choice")
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
uji_pasangan(selisih,0,0.05,3)

t.test(x, y, "two.sided",mu=0, paired = TRUE, 0.95)

#### uji proporsi ####

uji_prop = function(x1,x2,alpha,pilihan,n1,n2){
  p1 = x1/n1
  p2 = x2/n2
  phat = (x1+x2)/(n1+n2)
  
  zhit = (p1-p2)/sqrt(phat*(1-phat)*((1/n1)+(1/n2)))
  
  if(pilihan==1){
    cat("Lower tail test","\n")
    cat("H0: p1 < p2","\n")
    cat("H1: p1 >= p2","\n")
    z.alpha = -(qnorm(1-alpha))
    pval = pnorm(zhit)
  }
  else if(pilihan==2){
    cat("Upper tail test","\n")
    cat("H0: p1 > p2","\n")
    cat("H1: p1 <= p2","\n")
    z.alpha = qnorm(1-alpha)
    pval = pnorm(zhit, lower.tail = F)
  }
  else if(pilihan==3){
    cat("Two Tailed Test","\n")
    cat("H0 : p1 <> p2","\n")
    cat("H0 : p1 = p2","\n")
    z.half.alpha = qnorm(1-alpha/2) 
    z.alpha = c(-z.half.alpha, z.half.alpha) 
    pval = 2*pnorm(zhit, lower.tail = F) 
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
  
  list(z.hitung = zhit, z.tabel = z.alpha, pvalue = pval)
}

uji_prop(5,6,0.05,3,11,11)
prop.test

#### uji varians ####
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
uji_varians(x,y,0.05,3)
var.test(x,y,1,"two.side",0.95)
