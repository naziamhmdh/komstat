# Praktikum Komstat D
# 12/9/2023
# Uji Hipotesis

setwd("C:/Users/darre/Desktop/ASDOS KOMSTAT/W3")
#### UJI HIPOTESIS ####
#### Two-tail variation
sigma=0.9     #Hypothesized value
n=10          #Sample size
s=1.2         #Sample standar deviation
alpha=0.05
chi=(n-1)*(s^2)/(sigma^2)                     #Statistics
c.kiri=qchisq(alpha/2,df=n-1)
c.kanan=qchisq(1-alpha/2,df=n-1)              #Critical value
p=2*pchisq(chi, df=n-1, lower.tail=FALSE)     #P-value
LL=sqrt((n-1)*(s^2)/(qchisq(1-alpha/2, df=n-1)))      #Lower limit
UL=sqrt((n-1)*(s^2)/(qchisq(alpha/2, df=n-1)))        #Upper limit
if(p<alpha){
  keputusan="Tolak H0"
} else
{
  keputusan="Gagal Tolak H0"
}
list(Statistics=chi, chitab=c(c.kiri, c.kanan), Pval=p, Keputusan=keputusan, LL=LL, UL=UL)


#### Mean with known variance 
# H0 = mu >= mu0
# H1 = mu < mu0

xbar <- 9900
mu0 <- 10000
sigma <- 120
n <- 30
z <- (xbar-mu0)/(sigma/sqrt(n))
z

#critcal value
alpha <- 0.05
z.alpha <- qnorm(1-alpha)
-z.alpha

pval <- pnorm(z);pval

{
  if(pval>alpha){
    print("gagal tolak h0")
  }
  else
    print("tolak h0")
}

#### Mean with unkown variance
# h0 = mu <= mu0
# h1 = mu > mu

xbar <- 2.1
mu0 <- 2
s <- 0.3
n <- 35
t <- (xbar-mu0)/(s/sqrt(n))
t

#critical value
a <- 0.05
t.alpha <- qt(1-a, df = n-1)
t.alpha

#pvalue
pval <- pt(t,df=n-1,lower.tail = F);pval

{
  if(pval>alpha){
    print("gagal tolak h0")
  }
  else
    print("tolak h0")
}
#### Proportion Test 
# H0 = p <> p0
# H1 = p = p0

pbar = 12/20
p0 = 0.5
n = 20
alpha = 0.05
z = (pbar-p0)/sqrt(p0*(1-p0)/n)

z.half.alpha = qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)

# p value
pval = 2*pnorm(z, lower.tail = FALSE);pval

{
  if(pval>alpha){
    print("gagal tolak h0")
  }
  else
    print("tolak h0")
}



#### Function ####
uji_hipotesis = function(xbar,mu0,alpha,n,sigma){
  z = (xbar-mu0)/(sigma/sqrt(n))
  z_alpha = qnorm(1-alpha)
  pval = pnorm(z)
  
  if (pval <= alpha){
    cat("============================","\n")
    cat("p-value :",pval,"\n")
    cat("Keputusan : Tolak H0","\n")
    cat("============================","\n")
  } else{
    cat("============================","\n")
    cat("p-value :",pval,"\n")
    cat("Keputusan : Gagal Tolak H0","\n")
    cat("============================","\n")
  }
  list (z.hitung = z, z.tabel = z_alpha, pvalue = pval)
}

uji_hipotesis(9900,10000,0.05,30,120)


#### Function Gabungan ####
#### STUDY CASE ####
###SC 1 Left-side Z-test
mu0=16.43      #Hypothesized value
sigma=0.8      #Sigma
n=15           #Sample size
xbar=16        #Sample mean
alpha=0.05     
z=(xbar-mu0)/(sigma/sqrt(n))     #Statistics
z.test=qnorm(alpha)    #Critical value
p=pnorm(z)             #P-value
UL=xbar+(qnorm(1-alpha)*(sigma/sqrt(n)))    #Upper limit
if(p<alpha){
  keputusan="Reject H0"
} else
{
  keputusan="Do not Reject H0"
}
list(zhitung=z, Ztab=z.test, Pval=p, Keputusan=keputusan, UL=UL)

##### STUDY CASE 2
#####Two-tail t-test
mu0=8            #Hypothesized value
n=35             #Sample size
xbar=7.91        #Sample mean
s2=0.03          #Sample varians
s=sqrt(s2)       #Sample standar deviation
alpha=0.01
case2 = function(n,mu0,xbar,s2,alpha){
s=sqrt(s2)       #Sample standar deviation
t=(xbar-mu0)/(s/sqrt(n))        #Statistics
t.half=qt(1-alpha/2, df=n-1)    #Critical value
#c(-t.half, t.half)    
p=2*pt(t, df=n-1)               #P-value
LL=xbar-(qt(1-alpha/2, df=n-1))*(s/sqrt(n))     #Lower limit
UL=xbar+(qt(1-alpha/2, df=n-1))*(s/sqrt(n))     #Upper limit
if(p<alpha)
{
  keputusan="Reject H0"
} else
{
  keputusan="Do not Reject H0"
}
list(thitung=t, ttab=t.half, Pval=p, Keputusan=keputusan, LL=LL, UL=UL)
}
case2(35,8,7.91,0.03,0.01)


#### STUDY CASE 3
#####Right-side t-test
x=c(4,5,5,4,4,3,6,4,3,3,5,5,6,3,3,2,7,4,5,2,3,3,3,4)
case3 = function(x,mu0){
n=length(x);n              #Sample size
xbar=mean(x);xbar          #Sample mean
s=sd(x);s                  #Sample standar deviation
alpha=0.05
t=(xbar-mu0)/(s/sqrt(n));t    #Statistics
t.test=qt(alpha, df=n-1)  ;t.test    #Critical value
p=1-pt(t, df=n-1)             #P-value
LL=xbar-(qt(1-alpha, df=n-1))*(s/sqrt(n))     #Lower limit
if(p<alpha) {
  keputusan="Reject H0"
} else
{
  keputusan="Do not Reject H0"
}
list(thitung=t, ttab=t.test, Pval=p, Keputusan=keputusan, LL=LL)
}
case3(x,3.18)

