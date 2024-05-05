#Nazia Mahmudah 5003211157
#Tugas 1 praktikum komstat dengan asdos
#Kelas Komputasi Statistika C
#Tugas membuat algoritma untuk : correlation

#Membuat Data===================================================================
x = sample(10)
y=sample(10)
p= length(x)
p
q=length(y)
q
#Rumus correlation==============================================================
sxy=0
sx=0
sy=0
sx2=0
sy2=0
for(i in 1:p){
  xy=x[i]*y[i]
  sxy=sxy+xy
  sx=sx+x[i]
  sy=sy+y[i]
  x2=x[i]^2
  sx2=sx2+x2
  y2=y[i]^2
  sy2=sy2+y2
}
correlate=((p*sxy)-(sx*sy))/(sqrt(p*sx2-sx^2)*sqrt(p*sy2-sy^2))
correlate
#Pembuktian=====================================================================
cor(x,y)


##Cara 2 dengan covariance======================================================
#Membuat Data===================================================================
cordata = data.frame(x,y)
cordata
#Membuat function covariance untuk memudahkan menghitung correlation============
covar=0
for(i in cordata){
covar <- function(x,y=x) mean(x*y) - mean(x)*mean(y)}
#Rumus correlation==============================================================
correlation <- function(x,y) covar(x,y) / sqrt(covar(x) * covar(y))
correlation(cordata$x, cordata$y)
#Pembuktian=====================================================================
cor(cordata$x, cordata$y)
