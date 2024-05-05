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


