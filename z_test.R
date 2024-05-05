#uji x gabungan 1 populasi

data = read.csv("C:/Users/Nazia Mahmudah/Downloads/SEMESTER 5/NGASDOS KOMSTAT 2023/data.csv",sep = ";")
data
z_test = function(data,mu0,alpha,sigmapop,pilihan){
  n <- length(data)
  xbar <- mean(data)
  z = (xbar-mu0)/(sigma/sqrt(n)) 
  
  if(pilihan==1){
    cat("Lower Tail Test","\n")
    cat("H0 : mu < mu0","\n")
    cat("H1 : mu >= mu0","\n")
    z.alpha = -(qnorm(1-alpha))
    pval = pnorm(z) 
  }
  else if (pilihan==2){
    cat("Upper Tail Test","\n")
    cat("H0 : mu > mu0","\n")
    cat("H1 : mu <= mu0","\n")
    z.alpha = qnorm(1-alpha)
    pval = pnorm(z, lower.tail=FALSE)
  }
    else if(pilihan == 3){
      cat("Two Tailed Test","\n")
      cat("H0 : mu <> mu0","\n")
      cat("H0 : mu = mu0","\n")
      alpha = .05 
      z.half.alpha = qnorm(1-alpha/2) 
      z.alpha = c(-z.half.alpha, z.half.alpha) 
      pval = 2*pnorm(z) 
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
  
  list(z.hitung = z, z.tabel = z.alpha, pvalue = pval)
  }
z_test(data$verb,90,0.05,155,3)

