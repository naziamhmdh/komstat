#uji proporsi 1 populasi

data= read_excel("C:/Users/Nazia Mahmudah/Downloads/SEMESTER 5/NGASDOS KOMSTAT 2023/data proporsi.xlsx")

prop_test = function(data,p0,alpha,pilihan){
  n = length(data) 
  pbar = sum(data$Puas)/n
  z = (pbar-p0)/sqrt(p0*(1-p0)/n) 
  
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
    cat("Keputusan : Gagal Tolak H0","\n")
    cat("===========================","\n")
  }
  list(z.hitung = z, z.tabel = z.alpha, pvalue = pval)
}

prop_test(data,0.5,0.05,3)
