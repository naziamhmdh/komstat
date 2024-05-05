#uji t gabungan 1 populasi

t_test <- function(data,mu0,alpha,pilihan){
  n <- length(data)
  s <- sd(data)
  xbar <- mean(data)
  t <- (xbar-mu0)/(s/sqrt(n))
  
  if(pilihan == 1){
    cat("Lower Tail Test","\n")
    cat("H0 : mu < mu0","\n")
    cat("H1 : mu >= mu0","\n")
    t.alpha <- qt(1-alpha, df=n-1)
    pval <- pt(t, df=n-1)
  }
  else if(pilihan == 2){
    cat("Upper Tail Test","\n")
    cat("H0 : mu > mu0","\n")
    cat("H1 : mu <= mu0","\n")
    t.alpha <- qt(1-alpha, df = n-1, lower.tail = F)
    pval <- pt(t, df=n-1, lower.tail = F)
  }
  else if(pilihan == 3){
    cat("Two Tailed Test","\n")
    cat("H0 : mu <> mu0","\n")
    cat("H0 : mu = mu0","\n")
    t.alpha <- qt(1-alpha/2, df = n-1)
    pval <- 2*pt(t, df=n-1)
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
  
  list(t.hitung = t, t.tabel = t.alpha, pvalue = pval)
}


t_test(data$verb,90,0.05,1)
