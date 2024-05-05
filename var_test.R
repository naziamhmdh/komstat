# uji varians

var_test = function(data,alpha,sigma0,pilihan){
  n <- length(data)
  s <- sd(data)
  sigma0=155
  chi <- (n-1)*(s^2)/(sigma0^2);chi
  
  if(pilihan == 1){
    cat("Lower Tail Test")
    cat("H0 : sigma < sigma0")
    cat("H1 : sigma >= sigma0")
    chi.alpha <- qchisq(1-alpha, df = n-1)
    pval <- pchisq(chi, df = n-1)
  }
  else if(pilihan == 2){
    cat("Upper Tail Test")
    cat("H0 : sigma > sigma0")
    cat("H1 : sigma <= sigma0")
    chi.alpha <- qchisq(1-alpha, df = n-1, lower.tail = F)
    pval <- pchisq(chi, df = n-1, lower.tail = F)
  }
  else if(pilihan == 3){
    cat("Two Tail Test")
    cat("H0 : sigma <> sigma0")
    cat("H0 : sigma = sigma0")
    chi.alpha <- qchisq(1-alpha/2, df = n-1)
    pval <- 2*pchisq(chi, df = n-1, lower.tail = F)
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
  
  list(chi.hitung = chi, chi.tabel = chi.alpha, pvalue = pval)
}
var_test(QUIZ$BMI,0.05,155,1)
