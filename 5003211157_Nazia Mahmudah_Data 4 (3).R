#Menyiapkan data================================================================
library(readxl)
setwd("C:/Users/Nazia Mahmudah/Downloads")
data4 <- read_excel("C:/Users/Nazia Mahmudah/Downloads/Data W4.xlsx");
View(data4)
pop1 = subset(data4$Data,data4$Group==1)
pop1
pop2 = subset(data4$Data,data4$Group==2)
pop2
#untuk mempermudah pengecekan karena berbeda working directory, berikut data pop 1 dan 2
pop1 =c(32,11,47,7,26,18,28,39,36,55,34)
pop2=c(19,13,21,8,20,15,21,30,42,25,29)


#Uji Varians 2 Populasi=========================================================
var2pop = function (pop1, pop2, d0, alpha)
{
  s12 = var(pop1)
  s22 = var(pop2)
  n1=length(pop1)
  n2=length(pop2)
  v1=n1-1
  v2=n2-1
  fhit=s12/s22
  ftab=c(qf(alpha/2, df1=v1, df2=v2),qf(1-alpha/2, df1=v1, df2=v2))
  ftab
  p=2*(1-pf(fhit, df1 = v1, df2 = v2))
  LL=(s12/s22)/qf(1-alpha/2,df1=v1,df2=v2)
  UL=(s12/s22)*qf(1-alpha/2,df1=v1,df2=v2)
  
  if(p<alpha) {
    kep1 = "Tolak Ho, maka terdapat perbedaan varians antar dua populasi"
  } else {
    kep1 = "Gagal Tolak Ho, maka tidak terdapat perbedaan varians antar dua populasi"
  }
  if (fhit<qf(alpha/2, df1=v1,df2=v2) | fhit>qf(1-(alpha/2), df1=v1,df2=v2)) {
    kep2 = "Tolak Ho, maka terdapat perbedaan varians antar dua populasi"
  } else {
    kep2 = "Gagal Tolak Ho, maka tidak terdapat perbedaan varians antar dua populasi"
  }
  cat("Uji Varians 2 Populasi", "\n")
  cat("H1                  : Var 1/Var 2 = 1", "\n")
  cat("H0                  : Var 1/Var 2 != 1", "\n")
  cat("F Hitung            : ", fhit, "\n")
  cat("F Tabel             : ", ftab, "\n")
  cat("P Value             : ", p, "\n") 
  cat((1-alpha)*100,"% CI             : (",LL,";",UL,")", "\n")
  cat("Keputusan P Val     : ", kep1, "\n")
  cat("Keputusan F hit     : ", kep2, "\n")
  
  
  if (kep1 == "Tolak Ho, maka terdapat perbedaan varians antar dua populasi") 
  { 
  mean1 = mean(pop1)
  mean2 = mean(pop2)
  thit = ((mean1-mean2)-d0)/(sqrt((s12/n1)+(s22/n2)))
  v=((s12/n1 + s22/n2)^2)/( (((s12/n1)^2)/(n1-1)) + (((s22/n2)^2)/(n2-1))  )
  ttab=abs(qt(alpha/2, df3))
  pval=2*(1-pt(abs(thit),v)) 
  UL=(mean1-mean2)-(qt(alpha/2,v))*(sqrt((s12/n1)+(s22/n2)))
  LL=(mean1-mean2)+(qt(alpha/2,v))*(sqrt((s12/n1)+(s22/n2)))
  
  if(pval<alpha) {
    kepp1 = "Tolak Ho"
  } else {
    kepp1 = "Gagal Tolak Ho"
  }
  if (thit>abs(ttab)) {
    kepp2 = "Tolak Ho"
  } else {
    kepp2 = "Gagal Tolak Ho"
  }
  
  cat("\n","Uji Mean 2 Populasi Varians tidak diketahui Berbeda", "\n")
  cat("H0                  : miu 1 - miu 2 = 0", "\n")
  cat("Ha                  : miu 1 - miu 2 != 0", "\n")
  cat("t Hitung            : ", thit, "\n")
  cat("|t Tabel|           : ", ttab, "\n")
  cat("P Value             : ", pval, "\n") 
  cat((1-alpha)*100,"% CI             : (",LL,";",UL,")", "\n")
  cat("Keputusan P Val     : ", kepp1, "\n")
  cat("Keputusan t hit     : ", kepp2, "\n")
  
  
  } else {
    
    mean1 = mean(pop1)
    mean2 = mean(pop2)
    
    df3=n1+n2-2
    sp2 = (((n1-1)*s12)+((n2-1)*s22))/df3
    thit = ((mean1-mean2)-d0)/((sqrt(sp2))*(sqrt((1/n1) + (1/n2))))
    ttab=abs(qt(alpha/2, df3))
    pval=2*(1-pt(abs(thit),df3)) 
    UL=(mean1-mean2)-((qt(alpha/2,df3))*(sqrt(sp2))*(sqrt((1/n1)+(1/n2))))
    LL=(mean1-mean2)+((qt(alpha/2,df3))*(sqrt(sp2))*(sqrt((1/n1)+(1/n2))))
    
    if(p<alpha) {
      kepp1 = "Tolak Ho"
    } else {
      kepp1 = "Gagal Tolak Ho"
    }
    if (thit>abs(ttab)) {
      kepp2 = "Tolak Ho"
    } else {
      kepp2 = "Gagal Tolak Ho"
    }
    
    cat("\n","Uji Mean 2 Populasi varian tidak diketahui dan sama", "\n")
    cat("H0                  : miu 1 - miu 2 = 0", "\n")
    cat("Ha                  : miu 1 - miu 2 != 0", "\n")
    cat("t Hitung            : ", thit, "\n")
    cat("|t Tabel|           : ", ttab, "\n")
    cat("P Value             : ", pval, "\n") 
    cat((1-alpha)*100,"% CI             : (",LL,";",UL,")", "\n")
    cat("Keputusan P Val     : ", kepp1, "\n")
    cat("Keputusan t hit     : ", kepp2, "\n")
  }
}
#Menjalankan function===========================================================
var2pop(x, y, 0, 0.05)
#Pembuktian=====================================================================
var.test(x, y, 1, "two.side", 0.95)
t.test(x, y, "two.sided",mu=0, paired = F,var.equal=T, 0.95)

#Uji Mean 2 Populasi Varians diketahui
meanknown = function(pop1,pop2,alpha,d0){
  mean_pop1 <- mean(pop1)
  sd_pop1 <- sd(pop1)
  mean_pop2 <- mean(pop2)
  sd_pop2 <- sd(pop2)
  # Menghitung z-statistik
  n1 <- length(pop1)
  n2 <- length(pop2)
  df <- n1 + n2 - 2
  se <- sqrt(((sd_pop1 ^ 2) / n1) + ((sd_pop2 ^ 2) / n2))
  z_stat <- (mean_pop1 - mean_pop2)-d0 / se
  # Menampilkan nilai z-statistik
  z_stat
  z_tab=qnorm(alpha/2,mean=0,sd=1,lower.tail = FALSE)
  z_tab
  # Menghitung nilai p
  p_val <- 2 * pt(abs(z_stat), df = df, lower.tail = FALSE)
  
  # Menampilkan nilai p
  p_val
  LL=(mean_pop1-mean_pop2)-(z_tab*se)
  UL=(mean_pop1-mean_pop2)+(z_tab*se)
  # Membandingkan nilai p dengan level signifikansi
  if (p_val < alpha) {
    kep1=("tolak ho")
  } else {
    kep1=("gagal tolak ho")
  }
  if (z_tab < abs(z_stat)) {
    kep2=("tolak ho")
  } else {
    kep2=("gagal tolak ho")
  }
  cat("H0 : sigma1=sigma2", "\n")  
  cat("H1 : sigma1!=sigma2", "\n")  
  cat("Statistics : ", z_stat,"\n")
  cat("CriVal     : ", z_tab,"\n")
  cat((1-alpha)*100,"% CI    : (",LL,"; ",UL,")","\n")
  cat("pval       : ", p_val,"\n") 
  cat("Decision  pvalue : ", kep1,"\n")
  cat("Decision based f   : ", kep2)
}

meanknown(pop1,pop2,alpha,0)

#UJI MEAN 2 POPULASI DATA BERPASANGAN===========================================

#DATA
paired = function(data1,data2,alpha){
  # Menghitung selisih antara dua populasi
  selisih <- pop2 - pop1
  
  # Menghitung rata-rata selisih
  rata_selisih <- mean(selisih)
  
  # Menghitung standar deviasi selisih
  sd_selisih <- sd(selisih)
  
  # Menghitung t-statistik
  t_stat <- abs(rata_selisih / (sd_selisih / sqrt(length(selisih))))
  
  # Menampilkan nilai t-statistik
  t_stat
  
  # Menghitung nilai p
  p_val <- 2 * pt(t_stat, df = length(selisih) - 1, lower.tail = FALSE)
  
  # Menampilkan nilai p
  p_val
  
  # Membandingkan nilai p dengan level signifikansi
  if (p_val < alpha) {
    kep1=("Terdapat perbedaan yang signifikan antara dua populasi.")
  } else {
    kep1=("Tidak terdapat perbedaan yang signifikan antara dua populasi.")
  }
  
  cat("Statistics : ", t_stat,"\n")
  cat("pval       : ", p_val,"\n") 
  cat("Decision  pvalue : ", kep1,"\n")
}
paired(pop1,pop2,0.05)
t.test(pop1, pop2, "two.sided",mu=0, paired = TRUE, 0.95)
