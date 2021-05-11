#Kupiec test
kupiec_test=function(Location,alpha){
  n1=sum(ifelse(Location<=alpha,1,0));n0=length(Location)-n1
  p1=n1/(n0+n1);p0=1-p1
  Kupiec=2*log(((p0^n0)*(p1^n1))/((((1-alpha)^n0))*(alpha^n1)))
  return(1-pchisq(Kupiec,1))
}

#Matrix with Kupiec results by observations and number of excesses
Days=c(251,252,253,1259);Excess=c(0:30); Alpha=0.995; PValues_Kupiec=matrix(0,length(Excess)+1,length(Days)+1)
PValues_Kupiec[,1]=c(0,Excess);PValues_Kupiec[1,]=c(0,Days)
for(i in 1:length(Days)){
  for (j in 1:length(Excess)){
    Excess_Num=PValues_Kupiec[j+1,1]
    if(Excess_Num==0){Array_Excess=rep(0,PValues_Kupiec[1,i+1])} else {Array_Excess=c(rep(1,Excess_Num),rep(0,PValues_Kupiec[1,i+1]-Excess_Num))}
    PValues_Kupiec[j+1,i+1]=kupiec_test(Array_Excess,Alpha)
  }
}

#Christoffersen test
Christoffersen_Test=function(CarteraTest,VaR,alpha){
  n00=0;n11=0;n10=0;n01=0;alpha=1-alpha
  Exceed=ifelse(CarteraTest<VaR,1,0)
  for (i in 1:(length(CarteraTest)-1)){
    if (Exceed[i]==0 & Exceed[i+1]==0) {n00=n00+1}
    if (Exceed[i]==1 & Exceed[i+1]==1) {n11=n11+1}
    if (Exceed[i]==1 & Exceed[i+1]==0) {n10=n10+1}
    if (Exceed[i]==0 & Exceed[i+1]==1) {n01=n01+1}
  }
  n1=sum(CarteraTest[1:length(CarteraTest)-1]<VaR[1:length(CarteraTest)-1])
  n0=length(CarteraTest)-n1-1
  p00=n00/n0;p11=n11/n1;p10=n10/n1;p01=n01/n0
  p00_en00=p00^n00;p11_en11=p11^n11;p10_en10=p10^n10;p01_en01=p01^n01
  mult=p00_en00*p11_en11*p10_en10*p01_en01
  Christoffersen=2*log(mult/(((1-alpha)^n0)*(alpha^n1)))
  if (is.nan(Christoffersen)){return(999)} else if (Christoffersen==Inf) {return(999)} else {return(Christoffersen)}
}

#Results databases are loaded and observed returns and VaR are kept in different variables
DB1=read.csv("C:/Users/Usuario/Dropbox/Doctorado/z. Volatilidad II/2. Results/2. Test/Results/1_ANN_ARCH.csv",sep=";")
DB2=read.csv("C:/Users/Usuario/Dropbox/Doctorado/z. Volatilidad II/2. Results/2. Test/Results/2_LSTM_GARCH.csv",sep=";")
DB3=read.csv("C:/Users/Usuario/Dropbox/Doctorado/z. Volatilidad II/2. Results/2. Test/Results/3_MT_GARCH.csv",sep=";")
DB4=read.csv("C:/Users/Usuario/Dropbox/Doctorado/z. Volatilidad II/2. Results/2. Test/Results/4_T_GARCH.csv",sep=";")
DB5=read.csv("C:/Users/Usuario/Dropbox/Doctorado/z. Volatilidad II/2. Results/2. Test/Results/5_MTL_GARCH.csv",sep=";")
DB6=read.csv("C:/Users/Usuario/Dropbox/Doctorado/z. Volatilidad II/2. Results/2. Test/Results/6_TL_GARCH.csv",sep=";")
Test=DB1[,9]; VaR_GARCH=DB1[,"VaR_GARCH"]; VaR_AVGARCH=DB1[,"VaR_AVGARCH"]; VaR_EGARCH=DB1[,"VaR_EGARCH"]; VaR_GJR_GARCH=DB1[,"VaR_GJR_GARCH"]; VaR_TrARCH=DB1[,"VaR_TARCH"]; 
VaR_FIGARCH=DB1[,"VaR_FIGARCH"]; VaR_ANN_GARCH=DB1[,"VaR_ANN_ARCH"]; VaR_LSTM_ARCH=DB2[,"VaR_LSTM_ARCH"]; VaR_MT_GARCH=DB3[,"VaR_T_ANN_ARCH"]; VaR_T_GARCH=DB4[,"VaR_T_ANN_ARCH"]; 
VaR_MTL_GARCH=DB5[,"VaR_T_ANN_ARCH"]; VaR_TL_GARCH=DB6[,"VaR_T_ANN_ARCH"]; alpha=0.995

#Total Christoffersen Test
Chr_GARCH=1-pchisq(Christoffersen_Test(Test, -VaR_GARCH, alpha),1)
Chr_AVGARCH=1-pchisq(Christoffersen_Test(Test, -VaR_AVGARCH, alpha),1)
Chr_EGARCH=1-pchisq(Christoffersen_Test(Test, -VaR_EGARCH, alpha),1)
Chr_GJR_GARCH=1-pchisq(Christoffersen_Test(Test, -VaR_GJR_GARCH, alpha),1)
Chr_TrARCH=1-pchisq(Christoffersen_Test(Test, -VaR_TrARCH, alpha),1)
Chr_FIGARCH=1-pchisq(Christoffersen_Test(Test, -VaR_FIGARCH, alpha),1)
Chr_ANN_GARCH=1-pchisq(Christoffersen_Test(Test, -VaR_ANN_GARCH, alpha),1)
Chr_LSTM_ARCH=1-pchisq(Christoffersen_Test(Test, -VaR_LSTM_ARCH, alpha),1)
Chr_MT_GARCH=1-pchisq(Christoffersen_Test(Test, -VaR_MT_GARCH, alpha),1)
Chr_T_GARCH=1-pchisq(Christoffersen_Test(Test, -VaR_T_GARCH, alpha),1)
Chr_MTL_GARCH=1-pchisq(Christoffersen_Test(Test, -VaR_MTL_GARCH, alpha),1)
Chr_TL_GARCH=1-pchisq(Christoffersen_Test(Test, -VaR_TL_GARCH, alpha),1)
cbind.data.frame(Model=c("Chr_GARCH","Chr_AVGARCH","Chr_EGARCH", "Chr_GJR_GARCH", "Chr_TrARCH", "Chr_FIGARCH", "Chr_ANN_GARCH", "Chr_LSTM_ARCH", "Chr_MT_GARCH", "Chr_T_GARCH", "Chr_MTL_GARCH", "Chr_TL_GARCH"),
                 Pvalue=c(Chr_GARCH,Chr_AVGARCH,Chr_EGARCH, Chr_GJR_GARCH, Chr_TrARCH, Chr_FIGARCH, Chr_ANN_GARCH, Chr_LSTM_ARCH, Chr_MT_GARCH, Chr_T_GARCH, Chr_MTL_GARCH, Chr_TL_GARCH))

#Christoffersen Test 2016
Chr_GARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_GARCH[1:252], alpha),1)
Chr_AVGARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_AVGARCH[1:252], alpha),1)
Chr_EGARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_EGARCH[1:252], alpha),1)
Chr_GJR_GARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_GJR_GARCH[1:252], alpha),1)
Chr_TrARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_TrARCH[1:252], alpha),1)
Chr_FIGARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_FIGARCH[1:252], alpha),1)
Chr_ANN_GARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_ANN_GARCH[1:252], alpha),1)
Chr_LSTM_ARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_LSTM_ARCH[1:252], alpha),1)
Chr_MT_GARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_MT_GARCH[1:252], alpha),1)
Chr_T_GARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_T_GARCH[1:252], alpha),1)
Chr_MTL_GARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_MTL_GARCH[1:252], alpha),1)
Chr_TL_GARCH=1-pchisq(Christoffersen_Test(Test[1:252], -VaR_TL_GARCH[1:252], alpha),1)
cbind.data.frame(Model=c("Chr_GARCH","Chr_AVGARCH","Chr_EGARCH", "Chr_GJR_GARCH", "Chr_TrARCH", "Chr_FIGARCH", "Chr_ANN_GARCH", "Chr_LSTM_ARCH", "Chr_MT_GARCH", "Chr_T_GARCH", "Chr_MTL_GARCH", "Chr_TL_GARCH"),
                 Pvalue=c(Chr_GARCH,Chr_AVGARCH,Chr_EGARCH, Chr_GJR_GARCH, Chr_TrARCH, Chr_FIGARCH, Chr_ANN_GARCH, Chr_LSTM_ARCH, Chr_MT_GARCH, Chr_T_GARCH, Chr_MTL_GARCH, Chr_TL_GARCH))

#Christoffersen Test 2017
Chr_GARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_GARCH[253:503], alpha),1)
Chr_AVGARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_AVGARCH[253:503], alpha),1)
Chr_EGARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_EGARCH[253:503], alpha),1)
Chr_GJR_GARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_GJR_GARCH[253:503], alpha),1)
Chr_TrARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_TrARCH[253:503], alpha),1)
Chr_FIGARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_FIGARCH[253:503], alpha),1)
Chr_ANN_GARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_ANN_GARCH[253:503], alpha),1)
Chr_LSTM_ARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_LSTM_ARCH[253:503], alpha),1)
Chr_MT_GARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_MT_GARCH[253:503], alpha),1)
Chr_T_GARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_T_GARCH[253:503], alpha),1)
Chr_MTL_GARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_MTL_GARCH[253:503], alpha),1)
Chr_TL_GARCH=1-pchisq(Christoffersen_Test(Test[253:503], -VaR_TL_GARCH[253:503], alpha),1)
cbind.data.frame(Model=c("Chr_GARCH","Chr_AVGARCH","Chr_EGARCH", "Chr_GJR_GARCH", "Chr_TrARCH", "Chr_FIGARCH", "Chr_ANN_GARCH", "Chr_LSTM_ARCH", "Chr_MT_GARCH", "Chr_T_GARCH", "Chr_MTL_GARCH", "Chr_TL_GARCH"),
                 Pvalue=c(Chr_GARCH,Chr_AVGARCH,Chr_EGARCH, Chr_GJR_GARCH, Chr_TrARCH, Chr_FIGARCH, Chr_ANN_GARCH, Chr_LSTM_ARCH, Chr_MT_GARCH, Chr_T_GARCH, Chr_MTL_GARCH, Chr_TL_GARCH))

#Christoffersen Test 2018
Chr_GARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_GARCH[504:754], alpha),1)
Chr_AVGARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_AVGARCH[504:754], alpha),1)
Chr_EGARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_EGARCH[504:754], alpha),1)
Chr_GJR_GARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_GJR_GARCH[504:754], alpha),1)
Chr_TrARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_TrARCH[504:754], alpha),1)
Chr_FIGARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_FIGARCH[504:754], alpha),1)
Chr_ANN_GARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_ANN_GARCH[504:754], alpha),1)
Chr_LSTM_ARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_LSTM_ARCH[504:754], alpha),1)
Chr_MT_GARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_MT_GARCH[504:754], alpha),1)
Chr_T_GARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_T_GARCH[504:754], alpha),1)
Chr_MTL_GARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_MTL_GARCH[504:754], alpha),1)
Chr_TL_GARCH=1-pchisq(Christoffersen_Test(Test[504:754], -VaR_TL_GARCH[504:754], alpha),1)
cbind.data.frame(Model=c("Chr_GARCH","Chr_AVGARCH","Chr_EGARCH", "Chr_GJR_GARCH", "Chr_TrARCH", "Chr_FIGARCH", "Chr_ANN_GARCH", "Chr_LSTM_ARCH", "Chr_MT_GARCH", "Chr_T_GARCH", "Chr_MTL_GARCH", "Chr_TL_GARCH"),
                 Pvalue=c(Chr_GARCH,Chr_AVGARCH,Chr_EGARCH, Chr_GJR_GARCH, Chr_TrARCH, Chr_FIGARCH, Chr_ANN_GARCH, Chr_LSTM_ARCH, Chr_MT_GARCH, Chr_T_GARCH, Chr_MTL_GARCH, Chr_TL_GARCH))

#Christoffersen Test 2019
Chr_GARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_GARCH[755:1006], alpha),1)
Chr_AVGARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_AVGARCH[755:1006], alpha),1)
Chr_EGARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_EGARCH[755:1006], alpha),1)
Chr_GJR_GARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_GJR_GARCH[755:1006], alpha),1)
Chr_TrARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_TrARCH[755:1006], alpha),1)
Chr_FIGARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_FIGARCH[755:1006], alpha),1)
Chr_ANN_GARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_ANN_GARCH[755:1006], alpha),1)
Chr_LSTM_ARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_LSTM_ARCH[755:1006], alpha),1)
Chr_MT_GARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_MT_GARCH[755:1006], alpha),1)
Chr_T_GARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_T_GARCH[755:1006], alpha),1)
Chr_MTL_GARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_MTL_GARCH[755:1006], alpha),1)
Chr_TL_GARCH=1-pchisq(Christoffersen_Test(Test[755:1006], -VaR_TL_GARCH[755:1006], alpha),1)
cbind.data.frame(Model=c("Chr_GARCH","Chr_AVGARCH","Chr_EGARCH", "Chr_GJR_GARCH", "Chr_TrARCH", "Chr_FIGARCH", "Chr_ANN_GARCH", "Chr_LSTM_ARCH", "Chr_MT_GARCH", "Chr_T_GARCH", "Chr_MTL_GARCH", "Chr_TL_GARCH"),
                 Pvalue=c(Chr_GARCH,Chr_AVGARCH,Chr_EGARCH, Chr_GJR_GARCH, Chr_TrARCH, Chr_FIGARCH, Chr_ANN_GARCH, Chr_LSTM_ARCH, Chr_MT_GARCH, Chr_T_GARCH, Chr_MTL_GARCH, Chr_TL_GARCH))

#Christoffersen Test 2020
Chr_GARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_GARCH[1007:length(Test)], alpha),1)
Chr_AVGARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_AVGARCH[1007:length(Test)], alpha),1)
Chr_EGARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_EGARCH[1007:length(Test)], alpha),1)
Chr_GJR_GARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_GJR_GARCH[1007:length(Test)], alpha),1)
Chr_TrARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_TrARCH[1007:length(Test)], alpha),1)
Chr_FIGARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_FIGARCH[1007:length(Test)], alpha),1)
Chr_ANN_GARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_ANN_GARCH[1007:length(Test)], alpha),1)
Chr_LSTM_ARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_LSTM_ARCH[1007:length(Test)], alpha),1)
Chr_MT_GARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_MT_GARCH[1007:length(Test)], alpha),1)
Chr_T_GARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_T_GARCH[1007:length(Test)], alpha),1)
Chr_MTL_GARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_MTL_GARCH[1007:length(Test)], alpha),1)
Chr_TL_GARCH=1-pchisq(Christoffersen_Test(Test[1007:length(Test)], -VaR_TL_GARCH[1007:length(Test)], alpha),1)
cbind.data.frame(Model=c("Chr_GARCH","Chr_AVGARCH","Chr_EGARCH", "Chr_GJR_GARCH", "Chr_TrARCH", "Chr_FIGARCH", "Chr_ANN_GARCH", "Chr_LSTM_ARCH", "Chr_MT_GARCH", "Chr_T_GARCH", "Chr_MTL_GARCH", "Chr_TL_GARCH"),
                 Pvalue=c(Chr_GARCH,Chr_AVGARCH,Chr_EGARCH, Chr_GJR_GARCH, Chr_TrARCH, Chr_FIGARCH, Chr_ANN_GARCH, Chr_LSTM_ARCH, Chr_MT_GARCH, Chr_T_GARCH, Chr_MTL_GARCH, Chr_TL_GARCH))
