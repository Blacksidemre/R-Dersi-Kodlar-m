#normallik testleri

install.packages("nortest")
library(nortest)
install.packages("UsingR")
library(UsingR)

data(fat)

x<-fat$neck


P_VEK<-c(shapiro.test(x)$p.value,
         lillie.test(x)$p.value,
         ad.test(x)$p.value,
         sf.test(x)$p.value,
         cvm.test(x)$p.value,
         jarque.bera.test(x)$p.value,
         agostino.test(x)$p.value)

if(sum(P_VEK<0.05)>3){
  print("??o??unlukk normal de??ilildir dedi.")
}else{
  print("??o??unluk normaldir dedi.")
}

install.packages("DescTools") ###jarkuebera
library(DescTools)
install.packages("moments")  ##agostino
library(moments)



n<-nrow(fat)
P_VEK<-rep(0,7)
adD1m<-0

while(sum(P_VEK<0.05)>3){
  SEC_IND<-sample(1:n, round(n*0.90))
  xs<-x[SEC_IND]
  P_VEK<-c(shapiro.test(xs)$p.value,
           lillie.test(xs)$p.value,
           ad.test(xs)$p.value,
           sf.test(xs)$p.value,
           cvm.test(xs)$p.value,
           jarque.bera.test(xs)$p.value,
           agostino.test(xs)$p.value)
  adD1m<-adD1m+1
  print(adD1m)
}


x<-c(20,22,21,22,22,20,18,19,19,19,150)
mean(x)
median(x)

z=abs((x-mean(x))/sd(x))
z
x[which(z>3)]

install.packages("robustbase")
library(robustbase)
data("alcohol")
cz=alcohol$logSolubility
z=abs((cz-mean(cz))/sd(cz))
cz[-which(z>1.96)]

SD1nD1r<-1.96
cz1=alcohol$logSolubility
z1=abs((cz1-mean(cz1))/sd(cz1))
cz2=cz1[-which(z1>SD1nD1r)]
z2=abs((cz2-mean(cz2))/sd(cz2))
cz3=cz2[-which(z2>SD1nD1r)]
z3=abs((cz3-mean(cz3))/sd(cz3))

cz=alcohol$logSolubility
z=abs((cz-median(cz))/IQR(cz))
cz[which(z>1.96)]

SD1nD1r<-3
cz1=alcohol$logSolubility
z1=abs((cz1-mean(cz1))/sd(cz1))
z1d=abs((cz1-median(cz1))/s_Qn(cz1))
which(z1/SD1nD1r)
which(z1d>SD1nD1r)

cz1=alcohol$logSolubility
z1=abs((cz1-median(cz1))/s_Qn(cz1))
cz2=cz1[-which(z1>SD1nD1r)]
z2=abs((cz1-median(cz1))/s_Qn(cz1))

cz=alcohol$logSolubility
K<-quantile(cz)
Q1=K[2]
Q3=K[4]
KAR<-Q3-Q1
ALTS=Q1-1.5*KAR
USTS=Q3+1.5*KAR

cz[-which(ALTS>cz|USTS<cz)]

sayac=0
for (i in length(x)) {
  
  if(cz=alcohol$logSolubility
     z=abs((cz-median(cz))/s_Qn(cz))
     cz[-which(z>1.96)]
     sayac=sayac+1)
    
    else(print(sayac))
  
}


sayac=0
while (cz=alcohol$logSolubility
       z=abs((cz-median(cz))/s_Qn(cz))
       {
         if(cz[-which(z>1.96)])
         {
           sayac=sayac+1
           
         } 
         else(print(sayac))
       }
       
       
       #kayD1p veri analizi
       
       #bu paketler sD1rasD1yla indirlecek
       install.packages("homals")
       install.packages("sampling")
       install.packages("https://cran.r-project.org/src/contrib/Archive/ForImp/ForImp_1.0.3.tar.gz")
       
       install.packages("mi")
       install.packages("VIM")
       install.packages("DescTools")
       library(ForImp)
       library(DescTools)
       install.packages("DescTools")
       veri<- airquality
       PlotMiss(veri)
       
       #karanlD1k gC6rC<nC<tC<sC<
       
       veri<- airquality
       PlotMiss(veri)
       
       veri_t<-na.omit(veri) #kayD1p gC6zlemleri siler.
       
       nrow(veri)
       nrow(veri_t)
       
       nrow(veri)-nrow(veri_t)
       
       nrow(veri_t)/nrow(veri) #C'D1karD1lan kayD1p veri oranD1
       
       #karanlD1k gC6rC<ntC<sC<
       
       library(mi)
       mdf<-missing_data.frame(veri)
       image(mdf) #siyah kD1sD1mlar kayD1p veriler
       
       #mod ile kayD1p veri tahmini
       
       install.packages("ForImp")
       library(ForImp)
       library(psych)
       
       modeimp(veri) #en cok tekrar degerlerin ortlamasD1n alD1p kayD1p veriye sayar
       medianimp(veri)#bosluklarD1 medyana gC6re doldurur
       meanimp(veri) #kayD1p verileri ortalamayD1 referans alarak doldurur
       veri[,1:2=round(meanimp(veri[,1]))
            #kayD1p gC6zlemin uyumu
            
            if(any(meanimp(veri)[,1]%%1!=0)){
              print("uygun olmayan veriler var")
              
            } else{
              print("tC<m veriler uygun")
            }
            
            x=na.omit(veri[,2])
            
            z<-abs((x-mean(x))/sd(x))
            z
            
            x[which(z>3)]
            
            library(VIM)
            
            K<-5
            kss<-VIM::kNN(veri,k=K)[,1:ncol(veri)]
            kss
            
            
            
            
       
       
       
       
