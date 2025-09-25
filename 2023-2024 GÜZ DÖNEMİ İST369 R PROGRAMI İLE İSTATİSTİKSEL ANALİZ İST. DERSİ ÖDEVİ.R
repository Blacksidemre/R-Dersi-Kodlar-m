#2023-2024 GÜZ DÖNEMİ İST369 R PROGRAMI İLE İSTATİSTİKSEL ANALİZ İST. DERSİ ÖDEVİ 

#Hazırlayan; 

#Yunus Emre BÜYÜKGÜLER - 21020513

#25 Kasım 2023

#Umut YAMAK 
#İST369 R PROGRAMI İLE İSTATİSTİKSEL ANALİZ 

#------------------------------------------------------------------------------

#kesikli dağılım örneği


#Problem: Online Alışveriş Sitesinde Ürün Satın Alma Olasılığı 


kullanici_sayisi<-100 

urun_satin_alma <- rbinom(kullanici_sayisi, size = 1, prob = 0.2) 



test_sonucu <- prop.test(sum(urun_satin_alma), length(urun_satin_alma), p = 0.2, alternative = "two.sided") 



#p-value = 0.4533 çıktı. 

#p > α olduğundan H0 reddedilemez. 

#Kullanıcıların belirli bir ürünü satın alma olasılığı %20'dir denilebilir. 

#------------------------------------------------------------------------------
#sürekli dağılım örneği

#Problem: Online Oyunlardaki Denge 

prop.test(x=900,n=1000,p=0.5,alternative = "two.sided", correct = TRUE) 

#P < α olduğundan H0 reddedilir. 

#Oyunda bir dengesizlik olduğundan 95% güven düzeyinde söz edilebilir. 

#------------------------------------------------------------------------------

#Problem: Ortalama Teslimat Süresi 

populasyon_ortalama <- 3  # Popülasyonun teorik ortalama teslimat süresi (gün) 

populasyon_varyans <- 1  # Popülasyonun teorik varyansı 

orneklem_buyuklugu <- 40  # Örneklem büyüklüğü 



# Normal dağılıma dayalı rastgele örnekleme 

ornek_veri <- rnorm(orneklem_buyuklugu, mean = populasyon_ortalama, sd = sqrt(populasyon_varyans)) 



# Oluşturulan örneklem veri setini kontrol et 

print(ornek_veri) 



# Z-testi için parametreleri tanımla 

h0_ortalama <- 3  # Hipotez edilen popülasyon ortalaması 

anlam_düzeyi <- 0.05  # Anlamlılık düzeyi 



# Z-testi gerçekleştir 

z_testi_sonucu <- z.test(ornek_veri, mu = h0_ortalama, sigma = 1, conf.level = 0.95, alternative = "two.sided") 



# Z-testi sonuçlarını göster 

print(z_testi_sonucu) 

#------------------------------------------------------------------------------

#Problem: Sınav Performansı 

yonetim1_notlari <- rnorm(30, mean = 75, sd = 10)  # Yöntem 1 

yonetim2_notlari <- rnorm(30, mean = 80, sd = 12)  # Yöntem 2 

# Oluşturulan örneklem veri setlerini kontrol et 

print(yonetim1_notlari) 

print(yonetim2_notlari) 

# Bağımsız iki örneklem t-testi için parametreleri tanımla 

anlam_düzeyi <- 0.05  # Anlamlılık düzeyi 

# Hipotezler: 

# H0: İki öğretim yöntemi arasında bir fark yoktur. (Ortalama fark = 0) 

# H1: İki öğretim yöntemi arasında bir fark vardır. (Ortalama fark ≠ 0) 

# Bağımsız iki örneklem t-testi gerçekleştir 

t_testi_sonucu <- t.test(yonetim1_notlari, yonetim2_notlari) 
  
#p > α olduğundan H0 reddedilir. 

#İki yönetim arasında bir fark olduğu 95% güvenle söylenebilir. 

#------------------------------------------------------------------------------

#Wilcoxon Testi Örneği

#Problem: Politikaları Optimize Etmek 
  
# Eski ve yeni işe başlama süreleri veri seti oluştur 

eski_baslama_sureleri <- c(8:45, 8:55, 9:05, 8:50, 9:10) 

yeni_politika_baslama_sureleri <- c(9:15, 9:25, 9:35, 9:20, 9:30) 

# İki grup arasındaki veri noktalarını eşitle 

yeni_politika_baslama_sureleri <-yeni_politika_baslama_sureleri[1:length(eski_baslama_sureleri)] 

# Oluşturulan veri setlerini kontrol et 

print(eski_baslama_sureleri) 

print(yeni_politika_baslama_sureleri) 

# Wilcoxon işaretle testi için parametreleri tanımla 

alpha <- 0.05  # Anlamlılık düzeyi 

# Hipotezler: 

# H0: Eski ve yeni işe başlama süreleri arasında bir fark yoktur. 

# H1: Eski ve yeni işe başlama süreleri arasında bir fark vardır. 

# Wilcoxon işaretle testi gerçekleştir 

wilcoxon_test_sonucu <- wilcox.test(eski_baslama_sureleri, yeni_politika_baslama_sureleri, paired = TRUE) 

print(wilcoxon_test_sonucu)

#------------------------------------------------------------------------------

#Sürekli Olasılık Dağılımlarıyla Alakalı Ek Bir Soru  

#Problem: Müşterilere Ayırılan Hizmet Süresi 

a <- 0    # Minimum süre 

b <- 30   # Maksimum süre 



# 15 dakikadan fazla hizmet süresi olma olasılığını hesapla 

olasılık <- 1 - punif(15, min = a, max = b) 

print(olasılık)

#Restoranın bir müşteriye hizmet süresi 15 dakikadan fazla olma olasılığı 50%’dir 

