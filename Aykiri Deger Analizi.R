# Gerekli paketleri y??kleyelim
install.packages("mvnTest") # E??er hen??z y??kl?? de??ilse
library(mvnTest)

# ??rnek veri seti olu??tural??m
set.seed(123) # Tekrarlanabilirlik i??in seed'i ayarlayal??m
data <- matrix(rnorm(300, mean = 0, sd = 1), ncol = 3) # 100 g??zlem, 3 de??i??ken
outliers <- cbind(c(3, -3, 4), c(-2, 1, 5), c(1, -4, -3)) # Ayk??r?? de??erler
data_with_outliers <- rbind(data, outliers) # Ayk??r?? de??erleri veri setine ekleyelim

# Ayk??r?? de??erleri tespit etmek i??in kovaryans matrisi ve mahalanobis uzakl?????? kullanal??m
cov_matrix <- cov(data_with_outliers) # Kovaryans matrisi
mahalanobis_dist <- mahalanobis(data_with_outliers, colMeans(data_with_outliers), cov_matrix) # Mahalanobis uzakl??????

# Potansiyel ayk??r?? de??erleri belirleyelim
outlier_threshold <- qchisq(0.95, df = length(data)) # E??ik de??eri belirleyelim
potential_outliers <- which(mahalanobis_dist > outlier_threshold) # E??ik de??erinden b??y??k olan uzakl??klar?? bulal??m

# Ayk??r?? de??erleri veri setinden ????karal??m
cleaned_data <- data_with_outliers[-potential_outliers, ]

# Temizlenmi?? verilerin ??ok de??i??kenli normal da????l??ma uygunlu??unu de??erlendirelim
mvn(data = cleaned_data, mvnTest(alpha = 0.05))
------------------------------------------------------------------------------
  # Gerekli k??t??phaneleri y??kleyelim
  library(dplyr)

# ??rnek veri setini olu??tural??m
set.seed(123) # Tekrarlanabilirlik i??in seed'i ayarlayal??m
data <- rnorm(100, mean = 50, sd = 10) # 100 g??zlem

# Aritmetik ortalama ve medyan de??erlerini hesaplayal??m
mean_val <- mean(data)
median_val <- median(data)

# Z skorlar??n?? hesaplayal??m
z_scores_mean <- abs((data - mean_val) / sd(data))
z_scores_median <- abs((data - median_val) / sd(data))

# Ayk??r?? g??zlemleri tespit edelim
outliers_mean <- which(z_scores_mean > 2)
outliers_median <- which(z_scores_median > 2)

# Ayk??r?? g??zlemleri veri setinden ????karal??m
cleaned_data <- data[-c(outliers_mean, outliers_median)]

# Ayk??r?? g??zlemleri kay??p veri olarak ele alal??m
lost_values <- data[c(outliers_mean, outliers_median)]

# Kay??p de??er atama tekni??iyle ayk??r?? g??zlemleri doldural??m (??rne??in, ortalama ile doldural??m)
filled_data <- replace(data, c(outliers_mean, outliers_median), mean_val)

# Tan??mlay??c?? istatistikleri hesaplayal??m ve de??erlendirelim
summary(cleaned_data)
summary(filled_data)  
  

