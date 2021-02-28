#SSA
library(MLmetrics)
library(ggplot2)
library(svd)
library(forcats)
library(Rssa)
data = read.csv("C:/Users/IDEAPAD 120S/Documents/Kuliah/Semester 7/Tugas Akhir/Data Kurs/KURS TRAINING 60%.csv", header = TRUE, sep = ",")
data.test = read.csv("C:/Users/IDEAPAD 120S/Documents/Kuliah/Semester 7/Tugas Akhir/Data Kurs/KURS TESTING 40%.csv", header = TRUE, sep = ",")
#Dekomposisi
y = ts(data[,1])
plot(y)
s = ssa(y, L=137)
plot(s)
summary(s)
plot(s, type= "series", groups = as.list(1:3))
plot(s, type = "vectors",groups = as.list(1:3))
plot(s, type= "paired", groups = as.list(1:3))
#Rekonstruksi
plot(wcor(s, groups = as.list(1:20)))
rekon = reconstruct(s, groups = list(Trend = c(1,2,3)))
plot(wcor(s, groups = list(Trend = c(1,2,3))))
t = rekon$Trend
plot(rekon)
noise <- residuals(rekon)
plot(noise)
MAPE(t,data$Kurs)*100
RMSE(t,data$Kurs)
#Peramalan
Prediksi.t = rforecast(s, groups = list(Trend = c(1,2,3)), len = 365)
Prediksi.s1 = rforecast(s, groups = list(Seasonality1 = c(8:9)), len = 182)
Prediksi.s2 = rforecast(s, groups = list(Seasonality2 = c(10:11)), len = 182)
Prediksi.s3 = rforecast(s, groups = list(Seasonality3 = c(12:13)), len = 182)
Peramalan = (Prediksi.t)
MAPE(Peramalan,data.test$Kurs)*100
RMSE(Peramalan,data.test$Kurs)
