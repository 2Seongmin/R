rain <- read.csv("C:/wednesdata/project/골약동_강수.csv", header = TRUE, sep = ",",
                 stringsAsFactors = TRUE, fileEncoding = "euc-kr")
temp <- read.csv("C:/wednesdata/project/골약동_기온.csv", header = TRUE, sep = ",",
                 stringsAsFactors = TRUE, fileEncoding = "euc-kr")
hum <- read.csv("C:/wednesdata/project/골약동_습도.csv", header = TRUE, sep = ",",
                 stringsAsFactors = TRUE, fileEncoding = "euc-kr")
windspeed <- read.csv("C:/wednesdata/project/골약동_풍속.csv", header = TRUE, sep = ",",
                 stringsAsFactors = TRUE, fileEncoding = "euc-kr")
winddirection <- read.csv("C:/wednesdata/project/골약동_풍향.csv", header = TRUE, sep = ",",
                 stringsAsFactors = TRUE, fileEncoding = "euc-kr")
gtemp <- read.csv("C:/wednesdata/project/골약동_지면온도.csv", header = TRUE, sep = ",",
                          stringsAsFactors = TRUE, fileEncoding = "euc-kr")
sa <- read.csv("C:/wednesdata/project/골약동_일사.csv", header = TRUE, sep = ",",
                  stringsAsFactors = TRUE, fileEncoding = "euc-kr")
jo <- read.csv("C:/wednesdata/project/골약동_일조.csv", header = TRUE, sep = ",",
                  stringsAsFactors = TRUE, fileEncoding = "euc-kr")

sun <- read.csv("C:/wednesdata/project/태양광.csv", header = TRUE, sep = ",",
                stringsAsFactors = TRUE, fileEncoding = "euc-kr")

colnames(sun)
sun.day <- sun$일일발전량.Wh.
sun.day

colnames(rain)
rain.day <- rain$강수[1:395]

colnames(temp)
tem.day <- temp$기온[1:395]

colnames(hum)
hum.day <- hum$습도평균[1:395]

colnames(windspeed)
spd.day <- windspeed$풍속[1:395]

colnames(winddirection)
dir.day <- winddirection$풍향평균[1:395]

colnames(gtemp)
gtem.day <- gtemp$지면온도[1:395]

colnames(sa)
sa.day <- sa$일사[1:395]

colnames(jo)
jo.day <- jo$일조[1:395]


#************#
data <- data.frame(sun.day, rain.day, tem.day, hum.day, spd.day, dir.day, gtem.day, sa.day, jo.day)

summary(data)


# 독립변수들 간의 상관관계
cols <- c("rain.day", "tem.day", "hum.day", "spd.day", "dir.day", "gtem.day", "sa.day", "jo.day")
plot(data[, cols])

# library(psych)
# col_test_result <- corr.test(data[ ,cols])
# col_test_result$t

#독립변수와 종속변수 간의 상관관계 & 상관계수
plot(sun.day, tem.day,
     pch =21, col ="black", bg ="red", xlab = "일일발전량", ylab = "기온")
cor.test(sun.day, tem.day)

plot(sun.day, rain.day,
     pch =21, col ="black", bg ="orange", xlab = "일일발전량", ylab = "강수")
cor.test(sun.day, rain.day)

plot(sun.day, spd.day,
     pch =21, col ="black", bg ="yellow", xlab = "일일발전량", ylab = "풍속")
cor.test(sun.day, spd.day)

plot(sun.day, dir.day,
     pch =21, col ="black", bg ="green", xlab = "일일발전량", ylab = "풍향")
cor.test(sun.day, dir.day)

plot(sun.day, hum.day,
     pch =21, col ="black", bg ="blue", xlab = "일일발전량", ylab = "습도")
cor.test(sun.day, hum.day)

plot(sun.day, gtem.day,
     pch =21, col ="black", bg ="purple", xlab = "일일발전량", ylab = "지면온도")
cor.test(sun.day, gtem.day)

plot(sun.day, sa.day,
     pch =21, col ="black", bg ="gray", xlab = "일일발전량", ylab = "일사")
cor.test(sun.day, sa.day)

plot(sun.day, jo.day,
     pch =21, col ="black", bg ="black", xlab = "일일발전량", ylab = "일조")
cor.test(sun.day, jo.day)


# week4
attach(data)
library(car)

# R-squared 값이 0.6332로 매우 낮음
lm(sun.day ~ tem.day + rain.day + spd.day + dir.day + hum.day)
model1 <- lm(sun.day ~ tem.day + rain.day + spd.day + dir.day + hum.day)
summary(model1)
vif(model1)

# ws(풍속) 회귀계수가 유의하지 않음 -> 제거
lm(sun.day ~ tem.day + rain.day + dir.day + hum.day)
model2 <- lm(sun.day ~ tem.day + rain.day + dir.day + hum.day)
summary(model2)
# 결정계수와 ,p-value 값이 유사하기 때문에 그냥 사용

lm(sun.day ~ tem.day + rain.day + dir.day + hum.day + gtem.day)
model3 <- lm(sun.day ~ tem.day + rain.day + dir.day + hum.day + gtem.day)
summary(model3)
vif(model3)

# model2에 일조 변수 포함한 모델.final
lm(sun.day ~ tem.day + rain.day + dir.day + hum.day + gtem.day + jo.day)
model4 <- lm(sun.day ~ tem.day + rain.day + dir.day + hum.day + gtem.day + jo.day)
summary(model4)
vif(model4)

# 최종모델
lm(sun.day ~ tem.day + rain.day + hum.day + gtem.day + jo.day)
model.final <- lm(sun.day ~ tem.day + rain.day + hum.day + gtem.day + jo.day)
summary(model.final)
vif(model.final)

# 결정계수와 ,p-value 값이 유사하기 때문에 그냥 사용
library(car)
vif(model1)

par(mfrow=c(2,2))
plot(model1)

# 두 모델의 적합성이 거의 같다고 해석 가능
extractAIC(model1)
extractAIC(model2)
extractAIC(model3)
extractAIC(model4)
extractAIC(model.final)

extractAIC(model1, k=log(395))
extractAIC(model2, k=log(395))
extractAIC(model3, k=log(395))
extractAIC(model4, k=log(395))
extractAIC(model.final, k=log(395))

model.final <- lm(sun.day ~ tem.day + rain.day + hum.day + gtem.day + jo.day)
summary(model.final)
coefs <- model.final$coefficients

# 원래 데이터 값
custom_num <- coefs[1] + coefs[2]*1 + coefs[3]*1  + coefs[4]*1  + coefs[5]*1 + coefs[6]*1
custom_num

# 영향력 확인
custom_num_1 <- coefs[1] + coefs[2]*2 + coefs[3]*1  + coefs[4]*1  + coefs[5]*1 + coefs[6]*1
custom_num_1

custom_num_2 <- coefs[1] + coefs[2]*1 + coefs[3]*2  + coefs[4]*1  + coefs[5]*1 + coefs[6]*1
custom_num_2

custom_num_3 <- coefs[1] + coefs[2]*1 + coefs[3]*1  + coefs[4]*2  + coefs[5]*1 + coefs[6]*1
custom_num_3

custom_num_4 <- coefs[1] + coefs[2]*1 + coefs[3]*1  + coefs[4]*1  + coefs[5]*2 + coefs[6]*1
custom_num_4

custom_num_5 <- coefs[1] + coefs[2]*1 + coefs[3]*1  + coefs[4]*1  + coefs[5]*1 + coefs[6]*2
custom_num_5

install.packages("ggplot2")
library(ggplot2)

ggplot(data=sun, aes(x=sun.day, y=tem.day)) + geom_point(Shape = 19, size=1.5, colour ="red")+
  ggtitle("기온에 따른 일일발전량")
