sun <- read.csv("C:/wednesdata/project/태양광.csv", header = TRUE, sep = ",",
                stringsAsFactors = TRUE, fileEncoding = "euc-kr")
sun

colnames(sun)
wh <- sun$일일발전량.Wh.
tem <- sun$기온
rain <- sun$강수
ws <- sun$풍속
dw <- sun$풍향
hum <- sun$습도

summary(sun)

# 독립변수들 간의 상관관계
cols <- c("기온", "강수", "풍속", "풍향", "습도")
plot(sun[, cols])

## library(psych)
## col_test_result <- corr.test(sun[ , cols])
## col_test_result$t

#독립변수와 종속변수 간의 상관관계 & 상관계수
plot(wh, tem,
     pch =21, col ="blue", bg ="orange", xlab = "일일발전량", ylab = "기온")
cor.test(wh, tem)

plot(wh, rain,
     pch =21, col ="blue", bg ="orange", xlab = "일일발전량", ylab = "강수")
cor.test(wh, rain)

plot(wh, ws,
     pch =21, col ="blue", bg ="orange", xlab = "일일발전량", ylab = "풍속")
cor.test(wh, ws)

plot(wh, dw,
     pch =21, col ="blue", bg ="orange", xlab = "일일발전량", ylab = "풍향")
cor.test(wh, dw)

plot(wh, hum,
     pch =21, col ="blue", bg ="orange", xlab = "일일발전량", ylab = "습도")
cor.test(wh, hum)

summary(sun)

# week4
attach(sun)

lm(wh ~ dw + hum + rain + tem + ws)
# ws 회귀계수가 유의하지 않음 -> 제거
lm(wh ~ dw + hum + rain + tem)

model <- lm(wh ~ dw + hum + rain + tem + ws)
summary(model)
vif(model)

par(mfrow=c(2,2))
plot(model)
