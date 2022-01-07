library(corrplot)
library(Metrics)
library(randomForest)
library(caret)
library(gbm)

## clean code data analysis
setwd("C:/Users/HOME/Desktop/대구집값")
house <- read.csv("Daegu_Real_Estate_data.csv")

# 결측치 확인
sum(!complete.cases(house))  # 결측치 0개

# target 변수 정규성 만족 여부 확인
qqnorm(house$SalePrice)  # 정규성 만족

# 면적 평방피트를 평으로 변환한 새로운 변수 Size.p. 생성
house$Size.p. <- round(house$Size.sqf. / 35.583)

# TimeTo 변수 level 순서 수정 및 no_bus_stop level 수정 0-5min -> 0~5min으로 수정
levels(house$TimeToSubway) <- levels(house$TimeToSubway)[c(1, 4, 2, 3, 5)]
levels(house$TimeToSubway)[5] <- "no_subway_station_nearby"
levels(house$TimeToSubway)[1] <- "0~5min"
levels(house$TimeToBusStop) <- levels(house$TimeToBusStop)[c(1, 3, 2)]

house$SubwayStation[house$group == 25] <- "Sin-nam"
house$SubwayStation[house$group == 11] <- "Daegu"

house <- house[house$SubwayStation != "no_subway_nearby", ]

# 평당 가격 변수 Price.per.Size 생성
house$Price.per.Size <- house$SalePrice / house$Size.p.

# 건축일자와 거래일자 Date로 바꿔줌
# 건축일자 : YearBuilt - 01 - 01
# 거래일자 : YrSold - MonthSold - 01
house$DateSold <- as.Date(with(house, 
                               paste(YrSold, ifelse(MonthSold < 10, paste("0", MonthSold, sep = ""), MonthSold), "01", sep = "-")))

house$DateBuilt <- as.Date(paste(house$YearBuilt, "-01-01", sep = ""))

# 평당가격과 평수 비교
boxplot(house$Price.per.Size ~ house$Size.p.)
with(subset(house, YearBuilt >= 2000), boxplot(Price.per.Size ~ Size.p.))

# 평당가격과 층 비교
boxplot(house$Price.per.Size ~ house$Floor)

# 평당 가격과 매니저 수 비교
boxplot(house$Price.per.Size ~ house$N_manager)

# 주차장과 평당 가격 비교
corrplot(cor(house[, c("Price.per.Size", "N_Parkinglot.Ground.", "N_Parkinglot.Basement.")]), method = "ellipse")

# 아파트 동수와 평당 가격비교
boxplot(house$Price.per.Size ~ house$N_APT)

# 아파트 총 엘레베이터 수와 평당 가격 비교
boxplot(house$Price.per.Size ~ house$N_elevators)

# 동 당 엘레베이터 수와 평당 가격 비교
boxplot(house$Price.per.Size ~ c((house$N_elevators + 0.2) / house$N_APT))
house$Prob_elevators <- (house$N_elevators + 0.2) / house$N_APT

# 지하철 역 별 거래 시기에 따른 평당 가격 증가 추세
# windows()
par(mfrow = c(2, 4))
with(subset(house, SubwayStation == "Bangoge"), boxplot(Price.per.Size ~ YrSold, ylim = range(house$Price.per.Size), main = unique(SubwayStation)))
with(subset(house, SubwayStation == "Daegu"), boxplot(Price.per.Size ~ YrSold, ylim = range(house$Price.per.Size), main = unique(SubwayStation)))
with(subset(house, SubwayStation == "Sin-nam"), boxplot(Price.per.Size ~ YrSold, ylim = range(house$Price.per.Size), main = unique(SubwayStation)))
with(subset(house, SubwayStation == "Myung-duk"), boxplot(Price.per.Size ~ YrSold, ylim = range(house$Price.per.Size), main = unique(SubwayStation)))
with(subset(house, SubwayStation == "Chil-sung-market"), boxplot(Price.per.Size ~ YrSold, ylim = range(house$Price.per.Size), main = unique(SubwayStation)))
with(subset(house, SubwayStation == "Kyungbuk_uni_hospital"), boxplot(Price.per.Size ~ YrSold, ylim = range(house$Price.per.Size), main = unique(SubwayStation)))
with(subset(house, SubwayStation == "Banwoldang"), boxplot(Price.per.Size ~ YrSold, ylim = range(house$Price.per.Size), main = unique(SubwayStation)))
par(mfrow = c(1, 1))

# HeatingType
barplot(table(house$HeatingType))
boxplot(house$Price.per.Size ~ house$HeatingType)
house$YearBuilt[house$HeatingType == "central_heating"]

# HallwayType
boxplot(house$YearBuilt ~ house$HallwayType)

# AptManageType
boxplot(house$YearBuilt ~ house$AptManageType)
barplot(table(house$AptManageType))

# TimeToBusStop
boxplot(house$Price.per.Size ~ house$TimeToBusStop)
barplot(table(house$TimeToBusStop))

# TimeToSubway
boxplot(house$Price.per.Size ~ house$TimeToSubway)
with(subset(house, TimeToSubway == "15min~20min"), boxplot(Price.per.Size ~ YearBuilt))

# Facility & School
boxplot(house$Price.per.Size ~ house$N_FacilitiesNearBy.PublicOffice.)
boxplot(house$Price.per.Size ~ house$N_FacilitiesNearBy.Hospital.)
boxplot(house$Price.per.Size ~ house$N_FacilitiesNearBy.Dpartmentstore.)
boxplot(house$Price.per.Size ~ house$N_FacilitiesNearBy.Mall.)
boxplot(house$Price.per.Size ~ house$N_FacilitiesNearBy.ETC.)
boxplot(house$Price.per.Size ~ house$N_FacilitiesNearBy.Park.)
boxplot(house$Price.per.Size ~ house$N_FacilitiesNearBy.Total.)

boxplot(house$Price.per.Size ~ house$N_SchoolNearBy.Elementary.)
boxplot(house$Price.per.Size ~ house$N_SchoolNearBy.Total.)
boxplot(house$Price.per.Size ~ house$N_SchoolNearBy.Middle.)
boxplot(house$Price.per.Size ~ house$N_SchoolNearBy.High.)
boxplot(house$Price.per.Size ~ house$N_SchoolNearBy.University.)

boxplot(house$Price.per.Size ~ house$N_FacilitiesInApt)

corrplot(cor(house[, c(35, grep("FacilitiesNearBy", names(house)), grep("SchoolNearBy", names(house)))]), method = "ellipse", tl.pos = "", tl.cex = 1.2)

# 최종 변수(linear regression)
house_tmp <- subset(house, select = c(Price.per.Size, DateSold, DateBuilt, Size.p.,
                                      Floor, N_Parkinglot.Ground., N_Parkinglot.Basement.,
                                      TimeToSubway, N_manager, SubwayStation,
                                      N_FacilitiesNearBy.Park., N_FacilitiesNearBy.Mall.,
                                      N_FacilitiesNearBy.Total., N_SchoolNearBy.Total.))

# 최종 변수(randomforest, gradientBoosting)
house_tree <- subset(house, select = c(Price.per.Size, DateSold, DateBuilt, Size.p.,
                                       Floor, HallwayType, HeatingType, AptManageType,
                                       N_Parkinglot.Ground., N_Parkinglot.Basement.,
                                       TimeToBusStop, TimeToSubway, N_APT, N_manager,
                                       N_elevators, SubwayStation, N_FacilitiesNearBy.PublicOffice.,
                                       N_FacilitiesNearBy.Hospital., N_FacilitiesNearBy.Dpartmentstore.,
                                       N_FacilitiesNearBy.Mall., N_FacilitiesNearBy.ETC., N_FacilitiesNearBy.Park.,
                                       N_SchoolNearBy.Elementary., N_SchoolNearBy.Middle., N_SchoolNearBy.High.,
                                       N_SchoolNearBy.University., N_FacilitiesInApt,
                                       N_FacilitiesNearBy.Total., N_SchoolNearBy.Total.))

# data partition
# train index
set.seed(1234)
train_ind <- sample(1:nrow(house_tmp), nrow(house_tmp) * 0.7)

train <- house_tmp[train_ind, ]
train_X <- train[, -1]
train_Y <- house[train_ind, "SalePrice"]
test <- house_tmp[-train_ind, ]
test_X <- test[, -1]
test_Y <- house[-train_ind, "SalePrice"]

# regression modeling

par(mfrow = c(2, 2))
plot(lm(Price.per.Size ~ as.numeric(DateSold), data = train))
par(mfrow = c(1, 1))

model1 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2), data = train)
summary(model1)
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

model2 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt), data = train)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

model3 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + Floor, data = train)
summary(model3)
par(mfrow = c(2, 2))
plot(model3)
par(mfrow = c(1, 1))

model4 <- lm(sqrt(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + Floor, data = train)
summary(model4)
par(mfrow = c(2, 2))
plot(model4)
par(mfrow = c(1, 1))

model5 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, data = train)
summary(model5)
par(mfrow = c(2, 2))
plot(model5)
par(mfrow = c(1, 1))

model5_1 <- lm(log(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, data = train)
summary(model5_1)
par(mfrow = c(2, 2))
plot(model5_1)
par(mfrow = c(1, 1))

# 최종 모델
model5_2 <- lm(sqrt(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, data = train)
summary(model5_2)
par(mfrow = c(2, 2))
plot(model5_2)
par(mfrow = c(1, 1))

# BIC 기준 step
model5_2_step1 <- step(model5_2, direction = "both", k = 2*log(nrow(train)))
par(mfrow = c(2, 2))
plot(model5_2_step1)
par(mfrow = c(1, 1))
summary(model5_2_step1)

# AIC 기준 step
model5_2_step2 <- step(model5_2, direction = "both")
par(mfrow = c(2, 2))
plot(model5_2_step2)
par(mfrow = c(1, 1))

# SalePrice 예측 값 (model의 target 변수 form에 따라 바꿔줄 것)
predicted_Y <- predict(model5_2, test)^2 * test$Size.p.

# 평가지표
summary(model1)$adj
summary(model5)$adj
summary(model5_1)$adj
summary(model5_2)$adj

rmse(predict(model1, test) * test$Size.p., test_Y)
rmse(predict(model5, test) * test$Size.p., test_Y)
rmse(exp(predict(model5_1, test)) * test$Size.p., test_Y)
rmse(predicted_Y, test_Y)

mape(predict(model1, test) * test$Size.p., test_Y)
mape(predict(model5, test) * test$Size.p., test_Y)
mape(exp(predict(model5_1, test)) * test$Size.p., test_Y)
mape(predicted_Y, test_Y)

mae(predict(model1, test) * test$Size.p., test_Y)
mae(predict(model5, test) * test$Size.p., test_Y)
mae(exp(predict(model5_1, test)) * test$Size.p., test_Y)
mae(predicted_Y, test_Y)


# linear regression 시각화
# Ggplot2
library(ggfortify)

# model1
md1 <- autoplot(model1, smooth.colour = "red", col = "black") + theme_bw()
md1

# model5
md2 <- autoplot(model5, smooth.colour = "red", col = "black") + theme_bw()
md2

# model5_1
md3 <- autoplot(model5_1, smooth.colour = "red", col = "black") + theme_bw()
md3

# model5_2
md4 <- autoplot(model5_2, smooth.colour = "red", col = "black") + theme_bw()
md4



##################################
# tree 기반 modeling
# data partition
train_tree <- house_tree[train_ind, ]
test_tree <- house_tree[-train_ind, ]

# randomForest modeling
set.seed(1234)
rf.fit = randomForest(Price.per.Size ~ ., data = train_tree, mtry = floor(ncol(train_tree) / 3), ntree = 500, importance = T)
rf.fit

set.seed(1234)
rf.fit_40 = randomForest(Price.per.Size ~ ., data = train_tree, mtry = floor(ncol(train_tree) / 3), ntree = 40, importance = T)

set.seed(1234)
rf.fit_100 = randomForest(Price.per.Size ~ ., data = train_tree, mtry = floor(ncol(train_tree) / 3), ntree = 100, importance = T)

plot(rf.fit, main = "randomforest")
plot(rf.fit, main = "randomforest", xlim = c(0, 100))

importance(rf.fit)
importance(rf.fit_40)
importance(rf.fit_100)

par(mai = c(0.5, 3, 0.3, 3))
barplot(importance(rf.fit)[, 1], main = "ntree = 500                     ", horiz = T, las = 1)
abline(v = 10, col = "red", lwd = 2)
dev.off()

par(mai = c(0.5, 3, 0.3, 3))
barplot(importance(rf.fit_40)[, 1], main = "ntree = 40                     ", horiz = T, las = 1)
abline(v = 10, col = "red", lwd = 2)
dev.off()

par(mai = c(0.5, 3, 0.3, 3))
barplot(importance(rf.fit_100)[, 1], main = "ntree = 100                     ", horiz = T, las = 1)
abline(v = 10, col = "red", lwd = 2)
dev.off()

predicted_rf_Y <- predict(rf.fit, test_tree) * test$Size.p.
predicted_rf40_Y <- predict(rf.fit_40, test_tree) * test$Size.p.
predicted_rf100_Y <- predict(rf.fit_100, test_tree) * test$Size.p.

rmse(predicted_rf_Y, test_Y)
rmse(predicted_rf40_Y, test_Y)
rmse(predicted_rf100_Y, test_Y)

mape(predicted_rf_Y, test_Y)
mape(predicted_rf40_Y, test_Y)
mape(predicted_rf100_Y, test_Y)

mae(predicted_rf_Y, test_Y)
mae(predicted_rf40_Y, test_Y)
mae(predicted_rf100_Y, test_Y)


ggplot(rf.fit,aes(x=predicted_rf_Y,y=test_Y))


ggplot(aes(x=predicted_rf_Y, y=test_Y),
  data=data.frame(actual=test$alcohol, pred=predict(rf, test[,cols])))
p + geom_point() +
	geom_abline(color="red") +
	ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))

ggplot(data=test_tree, aes(x=predicted_rf_Y, y=test_Y)) + theme_bw() +
    geom_point() +
    geom_abline(color="red",size=1) 


# GradientBoosting
set.seed(1234)
fit.gbm1 <- gbm(Price.per.Size ~ as.numeric(DateSold) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, 
               distribution = "multinomial", n.trees = 40, verbose = T,
               interaction.depth = 3, shrinkage = 0.001, data = train_tree)

set.seed(1234)
fit.gbm2 <- gbm(Price.per.Size ~ as.numeric(DateSold) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, 
                distribution = "multinomial", n.trees = 100, verbose = T,
                interaction.depth = 3, shrinkage = 0.001, data = train_tree)

set.seed(1234)
fit.gbm3 <- gbm(Price.per.Size ~ as.numeric(DateSold) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, 
                distribution = "multinomial", n.trees = 40, verbose = T,
                interaction.depth = 3, shrinkage = 0.01, data = train_tree)

set.seed(1234)
fit.gbm4 <- gbm(Price.per.Size ~ as.numeric(DateSold) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, 
                distribution = "multinomial", n.trees = 100, verbose = T,
                interaction.depth = 3, shrinkage = 0.01, data = train_tree)

relative.influence(fit.gbm1)
relative.influence(fit.gbm2)
relative.influence(fit.gbm3)
relative.influence(fit.gbm4)

predicted_gbm1_Y <- predict(fit.gbm1, test_tree, n.trees = 40) * test$Size.p.
predicted_gbm2_Y <- predict(fit.gbm2, test_tree, n.trees = 100) * test$Size.p.
predicted_gbm3_Y <- predict(fit.gbm3, test_tree, n.trees = 40) * test$Size.p.
predicted_gbm4_Y <- predict(fit.gbm4, test_tree, n.trees = 100) * test$Size.p.

rmse(predicted_gbm1_Y, test_Y)
rmse(predicted_gbm2_Y, test_Y)
rmse(predicted_gbm3_Y, test_Y)
rmse(predicted_gbm4_Y, test_Y)

mae(predicted_gbm1_Y, test_Y)
mae(predicted_gbm2_Y, test_Y)
mae(predicted_gbm3_Y, test_Y)
mae(predicted_gbm4_Y, test_Y)

mape(predicted_gbm1_Y, test_Y)
mape(predicted_gbm2_Y, test_Y)
mape(predicted_gbm3_Y, test_Y)
mape(predicted_gbm4_Y, test_Y)