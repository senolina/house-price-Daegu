library(corrplot)
library(Metrics)
library(randomForest)
library(caret)
library(gbm)

## clean code data analysis
setwd("C:/Users/HOME/Desktop/대구집값")
house <- read.csv("Daegu_Real_Estate_data.csv")

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



# 동 당 엘레베이터 수와 평당 가격 비교

house$Prob_elevators <- (house$N_elevators + 0.2) / house$N_APT




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


##1 Target ~ poly(DateSold, 2)

model1 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2), data = train)
summary(model1)
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

 

windows()
##2 Target ~ poly(DateSold, 2) + .

model2 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, data = train)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))


##3 Log(Target) ~ poly(DateSold, 2) + .

model3 <- lm(log(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, data = train)
summary(model3)
par(mfrow = c(2, 2))
plot(model3)
par(mfrow = c(1, 1))


##4 Sqrt(Target) ~ poly(DateSold, 2) + .

model4 <- lm(sqrt(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + . - DateSold - DateBuilt, data = train)
summary(model4)
par(mfrow = c(2, 2))
plot(model4)
par(mfrow = c(1, 1))




## model 1
predicted_Y <- predict(model1, test) * test$Size.p.

# 평가지표
rmse(predicted_Y, test_Y)
mape(predicted_Y, test_Y)

plot(model1, 2)




## model 2
predicted_Y <- predict(model2, test) * test$Size.p.

# 평가지표
rmse(predicted_Y, test_Y)
mape(predicted_Y, test_Y)

plot(model2, 2)





## model 3
predicted_Y <- exp(predict(model3, test)) * test$Size.p.

# 평가지표
rmse(predicted_Y, test_Y)
mape(predicted_Y, test_Y)

plot(model3, 2)




## model 4
predicted_Y <- predict(model4, test)^2 * test$Size.p.

# 평가지표
rmse(predicted_Y, test_Y)
mape(predicted_Y, test_Y)

plot(model4, 2)












####이거 ggplot #####


# Ggplot2 -----------------------------------------------------------------

#install.packages("ggfortify")
library(ggfortify)


##model 1

md1<-autoplot(model1)+theme_bw()
md1


##model 2

md2<-autoplot(model2)+theme_bw()
md2


##model 3
md3<-autoplot(model3,="red")+theme_bw()
md3
?autoplot

##model 4
md4<-autoplot(model4)+theme_bw()
md4
