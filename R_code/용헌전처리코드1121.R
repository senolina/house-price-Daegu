#install.packages("ggmap")
#install.packages("clustMixType")
library(ggmap)
library(ggplot2)
library(clustMixType)

## clean code data analysis
setwd("C:/Users/HOME/Desktop/대구집값")
house <- read.csv("Daegu_Real_Estate_data.csv")

# 결측치 확인
sum(!complete.cases(house))  # 결측치 0개

# target 변수 정규성 만족 여부 확인
qqnorm(house$SalePrice)  # 정규성 만족  ##??어떻게 아는거여
qqline(house$SalePrice,col="red")  ###****확인


# 데이터 단지별 분류된 변수 group 생성
part <- aggregate((house$N_Parkinglot.Ground. + house$N_Parkinglot.Basement.), list(house$N_elevators, house$YearBuilt, house$N_manager), FUN = unique)  ##일단 이거 정확한 해석을 모르겠음 ㅜㅜ
# 그럼 part 는 같은 엘레베이터 수 , 같은 지어진 년도, 같은 관리자 수 별 지하주차장과 지상주차상 수의 합 인것=> 같은 단지로 (26개) 판단한다는 것?


aggregate(house$Size.sqf., list(house$N_elevators, house$YearBuilt, house$N_manager), FUN = unique)
#같은 엘레베이터 수 , 같은 지어진 년도, 같은 관리자 수 별 집 크기 (유니크)==> 26개 인거 다시 보는 것

house$group <- NA

for(i in 1:nrow(house)){
  for(j in 1:nrow(part)){
    if((house$N_elevators[i] == part[j, 1]) & (house$YearBuilt[i] == part[j, 2]) & (house$N_manager[i] == part[j, 3])){
      house$group[i] <- j
    }
  }
}
##part보면 26개의 단지라는 것을 알 수 있음 그 기준이 같은 엘레베이터 수 , 같은 지어진 년도, 같은 관리자 수 / 그래서 이 세 개가 모두 같으면 같은 단지로 지정해 주는 것임

head(house$group)  ##단지를 정한거임 (확인하려고)



# 면적 평방피트를 평으로 변환한 새로운 변수 Size.p. 생성
house$Size.p. <- round(house$Size.sqf. / 35.583)


# TimeTo 변수 level 순서 수정 및 no_bus_stop level 수정 0-5min -> 0~5min으로 수정
levels(house$TimeToSubway) <- levels(house$TimeToSubway)[c(1, 4, 2, 3, 5)]
levels(house$TimeToSubway)[5] <- "no_subway_station_nearby"
levels(house$TimeToSubway)[1] <- "0~5min"
levels(house$TimeToBusStop) <- levels(house$TimeToBusStop)[c(1, 3, 2)]

# 평당 가격 변수 Price.per.Size 생성
house$Price.per.Size <- house$SalePrice / house$Size.p.

# 총 주차장 수로 작은 단지, 중간 단지, 큰 단지로 묶음  
house$complex <- as.numeric(cut((house$N_Parkinglot.Basement. + house$N_Parkinglot.Ground.),
                                breaks = c(0, 500, 1000, 1500), include.lowest = T))  ##기준..?
 
# perfect 라고는 말할 수 없지만, 작은 단지에 작은 평수, 큰 단지에 큰 평수가 주로 포진해있는 것을 확인
aggregate(house$Size.p. ~ house$complex, FUN = unique)

# 눈에 띄게 보이는 것은
# 대단지가 반월당, 경대병원, 명덕, no_subway(3호선 건들바위역으로 추정) 가운데 지대에 포진해있는 것을 확인
# 반고개에는 중형 아파트만 위치
# 칠성시장, 대구역 부근에는 아파트단지 거의 없음     

##여긴 지도를 같이 봐야할 것 같음


100 * prop.table(table(house$complex, house$SubwayStation), margin = 1)  # 조건부 비율(complex 별 비율) (행별 합이 100)
100 * prop.table(table(house$complex, house$SubwayStation))  # 전체 비율 (전체 합이 100)
#근데 이건 굳이 비율로 봐야할까..? 


# 건축일자와 거래일자 Date로 바꿔줌
# 건축일자 : YearBuilt - 01 - 01
# 거래일자 : YrSold - MonthSold - 01
house$DateSold <- as.Date(with(house, 
                               paste(YrSold, ifelse(MonthSold < 10, paste("0", MonthSold, sep = ""), MonthSold), "01", sep = "-")))

house$DateBuilt <- as.Date(paste(house$YearBuilt, "-01-01", sep = ""))

# Facility와 School 등이 동 기준인지 단지 기준인지 대략적으로 확인
aggregate(house$TimeToSubway ~ house$group, FUN = unique)
aggregate(house$N_FacilitiesNearBy.Park. ~ house$group, FUN = unique)
aggregate(house$HallwayType ~ house$group, FUN = unique)
aggregate(house$HeatingType ~ house$group, FUN = unique)
aggregate(house$TimeToSubway ~ house$group, FUN = unique)
aggregate(house$N_SchoolNearBy.Elementary. ~ house$group, FUN = unique)
# 단지 기준인 것으로 확인되었음










==>1122나형여기까지


#### 시계열 제거

# 거래 가격으로 1차 피팅 시 residual plot이 약간의 U자 모양을 띄어서 2차 피팅함
# 2차 피팅 후에도 계단신 resudial plot을 띄는 구간이 있어 평당 가격으로 피팅 시도
par(mfrow = c(2, 2))
plot(lm(SalePrice ~ I(as.numeric(DateSold)) + I(as.numeric(DateSold)^2), data = house))

# 거래일 변수 기준 회귀 fitting 후 residual 확인 시 U자 곡선 형태를 보임
# 따라서 제곱항 추가해줌
# DateSold 변수는 date type이기 때문에 numeric으로 바꿔도 문제 되지 않음
# 큰 차이는 없지만 2차항 모델이 꼬리부분에서 좀 더 정규성을 보이는 것으로 보임

# 1차 fitting
par(mfrow = c(2, 2))
model1_price.date <- lm(Price.per.Size ~ I(as.numeric(DateSold)), data = house)
plot(model1_price.date)

# 2차 fitting
par(mfrow = c(2, 2))
model2_price.date <- lm(Price.per.Size ~ I(as.numeric(DateSold)) + I(as.numeric(DateSold)^2), data = house)
summary(model2_price.date)
anova(model2_price.date)

plot(model2_price.date)
par(mfrow = c(1, 1))

# 시계열 제거한 값으로 SalePrice_Time 변수 생성
house$SalePrice_Time <- summary(model2_price.date)$residual
plot(house$SalePrice_Time ~ house$Floor)
lines(lowess(x = house$Floor, y = house$SalePrice_Time), col = "red", lwd = 2)

names(house)

# 추가적으로 평당 가격과 건축일자의 plot에서 2000년대 중반 이후 큰폭으로 상승하는 경향을 띄는 것은
# 최근에 지어졌을 수록 가격이 높을 것이다라는 가설1과
# 2010년을 전후로 하여 부동산 시장이 전체적으로 급상승하는 경향을 보였기 때문에
# 그 부근에 지어진 아파트는 당연히 가격이 높게 형성되있다는 가설2가 있음
# 이를 확인하기 위해서 시간의 흐름을 제거한 후 건축일자와 plot을 그려본 결과
# 기존의 그래프와는 다르게 1980년대부터 고른 성장을 보이는 것을 파악
# 2010년 전후의 부동산 시장 급등 요소를 잘 제거한 후 건축일자를 변수로 사용하는데 문제가 없다는 것을 보여줌
# 잘 모르겠음

boxplot(house$Price.per.Size ~ house$DateBuilt)
lines(lowess(x = house$DateBuilt, y = house$Price.per.Size), col = "red", lwd = 2)

boxplot(house$SalePrice_Time ~ house$DateBuilt)
lines(lowess(x = house$DateBuilt, y = house$SalePrice_Time), col = "red", lwd = 2)

# 그래서 거래일자와 건축일자를 모두 변수에 포함하여 residual plot을 그린 결과 우측으로 갈 수록 분산이 커지는 모양을 보임
model1_second <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt), data = house)
summary(model1_second)
par(mfrow = c(2, 2))
plot(model1_second)
par(mfrow = c(1, 1))

# 따라서 sqrt(target)변수로 residual plot 확인 결과 increasing trend 제거된 것으로 확인
model2_second <- lm(sqrt(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt), data = house)
summary(model2_second)
par(mfrow = c(2, 2))
plot(model2_second)
par(mfrow = c(1, 1))

# 상식적으로 지하철까지 거리에 집값이 영향을 많이 받는다고 알려져있기 때문에 plot확인 결과 딱히 안보임
plot(house$SalePrice_Time ~ house$TimeToSubway)
table(house$TimeToSubway, house$N_manager)

# 지하철까지 거리를 고정시킨 상태에서 N_manager(단지 규모라고 생각함) 별 시간 제거한 상태의 평당 가격 확인
with(subset(house, TimeToSubway == "0~5min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "5min~10min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "10min~15min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "15min~20min"), boxplot(SalePrice_Time ~ N_manager))

# 0~5min 이상치 확인 N_manager == 7인 경우 아래꼬리에 이상치 다수 발생
# 확인 결과 group 21에 해당하는 단지로 10년간 총 거래 건수가 4건밖에 없는 것을 확인
# group 21 단지는 N_APT도 10이고, 꽤 큰 단지로 추정되는데 이상함
# 제거할지에 대한 논의 필요
# 추가로 확인 결과 21번 단지와 22번 단지가 건축일자는 다르지만 다른 변수가 모두 같은 것으로 확인
# 재건축이 아닐까 싶음...
# 만약 다른 재건축이 있다고 가정했을 때, 다 걸러내야함
subset(house, TimeToSubway == "0~5min" & SalePrice_Time < -1000 & N_manager == 7)
nrow(subset(house, group == 21))
subset(house, group == 22)
unique(subset(house, group == 22)$Size.p.)
subset(house, group == 21)

# 5~10min 이상치 확인 N_manager == 3이 5, 8보다 평당 가격, 시간 제거 후 평당 가격 모두 높음
# 확인 결과 5, 8인 데이터는 각각 1992년, 1986년에 지어진 아파트로 확인
table(subset(house, TimeToSubway == "5min~10min", select = c(SubwayStation, N_manager)))
table(subset(house, TimeToSubway == "5min~10min", select = DateBuilt))
with(subset(house, TimeToSubway == "5min~10min"), boxplot(Price.per.Size ~ N_manager))

boxplot(house$SalePrice_Time ~ house$group)
boxplot(house$Price.per.Size ~ house$group)
boxplot(house$SalePrice ~ house$group)

###### 아직 확인중 ######
subset(house, group == 21, select = c(Size.p., DateBuilt, DateSold, N_Parkinglot.Ground., N_Parkinglot.Basement.))

boxplot(aggregate(house$SalePrice_Time ~ house$group, FUN = var)[, 2] / 
          aggregate(house$Price.per.Size ~ house$group, FUN = var)[, 2] ~ c(1:26), ylim = c(0, 1))

var_group <- (aggregate(house$SalePrice_Time ~ house$group, FUN = var)[, 2] / 
                aggregate(house$Price.per.Size ~ house$group, FUN = var)[, 2])

which(var_group > 0.8 & var_group < 1)

subset(house, group == 16, select = c(DateBuilt, DateSold, N_Parkinglot.Ground., N_Parkinglot.Basement.))


# 21번 단지가 이상치로 드러남
# 21번 단지의 경우 1980년 건축되었지만, 총 거래 건 수가 4건임
# 21번 단지는 주차장의 대다수가 지하에 위치하고 있음. 특이함
# 제거 여부 결정

################################################################################
################################################################################


# google map

# 역명 / 위도 / 경도
# 반고개역 / 35.862462 / 128.573531
# 반월당역 / 35.865468 / 128.593369
# 칠성시장역 / 35.876102 / 128.605083
# 대구역 / 35.876293 / 128.597107
# 경대병원역 / 35.863127 / 128.603153
# 명덕역 / 35.857225 / 128.590794
# 청라언덕역(신남) / 35.865122 / 128.583093

# 백화점명 / 위도 / 경도
# 현대백화점 대구점 / 35.865866 / 128.585675
# 대구백화점 / 35.869143 / 128.595984
# 신세계백화점 대구점 / 35.877372 / 128.628534
# 롯데백화점 대구점 / 35.875801 / 128.591183
# 대구백화점 프라자 / 35.854915 / 128.608861
# 동아백화점 쇼핑점 / 35.862347 / 128.597566
# 신세계백화점은 동대구역 부근이라 지도에서 이탈

# 대학교명 / 위도 / 경도
# 계명대 의대 동산캠퍼스 / 35.867132 / 128.582309
# 대구가톨릭대 유스티노캠퍼스 / 35.8620364 / 128.5874015
# 경북대 의대 / 35.868840 / 128.603100
# 경북대 치대 / 35.864434 / 128.608359
# 대구대 대명동캠퍼스 / 35.838730 / 128.575100
# 계명대 대명캠퍼스 / 35.856548 / 128.585985
# 대구교육대 / 35.855367 / 128.590767
# 영남대학교 대구캠퍼스 / 35.854486 / 128.583121


register_google(key = "AIzaSyDuoA5BdZqKJpXIT7CUlGCWtbu_SsTWE7M")

subway_dat <- data.frame(SubwayStation = c("Bangoge", "Banwoldang", "Chil-sung-market", "Daegu", "Kyungbuk_uni_hospital", "Myung-duk", "Sin-nam"),
                         lat = c(35.862462, 35.865468, 35.876102, 35.876293, 35.863127, 35.857225, 35.865122),
                         lon = c(128.573531, 128.593369, 128.605083, 128.597107, 128.603153, 128.590794, 128.583093))
center <- c(mean(subway_dat$lon), mean(subway_dat$lat))

store_dat <- data.frame(Departmentstore = c("hyundai", "daegu", "ssg", "lotte", "daegu2", "donga"),
                        lat = c(35.865866, 35.869143, 35.877372, 35.875801, 35.854915, 35.862347),
                        lon = c(128.585675, 128.595984, 128.628534, 128.591183, 128.608861, 128.597566))

univ_dat <- data.frame(University = c("univ1", "univ2", "univ3", "univ4", "univ5", "univ6", "univ7", "univ8"),
                       lat = c(35.867132, 35.8620364, 35.868840, 35.864434, 35.838730, 35.856548, 35.855367, 35.854486),
                       lon = c(128.582309, 128.5874015, 128.603100, 128.608359, 128.575100, 128.585985, 128.590767, 128.583121))

map <- get_googlemap(center = center, maptype = "roadmap", zoom = 14, marker = subway_dat[, 2:3])
ggmap(map) +
  geom_point(data = subway_dat, aes(lon, lat), col = "blue", size = 4) +
  geom_point(data = store_dat, aes(lon, lat), col = "red", size = 3) +
  geom_point(data = univ_dat, aes(lon, lat), col = "black", size = 3)



# TimeToSubway가 15min~20min인 그룹에 no_subway_nearby 발견
# SubwayStation이 no_subway_nearby인 그룹에 TimeToSubway가 15min~20min이 166거래, no_subway_station_nearby가 238거래
# 이상치 처리 방법 논의
subset(house, TimeToSubway == "15min~20min", SubwayStation)
table(subset(house, SubwayStation == "no_subway_nearby", c(TimeToSubway)))

