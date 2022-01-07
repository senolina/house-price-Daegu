library(ggplot2)
library(ggmap)
library(clustMixType)

## clean code data analysis
setwd("C:/Users/HOME/Desktop/대구집값")
house <- read.csv("Daegu_Real_Estate_data.csv")

# 데이터 단지별 분류된 변수 group 생성
part <- aggregate((house$N_Parkinglot.Ground. + house$N_Parkinglot.Basement.), list(house$N_elevators, house$YearBuilt, house$N_manager), FUN = unique)  ##일단 이거 정확한 해석을 모르겠음 ㅜㅜ
# 그럼 part 는 같은 엘레베이터 수 , 같은 지어진 년도, 같은 관리자 수 별 지하주차장과 지상주차상 수의 합 인것=> 같은 단지로 (26개) 판단한다는 것?


house$group <- NA

for(i in 1:nrow(house)){
  for(j in 1:nrow(part)){
    if((house$N_elevators[i] == part[j, 1]) & (house$YearBuilt[i] == part[j, 2]) & (house$N_manager[i] == part[j, 3])){
      house$group[i] <- j
    }
  }
}
##part보면 26개의 단지라는 것을 알 수 있음 그 기준이 같은 엘레베이터 수 , 같은 지어진 년도, 같은 관리자 수 / 그래서 이 세 개가 모두 같으면 같은 단지로 지정해 주는 것임

# 면적 평방피트를 평으로 변환한 새로운 변수 Size.p. 생성
house$Size.p. <- round(house$Size.sqf. / 35.583)


# TimeTo 변수 level 순서 수정 및 no_bus_stop level 수정 0-5min -> 0~5min으로 수정
levels(house$TimeToSubway) <- levels(house$TimeToSubway)[c(1, 4, 2, 3, 5)]
levels(house$TimeToSubway)[5] <- "no_subway_station_nearby"
levels(house$TimeToSubway)[1] <- "0~5min"
levels(house$TimeToBusStop) <- levels(house$TimeToBusStop)[c(1, 3, 2)]

house$SubwayStation[house$group == 25] <- "Sin-nam"
house$SubwayStation[house$group == 11] <- "Daegu"


# 평당 가격 변수 Price.per.Size 생성
house$Price.per.Size <- house$SalePrice / house$Size.p.


# 총 주차장 수로 작은 단지, 중간 단지, 큰 단지로 묶음  
house$complex <- as.numeric(cut((house$N_Parkinglot.Basement. + house$N_Parkinglot.Ground.),
                                breaks = c(0, 500, 1000, 1500), include.lowest = T))  ##기준..?
plot(house$N_Parkinglot.Basement. + house$N_Parkinglot.Ground.)
 
# 건축일자와 거래일자 Date로 바꿔줌
# 건축일자 : YearBuilt - 01 - 01
# 거래일자 : YrSold - MonthSold - 01
house$DateSold <- as.Date(with(house, 
                               paste(YrSold, ifelse(MonthSold < 10, paste("0", MonthSold, sep = ""), MonthSold), "01", sep = "-")))

house$DateBuilt <- as.Date(paste(house$YearBuilt, "-01-01", sep = ""))


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

##TimeToBusStop 변수 수치형으로 변환

house$num_TTBus<-NA


for(i in 1:nrow(house)){
    if((house$ TimeToBusStop[i] == "0~5min")){
      house$num_TTBus[i] <- 2.5
    }
	else if(house$ TimeToBusStop[i]=="5min~10min"){
		house$num_TTBus[i] <- 7.5
  }
	else if(house$ TimeToBusStop[i]=="10min~15min"){
		house$num_TTBus[i] <- 12.5
}
}


##TimeToSubway 변수 수치형으로 변환

house$num_TTSub<-NA
table(house$TimeToSubway)

for(i in 1:nrow(house)){
    if((house$TimeToSubway[i] == "0~5min")){
      house$num_TTSub[i] <- 2.5
	}
	else if(house$TimeToSubway[i]=="5min~10min"){
		house$num_TTSub[i] <- 7.5
	}
	else if(house$TimeToSubway[i]=="10min~15min"){
		house$num_TTSub[i] <- 12.5
	}
	else if(house$TimeToSubway[i]=="15min~20min"){
		house$num_TTSub[i] <- 17.5
	}
	else if(house$TimeToSubway[i]=="no_subway_station_nearby"){
		house$num_TTSub[i] <- 30
}
}






