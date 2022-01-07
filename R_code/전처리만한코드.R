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

?write.csv
write.csv(house,file="new_house.csv",row.names = F)
