###오늘 한거 : 변수 TimeToSubway 중  no_bus_stop_nearby 범주 어떻게 할 것 인지?

unique(house1$TimeToSubway)

#지하철 역이 없다고?=> 왜 버스정류장이 없다고 나와있을까
?plot

format(as.Date(as.character(c(house1$YrSold,house1$MonthSold)),format="%Y%b"))
str(house1$YrSold)

house1$YMSold<-paste(house1$YrSold,house1$MonthSold)

str(house1$YMSold)



plot(house1$YMSold,y)


par(mfrow=c(1,2))
plot(house1$YrSold,y)

plot(house1$MonthSold,y)

with(house1,apply(

MSold_07<-subset(house$YrSold==2007,house$MonthSold==1)
yyy<-subset(house$SalePrice,house$MonthSold==1)

MSold_07<-subset(house,YrSold==2007)
MSold_08<-subset(house,YrSold==2008)
MSold_09<-subset(house,YrSold==2009)
MSold_10<-subset(house,YrSold==2010)
MSold_11<-subset(house,YrSold==2011)
MSold_12<-subset(house,YrSold==2012)
MSold_13<-subset(house,YrSold==2013)
MSold_14<-subset(house,YrSold==2014)
MSold_15<-subset(house,YrSold==2015)
MSold_16<-subset(house,YrSold==2016)
MSold_17<-subset(house,YrSold==2017)

plot(MSold_07$MonthSold,MSold_07$SalePrice)
plot(MSold_08$MonthSold,MSold_08$SalePrice)
plot(MSold_09$MonthSold,MSold_09$SalePrice)
plot(MSold_10$MonthSold,MSold_10$SalePrice)
plot(MSold_11$MonthSold,MSold_11$SalePrice)
plot(MSold_12$MonthSold,MSold_12$SalePrice)
plot(MSold_13$MonthSold,MSold_13$SalePrice)
plot(MSold_14$MonthSold,MSold_14$SalePrice)
plot(MSold_15$MonthSold,MSold_15$SalePrice)
plot(MSold_16$MonthSold,MSold_16$SalePrice)
plot(MSold_17$MonthSold,MSold_17$SalePrice)

unique(MSold_07$MonthSold)
str(MSold_07)
