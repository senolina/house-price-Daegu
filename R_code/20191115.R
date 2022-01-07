##### ~2019-11-12 #####

###경로설정

setwd("C:/Users/HOME/Desktop/대구집값")


###data 불러오기 (house로 통일)

house<-read.csv("Daegu_Real_Estate_data.csv")


###data 구조 확인

dim(house) #변수 30개/ 관측치 5891개

str(house) #타깃변수:SalePrice (연속형) // 날짜 변수 3개 // 양적변수- 연속형 1? , 이산형 19 // 질적변수 - 범주형 6 => 총 30개 확인 


###결측치 확인

colSums(is.na(house)) #결측치 없음


###타깃변수 객체 지정

y<-house$SalePrice


###연속형 변수 선택

plot(house$Size.sqf. , y, xlim=c(100,2500),ylim=c(32000,600000)) #평수를 연속형으로 보기 어렵나...?=> 한 x당 해당하는 y가 여러개이기 때문에// 근데 나름 선형적(x증가 할 수록 y 증가)


###이산형(숫자형)
x_num<-house[,c(6,10,11,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30)]

head(x_num)
str(x_num)


##일단 int형 num형으로 바꿔주기

x_num$Floor<-as.numeric(x_num$Floor)
x_num$N_FacilitiesNearBy.Hospital. <-as.numeric(x_num$N_FacilitiesNearBy.Hospital.)
x_num$N_FacilitiesInApt <-as.numeric(x_num$N_FacilitiesInApt)

str(x_num)


plot(x_num[,1],y)
plot(x_num[,2],y)
plot(x_num[,3],y)
plot(x_num[,4],y)
plot(x_num[,5],y)
plot(x_num[,6],y)
plot(x_num[,7],y)
plot(x_num[,8],y)
plot(x_num[,9],y)
plot(x_num[,10],y)
plot(x_num[,11],y)
plot(x_num[,12],y)
plot(x_num[,13],y)
plot(x_num[,14],y)
plot(x_num[,15],y)
plot(x_num[,16],y)
plot(x_num[,17],y)
plot(x_num[,18],y)
plot(x_num[,19],y)


hist(x_num[,1])
hist(x_num[,2])
hist(x_num[,3])
hist(x_num[,4])
hist(x_num[,5])
hist(x_num[,6])
hist(x_num[,7])
hist(x_num[,8])
hist(x_num[,9])
hist(x_num[,10])
hist(x_num[,11])
hist(x_num[,12])
hist(x_num[,13])
hist(x_num[,14])
hist(x_num[,15])
hist(x_num[,16])
hist(x_num[,17])
hist(x_num[,18])
hist(x_num[,19])


boxplot(y~x_num[,1])
boxplot(y~x_num[,2])
boxplot(y~x_num[,3])
boxplot(y~x_num[,4])
boxplot(y~x_num[,5])
boxplot(y~x_num[,6])
boxplot(y~x_num[,7])
boxplot(y~x_num[,8])
boxplot(y~x_num[,9])
boxplot(y~x_num[,10])
boxplot(y~x_num[,11])
boxplot(y~x_num[,12])
boxplot(y~x_num[,13])
boxplot(y~x_num[,14])
boxplot(y~x_num[,15])
boxplot(y~x_num[,16])
boxplot(y~x_num[,17])
boxplot(y~x_num[,18])
boxplot(y~x_num[,19])

table(x_num[,1])
table(x_num[,2])
table(x_num[,3])
table(x_num[,4])
table(x_num[,5])
table(x_num[,6])
table(x_num[,7])
table(x_num[,8])
table(x_num[,9])
table(x_num[,10])
table(x_num[,11])
table(x_num[,12])
table(x_num[,13])
table(x_num[,14])
table(x_num[,15])
table(x_num[,16])
table(x_num[,17])
table(x_num[,18])
table(x_num[,19])



###범주형 변수 
x_fac<-house[,c(7,8,9,12,13,17)]

head(x_fac)
str(x_fac)


with(house,boxplot(SalePrice~HallwayType))
with(house,boxplot(SalePrice~HeatingType))
with(house,boxplot(SalePrice~AptManageType))
with(house,boxplot(SalePrice~TimeToBusStop))
with(house,boxplot(SalePrice~TimeToSubway))
with(house,boxplot(SalePrice~SubwayStation))


with(house,table(HallwayType))
with(house,table(HeatingType))
with(house,table(AptManageType))
with(house,table(TimeToBusStop))
with(house,table(TimeToSubway))
with(house,table(SubwayStation))


######변수 여러개 섞어서 보기

aggregate(house$N_Parkinglot.Ground. ~ house$N_Parkinglot.Basement., FUN = mean)
aggregate(house$N_Parkinglot.Ground., list(house$N_manager, house$N_elevators), FUN = mean)


house1<-house
str(house1)

##지상주차장 + 지하주차장 => 새로운 변수 생성
 
house1$N_Parkinglot.Sum<-apply(house1[,c(10,11)],1,sum)



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

par(mfrow=c(3,4))

summary(house$SalePrice)
summary(MSold_08$SalePrice)

windows()
windows()
windows()
windows()
windows()
windows()
windows()
windows()
windows()
windows()


plot(MSold_07$MonthSold,MSold_07$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_08$MonthSold,MSold_08$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_09$MonthSold,MSold_09$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_10$MonthSold,MSold_10$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_11$MonthSold,MSold_11$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_12$MonthSold,MSold_12$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_13$MonthSold,MSold_13$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_14$MonthSold,MSold_14$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_15$MonthSold,MSold_15$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_16$MonthSold,MSold_16$SalePrice,xlim=c(1,12),ylim=c(30000,600000))
plot(MSold_17$MonthSold,MSold_17$SalePrice,xlim=c(1,12),ylim=c(30000,600000))






plot(MSold_07$MonthSold)
plot(MSold_08$MonthSold)
plot(MSold_09$MonthSold)
plot(MSold_10$MonthSold)
plot(MSold_11$MonthSold)
plot(MSold_12$MonthSold)
plot(MSold_13$MonthSold)
plot(MSold_14$MonthSold)
plot(MSold_15$MonthSold)
plot(MSold_16$MonthSold)
plot(MSold_17$MonthSold)


dim(MSold_07)
dim(MSold_08)
dim(MSold_09)
dim(MSold_10)
dim(MSold_11)
dim(MSold_12)
dim(MSold_13)
dim(MSold_14)
dim(MSold_15)
dim(MSold_16)
dim(MSold_17)

summary(MSold_13$SalePrice)

head(MSold_13,10)

unique(MSold_07$MonthSold)
str(MSold_07)
str(yyy)


a<-aggregate(house1$YMSold ,list(house1$MonthSold , house1$MonthSold), FUN = paste)

str(a)



#####20191115#####

###단지별,평수별,,등등####


part <- aggregate((house$N_Parkinglot.Ground. + house$N_Parkinglot.Basement.), list(house$N_elevators, house$YearBuilt, house$N_manager), FUN = unique)


house$group <- NA
for(i in 1:nrow(house)){
    for(j in 1:nrow(part)){
      if((house$N_elevators[i] == part[j, 1]) & (house$YearBuilt[i] == part[j, 2]) & (house$N_manager[i] == part[j, 3])){
        house$group[i] <- j
      }
    }
}  ###한 줄 한 줄 그룹화 시켜줬다는 뜻
 

table(house$group) 


aggregate(house$Size.sqf. ~ house$group, FUN = unique)

house$Size.p.<-round(house$Size.sqf./35.583) ##평방피트를 평으로 변환

table(house$Size.p.)

aggregate(house$Size.p. ~ house$group, FUN = unique)  ##단지별 평 수를 볼 것임


##군집분석
library(cluster)

set.seed(1234)


cluster1 <- kmeans(house[, 18:27], centers = 8)
cluster1
head(house)

clust_group1 <- cluster1$cluster
aggregate(clust_group1 ~ house$SubwayStation, FUN = unique)


cluster1$centers





RNGkind("Mersenne-Twister")
set.seed(1234)
cluster1 <- kmeans(house[, 18:27], centers = 8)
cluster1

clust_group1 <- cluster1$cluster
aggregate(clust_group1 ~ house$SubwayStation, FUN = unique)



###잘 분류됐는지 확인하기
library(ggplot2)
cluster1$cluster <- as.factor(cluster1$cluster)
cluster<-c(1,2,3,4,5,6,7,8)
qplot(clust_group1, SubwayStation, data = house)
 

 
###업데이트



# installing/loading the package:
 if(!require(installr)) {
+ install.packages("installr"); require(installr)}
# using the package:
updateR()
install.packages("installr")

install.packages("car")


