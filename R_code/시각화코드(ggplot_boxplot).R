
setwd("C:/Users/HOME/Desktop/대구집값")
house<-read.csv("new_house.csv")

str(house)


library(ggplot2);library(reshape2);library(tree) ; library(caret) ; library(e1071) ;library(randomForest)
library(dplyr)

####### 시각화 #######

#### boxplot ####

##1. 준공일자 / 평당가격

#install.packages("RColorBrewer")

#library(RColorBrewer)


#다 같은색으로 하고 싶으면? fill <- "#4271AE"
line="#1F3552"

gg1<-ggplot(data = house, aes(x=DateBuilt, y=Price.per.Size ,group=DateBuilt ,fill=DateBuilt,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "준공일자") +
   scale_y_continuous(name = "평당 가격") 
gg1


#2. 거래 연도 / 평당가격

gg2<-ggplot(data = house, aes(x=as.factor(YrSold), y=Price.per.Size ,group=YrSold ,fill=YrSold,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "거래 연도") +
   scale_y_continuous(name = "평당 가격")
gg2  ##2009년 없는 이유는 지하철 역이 없다고 나온 범주를 삭제했는데 그 범주가 모두 2009년도에 관측 되었기 때문


#3. 월별 / 평당가격

gg3<-ggplot(data = house, aes(x=as.factor(MonthSold), y=Price.per.Size ,group=MonthSold ,fill=MonthSold,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16,colour=line )  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "월") +
   scale_y_continuous(name = "평당 가격")
gg3


#4. 평수 / 평당가격

gg4<-ggplot(data = house, aes(x=as.factor(Size.p.), y=Price.per.Size ,group=Size.p.  ,fill=Size.p. ,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16,colour=line )  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "평수") +
   scale_y_continuous(name = "평당 가격")
gg4


#5. (준공 2000년 이후) 평수 / 평당가격

house[which(house,(subset(house,YrSold>=2000)),]
?which
house22<-subset(house,YearBuilt>=2000)
str(house22)
table(house22$YearBuilt)
table(house$Size.p.)

gg5<-ggplot(data = house22, aes(x=as.factor(Size.p.), y=Price.per.Size ,group=Size.p.  ,fill=Size.p.,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16,colour=line )  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "(준공 2000년 이후)평수") +
   scale_y_continuous(name = "평당 가격")
gg5



#6. 층 / 평당가격

gg6<-ggplot(data = house, aes(x=as.factor(Floor), y=Price.per.Size ,group=Floor  ,fill=Floor,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16,colour=line )  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "층") +
   scale_y_continuous(name = "평당 가격")
gg6


#7. 매니저 수 / 평당가격

gg7<-ggplot(data = house, aes(x=as.factor(N_manager), y=Price.per.Size ,group=N_manager  ,fill=N_manager,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "매니저 수") +
   scale_y_continuous(name = "평당 가격")
gg7


#8. 아파트 내 동 수 / 평당가격

gg8<-ggplot(data = house, aes(x=as.factor( N_APT ), y=Price.per.Size ,group= N_APT   ,fill= N_APT ,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "아파트 내 동 수") +
   scale_y_continuous(name = "평당 가격")
gg8



#9. 아파트 내 총 엘레베이터 수 / 평당가격

gg9<-ggplot(data = house, aes(x=as.factor(N_manager), y=Price.per.Size ,group=N_manager  ,fill=N_manager,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "아파트 내 총 엘레베이터 수") +
   scale_y_continuous(name = "평당 가격")
gg9



#10. 아파트 내 동 하나당 엘레베이터 수 / 평당가격

gg10<-ggplot(data = house, aes(x=as.factor(Prob_elevators ), y=Price.per.Size ,group=Prob_elevators  ,fill=Prob_elevators ,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "아파트 내 동 하나당 엘레베이터 수") +
   scale_y_continuous(name = "평당 가격")
gg10

##이 변수는 따로  Prob_elevators = (N_elevators + 0.2) / N_APT 이 내용을 적어줘야함



#11. 지하철 역 별 시기에 따른 / 평당가격

gg11<-ggplot(data = house, aes(x=as.factor(YrSold), y=Price.per.Size ,group=YrSold  ,fill=YrSold ,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "거래 연도") +
   scale_y_continuous(name = "평당 가격")+
   facet_wrap(~ SubwayStation)
gg11



#12. 난방 유형 / 평당가격

gg12<-ggplot(data = house, aes(x=as.factor(HeatingType), y=Price.per.Size ,group=HeatingType  ,fill=HeatingType,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "난방유형") +
   scale_y_continuous(name = "평당 가격")
gg12



#13. 복도 유형 / 평당가격

gg13<-ggplot(data = house, aes(x=as.factor(HallwayType), y=Price.per.Size ,group=HallwayType  ,fill=HallwayType,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "복도 유형") +
   scale_y_continuous(name = "평당 가격")
gg13



#14. 아파트 관리 유형 / 평당가격

gg14<-ggplot(data = house, aes(x=as.factor(AptManageType), y=Price.per.Size ,group=AptManageType  ,fill=AptManageType,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "아파트 관리 유형") +
   scale_y_continuous(name = "평당 가격")
gg14



#15. 버스정류장까지 걸리는 시간 / 평당가격

gg15<-ggplot(data = house, aes(x=as.factor(TimeToBusStop), y=Price.per.Size ,group=TimeToBusStop  ,fill=TimeToBusStop,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "버스정류장까지 걸리는 시간") +
   scale_y_continuous(name = "평당 가격")
gg15
##전처리 과정에서 5~10 삭제된듯..?


#16. 지하철역까지 걸리는 시간/ 평당가격

gg16<-ggplot(data = house, aes(x=as.factor(TimeToSubway), y=Price.per.Size ,group=TimeToSubway  ,fill=TimeToSubway,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "지하철역까지 걸리는 시간") +
   scale_y_continuous(name = "평당 가격")
gg16



#18. 지하철 15~20 연도별 / 평당가격

house33<-subset(house,TimeToSubway=="15min~20min")

levels(house$TimeToSubway)
gg18<-ggplot(data = house33, aes(x=as.factor(YearBuilt), y=Price.per.Size ,group=YearBuilt  ,fill=YearBuilt ,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "연도") +
   scale_y_continuous(name = "평당 가격")
gg18



#19. 주변 관공서 수 / 평당가격

gg19<-ggplot(data = house, aes(x=as.factor(N_FacilitiesNearBy.PublicOffice.), y=Price.per.Size ,group=N_FacilitiesNearBy.PublicOffice.  ,fill=N_FacilitiesNearBy.PublicOffice.,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "주변 관공서 수") +
   scale_y_continuous(name = "평당 가격")
gg19



#20. 주변 병원 수 / 평당가격

gg20<-ggplot(data = house, aes(x=as.factor(N_FacilitiesNearBy.Hospital.), y=Price.per.Size ,group=N_FacilitiesNearBy.Hospital.,fill=N_FacilitiesNearBy.Hospital.,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "주변 병원 수") +
   scale_y_continuous(name = "평당 가격")
gg20



#21. 주변 시설 수 / 평당가격

gg21<-ggplot(data = house, aes(x=as.factor(N_FacilitiesNearBy.Total.), y=Price.per.Size ,N_FacilitiesNearBy.Total. ,fill=N_FacilitiesNearBy.Total.,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "주변 시설 수") +
   scale_y_continuous(name = "평당 가격")
gg21




#22. 주변 초등학교 수 / 평당가격

gg22<-ggplot(data = house, aes(x=as.factor( N_SchoolNearBy.Elementary.), y=Price.per.Size ,group= N_SchoolNearBy.Elementary. ,fill= N_SchoolNearBy.Elementary.,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "주변 초등학교 수") +
   scale_y_continuous(name = "평당 가격")
gg22



#23. 아파트 내 시설 수 / 평당가격

gg23<-ggplot(data = house, aes(x=as.factor(N_FacilitiesInApt), y=Price.per.Size ,group=N_FacilitiesInApt  ,fill=N_FacilitiesInApt,alpha = 0.7)) + 
   geom_boxplot(width=0.8, outlier.size=2, outlier.shape=16 ,colour=line)  +  ## 이게 박스플랏 그리는 거
   theme_bw() +  ##바탕 하얗게 하는 거
   scale_x_discrete(name = "아파트 내 시설 수") +
   scale_y_continuous(name = "평당 가격")
gg23



