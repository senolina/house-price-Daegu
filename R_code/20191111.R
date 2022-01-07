##### ~2019-11-12 #####

###경로설정

setwd("C:/Users/HOME/Desktop/대구집값")


###data 불러오기 (이건 항상 train일자 로 지정하겠음 ex)11월 12일이므로 train12)

train12<-read.csv("Daegu_Real_Estate_data.csv")


###data 구조 확인

dim(train12) #변수 30개/ 관측치 5891개

str(train12) #타깃변수:SalePrice (연속형) // 날짜 변수 3개 // 양적변수- 연속형 1? , 이산형 19 // 질적변수 - 범주형 6 => 총 30개 확인 


###결측치 확인

colSums(is.na(train12)) #결측치 없음


###타깃변수 객체 지정

y<-train12$SalePrice


###연속형 변수 선택

plot(train12$Size.sqf. , y, xlim=c(100,2500),ylim=c(32000,600000)) #평수를 연속형으로 보기 어렵나...?=> 한 x당 해당하는 y가 여러개이기 때문에// 근데 나름 선형적(x증가 할 수록 y 증가)


###이산형(숫자형)
x_num<-train12[,c(6,10,11,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30)]

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
x_fac<-train12[,c(7,8,9,12,13,17)]

head(x_fac)
str(x_fac)


with(train12,boxplot(SalePrice~HallwayType))
with(train12,boxplot(SalePrice~HeatingType))
with(train12,boxplot(SalePrice~AptManageType))
with(train12,boxplot(SalePrice~TimeToBusStop))
with(train12,boxplot(SalePrice~TimeToSubway))
with(train12,boxplot(SalePrice~SubwayStation))


with(train12,table(HallwayType))
with(train12,table(HeatingType))
with(train12,table(AptManageType))
with(train12,table(TimeToBusStop))
with(train12,table(TimeToSubway))
with(train12,table(SubwayStation))





