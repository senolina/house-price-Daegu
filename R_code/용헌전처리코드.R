#install.packages("ggmap")
#install.packages("clustMixType")
library(ggmap)
library(ggplot2)
library(clustMixType)

## clean code data analysis
setwd("C:/Users/HOME/Desktop/??κ΅¬μ§κ°?")
house <- read.csv("Daegu_Real_Estate_data.csv")

# κ²°μΈ‘μΉ? ??Έ
sum(!complete.cases(house))  # κ²°μΈ‘μΉ? 0κ°?

# target λ³? ? κ·μ± λ§μ‘± ?¬λΆ ??Έ
qqnorm(house$SalePrice)  # ? κ·μ± λ§μ‘±  ##???΄?»κ²? ??κ±°μ¬
qqline(house$SalePrice,col="red")  ###****??Έ


# ?°?΄?° ?¨μ§λ³? λΆλ₯? λ³? group ??±
part <- aggregate((house$N_Parkinglot.Ground. + house$N_Parkinglot.Basement.), list(house$N_elevators, house$YearBuilt, house$N_manager), FUN = unique)  ##?Ό?¨ ?΄κ±? ? ?? ?΄?? λͺ¨λ₯΄κ² μ ??
# κ·ΈλΌ part ? κ°μ? ?? λ² μ΄?° ? , κ°μ? μ§?΄μ§? ??, κ°μ? κ΄λ¦¬μ ? λ³? μ§?μ£Όμ°¨?₯κ³? μ§?μ£Όμ°¨? ?? ?© ?Έκ²?=> κ°μ? ?¨μ§λ‘? (26κ°?) ??¨??€? κ²??


aggregate(house$Size.sqf., list(house$N_elevators, house$YearBuilt, house$N_manager), FUN = unique)
#κ°μ? ?? λ² μ΄?° ? , κ°μ? μ§?΄μ§? ??, κ°μ? κ΄λ¦¬μ ? λ³? μ§? ?¬κΈ? (? ??¬)==> 26κ°? ?Έκ±? ?€? λ³΄λ κ²?

house$group <- NA

for(i in 1:nrow(house)){
  for(j in 1:nrow(part)){
    if((house$N_elevators[i] == part[j, 1]) & (house$YearBuilt[i] == part[j, 2]) & (house$N_manager[i] == part[j, 3])){
      house$group[i] <- j
    }
  }
}
##partλ³΄λ©΄ 26κ°μ ?¨μ§?Ό? κ²μ ? ? ?? κ·? κΈ°μ??΄ κ°μ? ?? λ² μ΄?° ? , κ°μ? μ§?΄μ§? ??, κ°μ? κ΄λ¦¬μ ? / κ·Έλ? ?΄ ?Έ κ°κ? λͺ¨λ κ°μΌλ©? κ°μ? ?¨μ§λ‘? μ§? ?΄ μ£Όλ κ²μ

head(house$group)  ##?¨μ§λ₯? ? ?κ±°μ (??Έ?? €κ³?)



# λ©΄μ  ?λ°©νΌ?Έλ₯? ??Όλ‘? λ³?? ?λ‘μ΄ λ³? Size.p. ??±
house$Size.p. <- round(house$Size.sqf. / 35.583)


# TimeTo λ³? level ?? ??  λ°? no_bus_stop level ??  0-5min -> 0~5min?Όλ‘? ?? 
levels(house$TimeToSubway) <- levels(house$TimeToSubway)[c(1, 4, 2, 3, 5)]
levels(house$TimeToSubway)[5] <- "no_subway_station_nearby"
levels(house$TimeToSubway)[1] <- "0~5min"
levels(house$TimeToBusStop) <- levels(house$TimeToBusStop)[c(1, 3, 2)]

# ??Ή κ°κ²? λ³? Price.per.Size ??±
house$Price.per.Size <- house$SalePrice / house$Size.p.

# μ΄? μ£Όμ°¨?₯ ?λ‘? ??? ?¨μ§, μ€κ° ?¨μ§, ?° ?¨μ§λ‘? λ¬Άμ  
house$complex <- as.numeric(cut((house$N_Parkinglot.Basement. + house$N_Parkinglot.Ground.),
                                breaks = c(0, 500, 1000, 1500), include.lowest = T))  ##κΈ°μ?..?

# perfect ?Όκ³ λ λ§ν  ? ?μ§λ§?, ??? ?¨μ§? ??? ??, ?° ?¨μ§? ?° ??κ° μ£Όλ‘ ?¬μ§ν΄?? κ²μ ??Έ
aggregate(house$Size.p. ~ house$complex, FUN = unique)

# ?? ?κ²? λ³΄μ΄? κ²μ?
# ???¨μ§κ° λ°μ?Ή, κ²½λ?λ³μ, λͺλ, no_subway(3?Έ?  κ±΄λ€λ°μ?­?Όλ‘? μΆμ ) κ°?΄?° μ§??? ?¬μ§ν΄?? κ²μ ??Έ
# λ°κ³ κ°μ? μ€ν ???Έλ§? ?μΉ?
# μΉ μ±??₯, ??κ΅¬μ­ λΆκ·Όμ? ???Έ?¨μ§ κ±°μ ??     

##?¬κΈ? μ§?λ₯? κ°μ΄ λ΄μΌ?  κ²? κ°μ


100 * prop.table(table(house$complex, house$SubwayStation), margin = 1)  # μ‘°κ±΄λΆ λΉμ¨(complex λ³? λΉμ¨) (?λ³? ?©?΄ 100)
100 * prop.table(table(house$complex, house$SubwayStation))  # ? μ²? λΉμ¨ (? μ²? ?©?΄ 100)
#κ·Όλ° ?΄κ±? κ΅³μ΄ λΉμ¨λ‘? λ΄μΌ? κΉ?..? 


# κ±΄μΆ?Ό??? κ±°λ?Ό? Dateλ‘? λ°κΏμ€?
# κ±΄μΆ?Ό? : YearBuilt - 01 - 01
# κ±°λ?Ό? : YrSold - MonthSold - 01
house$DateSold <- as.Date(with(house, 
                               paste(YrSold, ifelse(MonthSold < 10, paste("0", MonthSold, sep = ""), MonthSold), "01", sep = "-")))

house$DateBuilt <- as.Date(paste(house$YearBuilt, "-01-01", sep = ""))

# Facility?? School ?±?΄ ? κΈ°μ??Έμ§ ?¨μ§ κΈ°μ??Έμ§ ???΅? ?Όλ‘? ??Έ
aggregate(house$TimeToSubway ~ house$group, FUN = unique)
aggregate(house$N_FacilitiesNearBy.Park. ~ house$group, FUN = unique)
aggregate(house$HallwayType ~ house$group, FUN = unique)
aggregate(house$HeatingType ~ house$group, FUN = unique)
aggregate(house$TimeToSubway ~ house$group, FUN = unique)
aggregate(house$N_SchoolNearBy.Elementary. ~ house$group, FUN = unique)
# ?¨μ§ κΈ°μ??Έ κ²μΌλ‘? ??Έ???










==>1122???¬κΈ°κΉμ§


#### ?κ³μ΄ ? κ±?

# κ±°λ κ°κ²©μΌλ‘? 1μ°? ?Ό? ? residual plot?΄ ?½κ°μ U? λͺ¨μ? ??΄? 2μ°? ?Ό??¨
# 2μ°? ?Ό? ??? κ³λ¨?  resudial plot? ?? κ΅¬κ°?΄ ??΄ ??Ή κ°κ²©μΌλ‘? ?Ό? ??
par(mfrow = c(2, 2))
plot(lm(SalePrice ~ I(as.numeric(DateSold)) + I(as.numeric(DateSold)^2), data = house))

# κ±°λ?Ό λ³? κΈ°μ? ?κ· fitting ? residual ??Έ ? U? κ³‘μ  ??λ₯? λ³΄μ
# ?°?Ό? ? κ³±ν­ μΆκ??΄μ€?
# DateSold λ³?? date type?΄κΈ? ?λ¬Έμ numeric?Όλ‘? λ°κΏ? λ¬Έμ  ?μ§ ??
# ?° μ°¨μ΄? ?μ§λ§? 2μ°¨ν­ λͺ¨λΈ?΄ κΌ¬λ¦¬λΆλΆμ? μ’ ? ? κ·μ±? λ³΄μ΄? κ²μΌλ‘? λ³΄μ

# 1μ°? fitting
par(mfrow = c(2, 2))
model1_price.date <- lm(Price.per.Size ~ I(as.numeric(DateSold)), data = house)
plot(model1_price.date)

# 2μ°? fitting
par(mfrow = c(2, 2))
model2_price.date <- lm(Price.per.Size ~ I(as.numeric(DateSold)) + I(as.numeric(DateSold)^2), data = house)
summary(model2_price.date)
anova(model2_price.date)

plot(model2_price.date)
par(mfrow = c(1, 1))

# ?κ³μ΄ ? κ±°ν κ°μΌλ‘? SalePrice_Time λ³? ??±
house$SalePrice_Time <- summary(model2_price.date)$residual
plot(house$SalePrice_Time ~ house$Floor)
lines(lowess(x = house$Floor, y = house$SalePrice_Time), col = "red", lwd = 2)

names(house)

# μΆκ?? ?Όλ‘? ??Ή κ°κ²©κ³Ό κ±΄μΆ?Ό?? plot?? 2000??? μ€λ° ?΄? ?°?­?Όλ‘? ??Ή?? κ²½ν₯? ?? κ²μ?
# μ΅κ·Ό? μ§?΄μ‘μ ?λ‘? κ°κ²©μ΄ ?? κ²μ΄?€?Ό? κ°?€1κ³?
# 2010?? ? ?λ‘? ??¬ λΆ??° ??₯?΄ ? μ²΄μ ?Όλ‘? κΈμ?Ή?? κ²½ν₯? λ³΄μ?κΈ? ?λ¬Έμ
# κ·? λΆκ·Όμ μ§?΄μ§? ???Έ? ?Ή?°? κ°κ²©μ΄ ?κ²? ??±???€? κ°?€2κ° ??
# ?΄λ₯? ??Έ?κΈ? ??΄? ?κ°μ ?λ¦μ ? κ±°ν ? κ±΄μΆ?Ό??? plot? κ·Έλ €λ³? κ²°κ³Ό
# κΈ°μ‘΄? κ·Έλ???? ?€λ₯΄κ² 1980???λΆ?° κ³ λ₯Έ ?±?₯? λ³΄μ΄? κ²μ ??
# 2010? ? ?? λΆ??° ??₯ κΈλ± ??λ₯? ? ? κ±°ν ? κ±΄μΆ?Ό?λ₯? λ³?λ‘? ?¬?©???° λ¬Έμ κ° ??€? κ²μ λ³΄μ¬μ€?
# ? λͺ¨λ₯΄κ² μ

boxplot(house$Price.per.Size ~ house$DateBuilt)
lines(lowess(x = house$DateBuilt, y = house$Price.per.Size), col = "red", lwd = 2)

boxplot(house$SalePrice_Time ~ house$DateBuilt)
lines(lowess(x = house$DateBuilt, y = house$SalePrice_Time), col = "red", lwd = 2)

# κ·Έλ? κ±°λ?Ό??? κ±΄μΆ?Ό?λ₯? λͺ¨λ λ³?? ?¬?¨??¬ residual plot? κ·Έλ¦° κ²°κ³Ό ?°μΈ‘μΌλ‘? κ°? ?λ‘? λΆμ°?΄ μ»€μ?? λͺ¨μ? λ³΄μ
model1_second <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt), data = house)
summary(model1_second)
par(mfrow = c(2, 2))
plot(model1_second)
par(mfrow = c(1, 1))

# ?°?Ό? sqrt(target)λ³?λ‘? residual plot ??Έ κ²°κ³Ό increasing trend ? κ±°λ κ²μΌλ‘? ??Έ
model2_second <- lm(sqrt(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt), data = house)
summary(model2_second)
par(mfrow = c(2, 2))
plot(model2_second)
par(mfrow = c(1, 1))

# ??? ?Όλ‘? μ§?μ² κΉμ§ κ±°λ¦¬? μ§κ°?΄ ??₯? λ§μ΄ λ°λ?€κ³? ?? €? Έ?κΈ? ?λ¬Έμ plot??Έ κ²°κ³Ό ?±? ?λ³΄μ
plot(house$SalePrice_Time ~ house$TimeToSubway)
table(house$TimeToSubway, house$N_manager)

# μ§?μ² κΉμ§ κ±°λ¦¬λ₯? κ³ μ ??¨ ???? N_manager(?¨μ§ κ·λͺ¨?Όκ³? ?κ°ν¨) λ³? ?κ°? ? κ±°ν ??? ??Ή κ°κ²? ??Έ
with(subset(house, TimeToSubway == "0~5min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "5min~10min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "10min~15min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "15min~20min"), boxplot(SalePrice_Time ~ N_manager))

# 0~5min ?΄?μΉ? ??Έ N_manager == 7?Έ κ²½μ° ??κΌ¬λ¦¬? ?΄?μΉ? ?€? λ°μ
# ??Έ κ²°κ³Ό group 21? ?΄?Ή?? ?¨μ§λ‘? 10?κ°? μ΄? κ±°λ κ±΄μκ° 4κ±΄λ°? ?? κ²μ ??Έ
# group 21 ?¨μ§? N_APT? 10?΄κ³?, κ½? ?° ?¨μ§λ‘? μΆμ ???° ?΄??¨
# ? κ±°ν μ§? ??? ?Ό? ??
# μΆκ?λ‘? ??Έ κ²°κ³Ό 21λ²? ?¨μ§?? 22λ²? ?¨μ§κ° κ±΄μΆ?Ό?? ?€λ₯΄μ?λ§? ?€λ₯? λ³?κ° λͺ¨λ κ°μ? κ²μΌλ‘? ??Έ
# ?¬κ±΄μΆ?΄ ??κΉ? ?Ά?...
# λ§μ½ ?€λ₯? ?¬κ±΄μΆ?΄ ??€κ³? κ°? ?? ?, ?€ κ±Έλ¬?΄?Ό?¨
subset(house, TimeToSubway == "0~5min" & SalePrice_Time < -1000 & N_manager == 7)
nrow(subset(house, group == 21))
subset(house, group == 22)
unique(subset(house, group == 22)$Size.p.)
subset(house, group == 21)

# 5~10min ?΄?μΉ? ??Έ N_manager == 3?΄ 5, 8λ³΄λ€ ??Ή κ°κ²?, ?κ°? ? κ±? ? ??Ή κ°κ²? λͺ¨λ ??
# ??Έ κ²°κ³Ό 5, 8?Έ ?°?΄?°? κ°κ° 1992?, 1986?? μ§?΄μ§? ???Έλ‘? ??Έ
table(subset(house, TimeToSubway == "5min~10min", select = c(SubwayStation, N_manager)))
table(subset(house, TimeToSubway == "5min~10min", select = DateBuilt))
with(subset(house, TimeToSubway == "5min~10min"), boxplot(Price.per.Size ~ N_manager))

boxplot(house$SalePrice_Time ~ house$group)
boxplot(house$Price.per.Size ~ house$group)
boxplot(house$SalePrice ~ house$group)

###### ?μ§? ??Έμ€? ######
subset(house, group == 21, select = c(Size.p., DateBuilt, DateSold, N_Parkinglot.Ground., N_Parkinglot.Basement.))

boxplot(aggregate(house$SalePrice_Time ~ house$group, FUN = var)[, 2] / 
          aggregate(house$Price.per.Size ~ house$group, FUN = var)[, 2] ~ c(1:26), ylim = c(0, 1))

var_group <- (aggregate(house$SalePrice_Time ~ house$group, FUN = var)[, 2] / 
                aggregate(house$Price.per.Size ~ house$group, FUN = var)[, 2])

which(var_group > 0.8 & var_group < 1)

subset(house, group == 16, select = c(DateBuilt, DateSold, N_Parkinglot.Ground., N_Parkinglot.Basement.))


# 21λ²? ?¨μ§κ° ?΄?μΉλ‘ ??¬?¨
# 21λ²? ?¨μ§? κ²½μ° 1980? κ±΄μΆ??μ§λ§?, μ΄? κ±°λ κ±? ?κ° 4κ±΄μ
# 21λ²? ?¨μ§? μ£Όμ°¨?₯? ???€?κ° μ§?? ?μΉνκ³? ??. ?Ή?΄?¨
# ? κ±? ?¬λΆ κ²°μ 

################################################################################
################################################################################


# google map

# ?­λͺ? / ?? / κ²½λ
# λ°κ³ κ°μ­ / 35.862462 / 128.573531
# λ°μ?Ή?­ / 35.865468 / 128.593369
# μΉ μ±??₯?­ / 35.876102 / 128.605083
# ??κ΅¬μ­ / 35.876293 / 128.597107
# κ²½λ?λ³μ?­ / 35.863127 / 128.603153
# λͺλ?­ / 35.857225 / 128.590794
# μ²??Ό?Έ??­(? ?¨) / 35.865122 / 128.583093

# λ°±ν? λͺ? / ?? / κ²½λ
# ???λ°±ν?  ??κ΅¬μ  / 35.865866 / 128.585675
# ??κ΅¬λ°±??  / 35.869143 / 128.595984
# ? ?Έκ³λ°±??  ??κ΅¬μ  / 35.877372 / 128.628534
# λ‘??°λ°±ν?  ??κ΅¬μ  / 35.875801 / 128.591183
# ??κ΅¬λ°±??  ??Ό? / 35.854915 / 128.608861
# ??λ°±ν?  ?Ό??  / 35.862347 / 128.597566
# ? ?Έκ³λ°±?? ?? ???κ΅¬μ­ λΆκ·Όμ΄?Ό μ§??? ?΄?

# ???κ΅λͺ / ?? / κ²½λ
# κ³λͺ?? ??? ??°μΊ νΌ?€ / 35.867132 / 128.582309
# ??κ΅¬κ??¨λ¦??? ? ?€?°?ΈμΊ νΌ?€ / 35.8620364 / 128.5874015
# κ²½λΆ?? ??? / 35.868840 / 128.603100
# κ²½λΆ?? μΉλ? / 35.864434 / 128.608359
# ??κ΅¬λ? ??λͺλμΊ νΌ?€ / 35.838730 / 128.575100
# κ³λͺ?? ??λͺμΊ ?Ό?€ / 35.856548 / 128.585985
# ??κ΅¬κ΅?‘?? / 35.855367 / 128.590767
# ??¨???κ΅? ??κ΅¬μΊ ?Ό?€ / 35.854486 / 128.583121


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



# TimeToSubwayκ° 15min~20min?Έ κ·Έλ£Ή? no_subway_nearby λ°κ²¬
# SubwayStation?΄ no_subway_nearby?Έ κ·Έλ£Ή? TimeToSubwayκ° 15min~20min?΄ 166κ±°λ, no_subway_station_nearbyκ° 238κ±°λ
# ?΄?μΉ? μ²λ¦¬ λ°©λ² ?Ό?
subset(house, TimeToSubway == "15min~20min", SubwayStation)
table(subset(house, SubwayStation == "no_subway_nearby", c(TimeToSubway)))

