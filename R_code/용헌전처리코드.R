#install.packages("ggmap")
#install.packages("clustMixType")
library(ggmap)
library(ggplot2)
library(clustMixType)

## clean code data analysis
setwd("C:/Users/HOME/Desktop/??€êµ¬ì§‘ê°?")
house <- read.csv("Daegu_Real_Estate_data.csv")

# ê²°ì¸¡ì¹? ?™•?¸
sum(!complete.cases(house))  # ê²°ì¸¡ì¹? 0ê°?

# target ë³€?ˆ˜ ? •ê·œì„± ë§Œì¡± ?—¬ë¶€ ?™•?¸
qqnorm(house$SalePrice)  # ? •ê·œì„± ë§Œì¡±  ##???–´?–»ê²? ?•„?Š”ê±°ì—¬
qqline(house$SalePrice,col="red")  ###****?™•?¸


# ?°?´?„° ?‹¨ì§€ë³? ë¶„ë¥˜?œ ë³€?ˆ˜ group ?ƒ?„±
part <- aggregate((house$N_Parkinglot.Ground. + house$N_Parkinglot.Basement.), list(house$N_elevators, house$YearBuilt, house$N_manager), FUN = unique)  ##?¼?‹¨ ?´ê±? ? •?™•?•œ ?•´?„?„ ëª¨ë¥´ê² ìŒ ?…œ?…œ
# ê·¸ëŸ¼ part ?Š” ê°™ì?€ ?—˜? ˆë² ì´?„° ?ˆ˜ , ê°™ì?€ ì§€?–´ì§? ?…„?„, ê°™ì?€ ê´€ë¦¬ì ?ˆ˜ ë³? ì§€?•˜ì£¼ì°¨?¥ê³? ì§€?ƒì£¼ì°¨?ƒ ?ˆ˜?˜ ?•© ?¸ê²?=> ê°™ì?€ ?‹¨ì§€ë¡? (26ê°?) ?Œ?‹¨?•œ?‹¤?Š” ê²??


aggregate(house$Size.sqf., list(house$N_elevators, house$YearBuilt, house$N_manager), FUN = unique)
#ê°™ì?€ ?—˜? ˆë² ì´?„° ?ˆ˜ , ê°™ì?€ ì§€?–´ì§? ?…„?„, ê°™ì?€ ê´€ë¦¬ì ?ˆ˜ ë³? ì§? ?¬ê¸? (?œ ?‹ˆ?¬)==> 26ê°? ?¸ê±? ?‹¤?‹œ ë³´ëŠ” ê²?

house$group <- NA

for(i in 1:nrow(house)){
  for(j in 1:nrow(part)){
    if((house$N_elevators[i] == part[j, 1]) & (house$YearBuilt[i] == part[j, 2]) & (house$N_manager[i] == part[j, 3])){
      house$group[i] <- j
    }
  }
}
##partë³´ë©´ 26ê°œì˜ ?‹¨ì§€?¼?Š” ê²ƒì„ ?•Œ ?ˆ˜ ?ˆ?Œ ê·? ê¸°ì?€?´ ê°™ì?€ ?—˜? ˆë² ì´?„° ?ˆ˜ , ê°™ì?€ ì§€?–´ì§? ?…„?„, ê°™ì?€ ê´€ë¦¬ì ?ˆ˜ / ê·¸ë˜?„œ ?´ ?„¸ ê°œê?€ ëª¨ë‘ ê°™ìœ¼ë©? ê°™ì?€ ?‹¨ì§€ë¡? ì§€? •?•´ ì£¼ëŠ” ê²ƒì„

head(house$group)  ##?‹¨ì§€ë¥? ? •?•œê±°ì„ (?™•?¸?•˜? ¤ê³?)



# ë©´ì  ?‰ë°©í”¼?Š¸ë¥? ?‰?œ¼ë¡? ë³€?™˜?•œ ?ƒˆë¡œìš´ ë³€?ˆ˜ Size.p. ?ƒ?„±
house$Size.p. <- round(house$Size.sqf. / 35.583)


# TimeTo ë³€?ˆ˜ level ?ˆœ?„œ ?ˆ˜? • ë°? no_bus_stop level ?ˆ˜? • 0-5min -> 0~5min?œ¼ë¡? ?ˆ˜? •
levels(house$TimeToSubway) <- levels(house$TimeToSubway)[c(1, 4, 2, 3, 5)]
levels(house$TimeToSubway)[5] <- "no_subway_station_nearby"
levels(house$TimeToSubway)[1] <- "0~5min"
levels(house$TimeToBusStop) <- levels(house$TimeToBusStop)[c(1, 3, 2)]

# ?‰?‹¹ ê°€ê²? ë³€?ˆ˜ Price.per.Size ?ƒ?„±
house$Price.per.Size <- house$SalePrice / house$Size.p.

# ì´? ì£¼ì°¨?¥ ?ˆ˜ë¡? ?‘??€ ?‹¨ì§€, ì¤‘ê°„ ?‹¨ì§€, ?° ?‹¨ì§€ë¡? ë¬¶ìŒ  
house$complex <- as.numeric(cut((house$N_Parkinglot.Basement. + house$N_Parkinglot.Ground.),
                                breaks = c(0, 500, 1000, 1500), include.lowest = T))  ##ê¸°ì?€..?

# perfect ?¼ê³ ëŠ” ë§í•  ?ˆ˜ ?—†ì§€ë§?, ?‘??€ ?‹¨ì§€?— ?‘??€ ?‰?ˆ˜, ?° ?‹¨ì§€?— ?° ?‰?ˆ˜ê°€ ì£¼ë¡œ ?¬ì§„í•´?ˆ?Š” ê²ƒì„ ?™•?¸
aggregate(house$Size.p. ~ house$complex, FUN = unique)

# ?ˆˆ?— ?„ê²? ë³´ì´?Š” ê²ƒì?€
# ??€?‹¨ì§€ê°€ ë°˜ì›”?‹¹, ê²½ë?€ë³‘ì›, ëª…ë•, no_subway(3?˜¸?„  ê±´ë“¤ë°”ìœ„?—­?œ¼ë¡? ì¶”ì •) ê°€?š´?° ì§€??€?— ?¬ì§„í•´?ˆ?Š” ê²ƒì„ ?™•?¸
# ë°˜ê³ ê°œì—?Š” ì¤‘í˜• ?•„?ŒŒ?Š¸ë§? ?œ„ì¹?
# ì¹ ì„±?‹œ?¥, ??€êµ¬ì—­ ë¶€ê·¼ì—?Š” ?•„?ŒŒ?Š¸?‹¨ì§€ ê±°ì˜ ?—†?Œ     

##?—¬ê¸? ì§€?„ë¥? ê°™ì´ ë´ì•¼?•  ê²? ê°™ìŒ


100 * prop.table(table(house$complex, house$SubwayStation), margin = 1)  # ì¡°ê±´ë¶€ ë¹„ìœ¨(complex ë³? ë¹„ìœ¨) (?–‰ë³? ?•©?´ 100)
100 * prop.table(table(house$complex, house$SubwayStation))  # ? „ì²? ë¹„ìœ¨ (? „ì²? ?•©?´ 100)
#ê·¼ë° ?´ê±? êµ³ì´ ë¹„ìœ¨ë¡? ë´ì•¼?• ê¹?..? 


# ê±´ì¶•?¼???€ ê±°ë˜?¼? Dateë¡? ë°”ê¿”ì¤?
# ê±´ì¶•?¼? : YearBuilt - 01 - 01
# ê±°ë˜?¼? : YrSold - MonthSold - 01
house$DateSold <- as.Date(with(house, 
                               paste(YrSold, ifelse(MonthSold < 10, paste("0", MonthSold, sep = ""), MonthSold), "01", sep = "-")))

house$DateBuilt <- as.Date(paste(house$YearBuilt, "-01-01", sep = ""))

# Facility??€ School ?“±?´ ?™ ê¸°ì?€?¸ì§€ ?‹¨ì§€ ê¸°ì?€?¸ì§€ ??€?µ? ?œ¼ë¡? ?™•?¸
aggregate(house$TimeToSubway ~ house$group, FUN = unique)
aggregate(house$N_FacilitiesNearBy.Park. ~ house$group, FUN = unique)
aggregate(house$HallwayType ~ house$group, FUN = unique)
aggregate(house$HeatingType ~ house$group, FUN = unique)
aggregate(house$TimeToSubway ~ house$group, FUN = unique)
aggregate(house$N_SchoolNearBy.Elementary. ~ house$group, FUN = unique)
# ?‹¨ì§€ ê¸°ì?€?¸ ê²ƒìœ¼ë¡? ?™•?¸?˜?—ˆ?Œ










==>1122?‚˜?˜•?—¬ê¸°ê¹Œì§€


#### ?‹œê³„ì—´ ? œê±?

# ê±°ë˜ ê°€ê²©ìœ¼ë¡? 1ì°? ?”¼?Œ… ?‹œ residual plot?´ ?•½ê°„ì˜ U? ëª¨ì–‘?„ ?„?–´?„œ 2ì°? ?”¼?Œ…?•¨
# 2ì°? ?”¼?Œ… ?›„?—?„ ê³„ë‹¨?‹  resudial plot?„ ?„?Š” êµ¬ê°„?´ ?ˆ?–´ ?‰?‹¹ ê°€ê²©ìœ¼ë¡? ?”¼?Œ… ?‹œ?„
par(mfrow = c(2, 2))
plot(lm(SalePrice ~ I(as.numeric(DateSold)) + I(as.numeric(DateSold)^2), data = house))

# ê±°ë˜?¼ ë³€?ˆ˜ ê¸°ì?€ ?šŒê·€ fitting ?›„ residual ?™•?¸ ?‹œ U? ê³¡ì„  ?˜•?ƒœë¥? ë³´ì„
# ?”°?¼?„œ ? œê³±í•­ ì¶”ê?€?•´ì¤?
# DateSold ë³€?ˆ˜?Š” date type?´ê¸? ?•Œë¬¸ì— numeric?œ¼ë¡? ë°”ê¿”?„ ë¬¸ì œ ?˜ì§€ ?•Š?Œ
# ?° ì°¨ì´?Š” ?—†ì§€ë§? 2ì°¨í•­ ëª¨ë¸?´ ê¼¬ë¦¬ë¶€ë¶„ì—?„œ ì¢€ ?” ? •ê·œì„±?„ ë³´ì´?Š” ê²ƒìœ¼ë¡? ë³´ì„

# 1ì°? fitting
par(mfrow = c(2, 2))
model1_price.date <- lm(Price.per.Size ~ I(as.numeric(DateSold)), data = house)
plot(model1_price.date)

# 2ì°? fitting
par(mfrow = c(2, 2))
model2_price.date <- lm(Price.per.Size ~ I(as.numeric(DateSold)) + I(as.numeric(DateSold)^2), data = house)
summary(model2_price.date)
anova(model2_price.date)

plot(model2_price.date)
par(mfrow = c(1, 1))

# ?‹œê³„ì—´ ? œê±°í•œ ê°’ìœ¼ë¡? SalePrice_Time ë³€?ˆ˜ ?ƒ?„±
house$SalePrice_Time <- summary(model2_price.date)$residual
plot(house$SalePrice_Time ~ house$Floor)
lines(lowess(x = house$Floor, y = house$SalePrice_Time), col = "red", lwd = 2)

names(house)

# ì¶”ê?€? ?œ¼ë¡? ?‰?‹¹ ê°€ê²©ê³¼ ê±´ì¶•?¼??˜ plot?—?„œ 2000?…„??€ ì¤‘ë°˜ ?´?›„ ?°?­?œ¼ë¡? ?ƒ?Š¹?•˜?Š” ê²½í–¥?„ ?„?Š” ê²ƒì?€
# ìµœê·¼?— ì§€?–´ì¡Œì„ ?ˆ˜ë¡? ê°€ê²©ì´ ?†’?„ ê²ƒì´?‹¤?¼?Š” ê°€?„¤1ê³?
# 2010?…„?„ ? „?›„ë¡? ?•˜?—¬ ë¶€?™?‚° ?‹œ?¥?´ ? „ì²´ì ?œ¼ë¡? ê¸‰ìƒ?Š¹?•˜?Š” ê²½í–¥?„ ë³´ì?€ê¸? ?•Œë¬¸ì—
# ê·? ë¶€ê·¼ì— ì§€?–´ì§? ?•„?ŒŒ?Š¸?Š” ?‹¹?—°?ˆ ê°€ê²©ì´ ?†’ê²? ?˜•?„±?˜?ˆ?‹¤?Š” ê°€?„¤2ê°€ ?ˆ?Œ
# ?´ë¥? ?™•?¸?•˜ê¸? ?œ„?•´?„œ ?‹œê°„ì˜ ?ë¦„ì„ ? œê±°í•œ ?›„ ê±´ì¶•?¼???€ plot?„ ê·¸ë ¤ë³? ê²°ê³¼
# ê¸°ì¡´?˜ ê·¸ë˜?”„??€?Š” ?‹¤ë¥´ê²Œ 1980?…„??€ë¶€?„° ê³ ë¥¸ ?„±?¥?„ ë³´ì´?Š” ê²ƒì„ ?ŒŒ?•…
# 2010?…„ ? „?›„?˜ ë¶€?™?‚° ?‹œ?¥ ê¸‰ë“± ?š”?†Œë¥? ?˜ ? œê±°í•œ ?›„ ê±´ì¶•?¼?ë¥? ë³€?ˆ˜ë¡? ?‚¬?š©?•˜?Š”?° ë¬¸ì œê°€ ?—†?‹¤?Š” ê²ƒì„ ë³´ì—¬ì¤?
# ?˜ ëª¨ë¥´ê² ìŒ

boxplot(house$Price.per.Size ~ house$DateBuilt)
lines(lowess(x = house$DateBuilt, y = house$Price.per.Size), col = "red", lwd = 2)

boxplot(house$SalePrice_Time ~ house$DateBuilt)
lines(lowess(x = house$DateBuilt, y = house$SalePrice_Time), col = "red", lwd = 2)

# ê·¸ë˜?„œ ê±°ë˜?¼???€ ê±´ì¶•?¼?ë¥? ëª¨ë‘ ë³€?ˆ˜?— ?¬?•¨?•˜?—¬ residual plot?„ ê·¸ë¦° ê²°ê³¼ ?š°ì¸¡ìœ¼ë¡? ê°? ?ˆ˜ë¡? ë¶„ì‚°?´ ì»¤ì?€?Š” ëª¨ì–‘?„ ë³´ì„
model1_second <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt), data = house)
summary(model1_second)
par(mfrow = c(2, 2))
plot(model1_second)
par(mfrow = c(1, 1))

# ?”°?¼?„œ sqrt(target)ë³€?ˆ˜ë¡? residual plot ?™•?¸ ê²°ê³¼ increasing trend ? œê±°ëœ ê²ƒìœ¼ë¡? ?™•?¸
model2_second <- lm(sqrt(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt), data = house)
summary(model2_second)
par(mfrow = c(2, 2))
plot(model2_second)
par(mfrow = c(1, 1))

# ?ƒ?‹? ?œ¼ë¡? ì§€?•˜ì² ê¹Œì§€ ê±°ë¦¬?— ì§‘ê°’?´ ?˜?–¥?„ ë§ì´ ë°›ëŠ”?‹¤ê³? ?•Œ? ¤? ¸?ˆê¸? ?•Œë¬¸ì— plot?™•?¸ ê²°ê³¼ ?”±?ˆ ?•ˆë³´ì„
plot(house$SalePrice_Time ~ house$TimeToSubway)
table(house$TimeToSubway, house$N_manager)

# ì§€?•˜ì² ê¹Œì§€ ê±°ë¦¬ë¥? ê³ ì •?‹œ?‚¨ ?ƒ?ƒœ?—?„œ N_manager(?‹¨ì§€ ê·œëª¨?¼ê³? ?ƒê°í•¨) ë³? ?‹œê°? ? œê±°í•œ ?ƒ?ƒœ?˜ ?‰?‹¹ ê°€ê²? ?™•?¸
with(subset(house, TimeToSubway == "0~5min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "5min~10min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "10min~15min"), boxplot(SalePrice_Time ~ N_manager))
with(subset(house, TimeToSubway == "15min~20min"), boxplot(SalePrice_Time ~ N_manager))

# 0~5min ?´?ƒì¹? ?™•?¸ N_manager == 7?¸ ê²½ìš° ?•„?˜ê¼¬ë¦¬?— ?´?ƒì¹? ?‹¤?ˆ˜ ë°œìƒ
# ?™•?¸ ê²°ê³¼ group 21?— ?•´?‹¹?•˜?Š” ?‹¨ì§€ë¡? 10?…„ê°? ì´? ê±°ë˜ ê±´ìˆ˜ê°€ 4ê±´ë°–?— ?—†?Š” ê²ƒì„ ?™•?¸
# group 21 ?‹¨ì§€?Š” N_APT?„ 10?´ê³?, ê½? ?° ?‹¨ì§€ë¡? ì¶”ì •?˜?Š”?° ?´?ƒ?•¨
# ? œê±°í• ì§€?— ??€?•œ ?…¼?˜ ?•„?š”
# ì¶”ê?€ë¡? ?™•?¸ ê²°ê³¼ 21ë²? ?‹¨ì§€??€ 22ë²? ?‹¨ì§€ê°€ ê±´ì¶•?¼??Š” ?‹¤ë¥´ì?€ë§? ?‹¤ë¥? ë³€?ˆ˜ê°€ ëª¨ë‘ ê°™ì?€ ê²ƒìœ¼ë¡? ?™•?¸
# ?¬ê±´ì¶•?´ ?•„?‹ê¹? ?‹¶?Œ...
# ë§Œì•½ ?‹¤ë¥? ?¬ê±´ì¶•?´ ?ˆ?‹¤ê³? ê°€? •?–ˆ?„ ?•Œ, ?‹¤ ê±¸ëŸ¬?‚´?•¼?•¨
subset(house, TimeToSubway == "0~5min" & SalePrice_Time < -1000 & N_manager == 7)
nrow(subset(house, group == 21))
subset(house, group == 22)
unique(subset(house, group == 22)$Size.p.)
subset(house, group == 21)

# 5~10min ?´?ƒì¹? ?™•?¸ N_manager == 3?´ 5, 8ë³´ë‹¤ ?‰?‹¹ ê°€ê²?, ?‹œê°? ? œê±? ?›„ ?‰?‹¹ ê°€ê²? ëª¨ë‘ ?†’?Œ
# ?™•?¸ ê²°ê³¼ 5, 8?¸ ?°?´?„°?Š” ê°ê° 1992?…„, 1986?…„?— ì§€?–´ì§? ?•„?ŒŒ?Š¸ë¡? ?™•?¸
table(subset(house, TimeToSubway == "5min~10min", select = c(SubwayStation, N_manager)))
table(subset(house, TimeToSubway == "5min~10min", select = DateBuilt))
with(subset(house, TimeToSubway == "5min~10min"), boxplot(Price.per.Size ~ N_manager))

boxplot(house$SalePrice_Time ~ house$group)
boxplot(house$Price.per.Size ~ house$group)
boxplot(house$SalePrice ~ house$group)

###### ?•„ì§? ?™•?¸ì¤? ######
subset(house, group == 21, select = c(Size.p., DateBuilt, DateSold, N_Parkinglot.Ground., N_Parkinglot.Basement.))

boxplot(aggregate(house$SalePrice_Time ~ house$group, FUN = var)[, 2] / 
          aggregate(house$Price.per.Size ~ house$group, FUN = var)[, 2] ~ c(1:26), ylim = c(0, 1))

var_group <- (aggregate(house$SalePrice_Time ~ house$group, FUN = var)[, 2] / 
                aggregate(house$Price.per.Size ~ house$group, FUN = var)[, 2])

which(var_group > 0.8 & var_group < 1)

subset(house, group == 16, select = c(DateBuilt, DateSold, N_Parkinglot.Ground., N_Parkinglot.Basement.))


# 21ë²? ?‹¨ì§€ê°€ ?´?ƒì¹˜ë¡œ ?“œ?Ÿ¬?‚¨
# 21ë²? ?‹¨ì§€?˜ ê²½ìš° 1980?…„ ê±´ì¶•?˜?—ˆì§€ë§?, ì´? ê±°ë˜ ê±? ?ˆ˜ê°€ 4ê±´ì„
# 21ë²? ?‹¨ì§€?Š” ì£¼ì°¨?¥?˜ ??€?‹¤?ˆ˜ê°€ ì§€?•˜?— ?œ„ì¹˜í•˜ê³? ?ˆ?Œ. ?Š¹?´?•¨
# ? œê±? ?—¬ë¶€ ê²°ì •

################################################################################
################################################################################


# google map

# ?—­ëª? / ?œ„?„ / ê²½ë„
# ë°˜ê³ ê°œì—­ / 35.862462 / 128.573531
# ë°˜ì›”?‹¹?—­ / 35.865468 / 128.593369
# ì¹ ì„±?‹œ?¥?—­ / 35.876102 / 128.605083
# ??€êµ¬ì—­ / 35.876293 / 128.597107
# ê²½ë?€ë³‘ì›?—­ / 35.863127 / 128.603153
# ëª…ë•?—­ / 35.857225 / 128.590794
# ì²??¼?–¸?•?—­(?‹ ?‚¨) / 35.865122 / 128.583093

# ë°±í™”? ëª? / ?œ„?„ / ê²½ë„
# ?˜„??€ë°±í™”?  ??€êµ¬ì  / 35.865866 / 128.585675
# ??€êµ¬ë°±?™”?  / 35.869143 / 128.595984
# ?‹ ?„¸ê³„ë°±?™”?  ??€êµ¬ì  / 35.877372 / 128.628534
# ë¡??°ë°±í™”?  ??€êµ¬ì  / 35.875801 / 128.591183
# ??€êµ¬ë°±?™”?  ?”„?¼? / 35.854915 / 128.608861
# ?™?•„ë°±í™”?  ?‡¼?•‘?  / 35.862347 / 128.597566
# ?‹ ?„¸ê³„ë°±?™”? ??€ ?™??€êµ¬ì—­ ë¶€ê·¼ì´?¼ ì§€?„?—?„œ ?´?ƒˆ

# ??€?•™êµëª… / ?œ„?„ / ê²½ë„
# ê³„ëª…??€ ?˜??€ ?™?‚°ìº í¼?Š¤ / 35.867132 / 128.582309
# ??€êµ¬ê?€?†¨ë¦???€ ?œ ?Š¤?‹°?…¸ìº í¼?Š¤ / 35.8620364 / 128.5874015
# ê²½ë¶??€ ?˜??€ / 35.868840 / 128.603100
# ê²½ë¶??€ ì¹˜ë?€ / 35.864434 / 128.608359
# ??€êµ¬ë?€ ??€ëª…ë™ìº í¼?Š¤ / 35.838730 / 128.575100
# ê³„ëª…??€ ??€ëª…ìº ?¼?Š¤ / 35.856548 / 128.585985
# ??€êµ¬êµ?œ¡??€ / 35.855367 / 128.590767
# ?˜?‚¨??€?•™êµ? ??€êµ¬ìº ?¼?Š¤ / 35.854486 / 128.583121


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



# TimeToSubwayê°€ 15min~20min?¸ ê·¸ë£¹?— no_subway_nearby ë°œê²¬
# SubwayStation?´ no_subway_nearby?¸ ê·¸ë£¹?— TimeToSubwayê°€ 15min~20min?´ 166ê±°ë˜, no_subway_station_nearbyê°€ 238ê±°ë˜
# ?´?ƒì¹? ì²˜ë¦¬ ë°©ë²• ?…¼?˜
subset(house, TimeToSubway == "15min~20min", SubwayStation)
table(subset(house, SubwayStation == "no_subway_nearby", c(TimeToSubway)))

