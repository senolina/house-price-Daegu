# 변수 선택
house_tmp <- subset(house, select = c(Floor, AptManageType, N_Parkinglot.Ground., N_Parkinglot.Basement.,
                                      TimeToSubway, N_APT, N_manager, N_elevators, SubwayStation,
                                      N_FacilitiesNearBy.PublicOffice.,
                                      N_FacilitiesNearBy.Park., N_FacilitiesNearBy.ETC.,
                                      N_SchoolNearBy.Total., Size.p., DateSold, DateBuilt, Price.per.Size))



# modeling
set.seed(1234)
train_ind <- sample(1:nrow(house_tmp), nrow(house_tmp) * 0.7)
train <- house_tmp[train_ind, ]
names(train)
train_X <- train[, -17]
train_Y <- house[train_ind, "SalePrice"]
test <- house_tmp[-train_ind, ]
test_X <- test[, -17]
test_Y <- house[-train_ind, "SalePrice"]

plot(lm(Price.per.Size ~ as.numeric(DateSold), data = train))

model1 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2), data = train)
summary(model1)
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

model2 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt), data = train)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

model3 <- lm(Price.per.Size ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + Floor, data = train)
summary(model3)
par(mfrow = c(2, 2))
plot(model3)
par(mfrow = c(1, 1))

model4 <- lm(sqrt(Price.per.Size) ~ poly(as.numeric(DateSold), 2) + as.numeric(DateBuilt) + Floor, data = train)
summary(model4)
par(mfrow = c(2, 2))
plot(model4)
par(mfrow = c(1, 1))

model5 <- lm(sqrt(Price.per.Size) ~ poly(as.numeric(DateSold), 1) + as.numeric(DateBuilt) + ., data = train)
summary(model5)
par(mfrow = c(2, 2))
plot(model5)
par(mfrow = c(1, 1))

model5_step1 <- step(model5, direction = "both", k = 2*log(nrow(train)))
par(mfrow = c(2, 2))
plot(model5_step1)
par(mfrow = c(1, 1))
summary(model5_step1)

#adjust r squared
#0.8187 1차
#0.8287 2차
#0.8225 log 2차
#0.8297 sqrt 2차
#0.8225 log(sqrt) 2차


model5_step2 <- step(model5, direction = "both")
par(mfrow = c(2, 2))
plot(model5_step2)
par(mfrow = c(1, 1))