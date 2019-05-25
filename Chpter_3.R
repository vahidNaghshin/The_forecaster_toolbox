library(fpp2)
# Problem 3-1
autoplot(usnetelec)
(lambda <- BoxCox.lambda(usnetelec))
autoplot(BoxCox(usnetelec,lambda))

autoplot(usgdp)
(lambda <- BoxCox.lambda(usgdp))
autoplot(BoxCox(usgdp,lambda))

autoplot(mcopper)
(lambda <- BoxCox.lambda(mcopper))
autoplot(BoxCox(mcopper,lambda))

autoplot(enplanements)
(lambda <- BoxCox.lambda(enplanements))
autoplot(BoxCox(enplanements,lambda))

#Problem 3-2
autoplot(cangas)
(lambda <- BoxCox.lambda(cangas))
autoplot(BoxCox(cangas,lambda))
# Box-Cox transformation is unhelpful cause the variation is not monotonically increasing or decreasing

#Problem 3-3
retaildata <- readxl::read_excel("/Users/vnagh/Desktop/untitled_folder_4/retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))
autoplot(myts)
(lambda <- BoxCox.lambda(myts))
autoplot(BoxCox(myts,lambda))

#Problem 3-5
beer <- window(ausbeer, start=1992) 
fc <- snaive(beer)
autoplot(fc, PI=T)
res <- residuals(fc)
autoplot(res)
checkresiduals(fc)


#Problem 3-6
autoplot(WWWusage)
fc <- naive(WWWusage)
autoplot(fc, PI=T)

autoplot(bricksq)
fc <- snaive(bricksq)
autoplot(fc, PI=T)

#Problem 3-8
myts.train <- window(myts, end=c(2010,12))
myts.test <- window(myts, start=2011)
autoplot(myts) +
  autolayer(myts.train, series="Training") + autolayer(myts.test, series="Test")

fc <- snaive(myts.train)
accuracy(fc,myts.test)
checkresiduals(fc)


# Problem 3-9
autoplot(visnights[,"QLDMetro"])
train1 <- window(visnights[, "QLDMetro"],
                 end = c(2015, 4))
train2 <- window(visnights[, "QLDMetro"],
                 end = c(2014, 4))
train3 <- window(visnights[, "QLDMetro"],
                 end = c(2013, 4))

visnight.fit1 <- snaive(train1,h=4)
visnight.fit2 <- snaive(train2,h=4)
visnight.fit3 <- snaive(train3,h=4)

test1 <- window(visnights[, "QLDMetro"], start=c(2015,4),end = c(2016, 4))
test2 <- window(visnights[, "QLDMetro"], start=c(2014,4),end = c(2015, 4))
test3 <- window(visnights[, "QLDMetro"], start=c(2013,4),end = c(2014, 4))

MAPE.train1 <- accuracy(visnight.fit1,test1)[5]
MAPE.train2 <- accuracy(visnight.fit2, test2)[5]
MAPE.train3 <- accuracy(visnight.fit3, test3)[5]
print(cbind(MAPE.train1, MAPE.train2, MAPE.train3))

autoplot(train1) +
  autolayer(visnight.fit1, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("visnights") +
  ggtitle("Forecasts for quarterly visnights") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(train2) +
  autolayer(visnight.fit2, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("visnights") +
  ggtitle("Forecasts for quarterly visnights") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(train3) +
  autolayer(visnight.fit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("visnights") +
  ggtitle("Forecasts for quarterly visnights") +
  guides(colour=guide_legend(title="Forecast"))

# Problem 3-10
dowjones.ts <- ts(dowjones, start=c(2006,8,28), frequency = 365)
autoplot(dowjones.ts)
autoplot(dowjones.ts) +
  autolayer(meanf(dowjones.ts, h=10),
            series="Mean", PI=FALSE) +
  autolayer(naive(dowjones.ts, h=10),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(dowjones.ts, h=10, drift=TRUE),
            series="drift", PI=FALSE) +
  ggtitle("Forecasts for dowjones") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

#Problem 3-11
autoplot(ibmclose)
train <-ibmclose[1:300]
test <- ibmclose[301:length(ibmclose)]
test <- ts(test, start=301)

ibmclose.fit1 <- naive(train,h=69)
ibmclose.fit2 <- snaive(train,h=69)
ibmclose.fit3 <- meanf(train,h=69)
ibmclose.fit4 <- rwf(train,h=69, drift = T)

accuracy(ibmclose.fit1, test)
accuracy(ibmclose.fit2, test)
accuracy(ibmclose.fit3, test)
accuracy(ibmclose.fit4, test)

checkresiduals(ibmclose.fit1)
checkresiduals(ibmclose.fit2)
checkresiduals(ibmclose.fit3)
checkresiduals(ibmclose.fit4)
