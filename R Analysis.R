
require(quantmod)
require(RODBC)
require(dplyr)
require(urca)
require(GGally)
require(ggplot2)
require(quantstrat)
#Exploratory analysis between AAPL and VXX 

AAPL <- getSymbols("AAPL", from = '2017-01-01', to = '2018-01-01', adjust = T, auto.assign = FALSE)
VXX <- getSymbols("VXX", from = '2017-01-01', to = '2018-01-01', adjust = T, auto.assign = FALSE)

 
prices <- cbind(AAPL[,6], VXX [,6])
price_changes <- apply(prices,2 , diff)

# observations on 
plot(price_changes[, 1], price_changes[, 2],
     xlab = "VXX Price Changes",
     ylab = "AAPL Price Changes", 
     main = "AAPL vs. VXX",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
grid()

ans <- lm(price_changes[,2] ~ price_changes[,1])
beta <- ans$coefficents[2]

beta

prices1 <- AAPL$AAPL.Close
aapl <- AAPL$AAPL.Adjusted
vxx <- VXX$VXX.Adjusted

#compute log prices to analyze stationarity 
returns1 <- diff(log(prices1))
scatter.smooth(returns1)

test <- ur.kpss(as.numeric(aapl))
class(test)

# if (FALSE){
# [1] "ur.kpss"
# attr(,"package")
# [1] "urca"
# > test@teststat
# [1] 26.84044
# > test@cval
# 10pct  5pct 2.5pct  1pct
# critical values 0.347 0.463  0.574 0.739
# }

# test subsetting data at 10 pct threshold to obtain stationarity

test_2013 <- ur.kpss(as.numeric(aapl['2013::']))
class(test_2013)

# > test_2013@teststat
# [1] 15.79085
# > test_2013 <- ur.kpss(as.numeric(aapl['2007::']))
# > class(test_2013)
# [1] "ur.kpss"
# attr(,"package")
# [1] "urca"
# > test_2013@teststat
# [1] 26.84044
# > test_2013 <- ur.kpss(as.numeric(aapl['2017::']))
# > class(test_2013)
# [1] "ur.kpss"
# attr(,"package")
# [1] "urca"
# > test_2013@teststat
# [1] 7.046115

# multiple tests and still must reject null hypothesis 

qqnorm(as.numeric(aapl), main = "AAPL empirical returns", cex.main = 0.8)
qqline(as.numeric(returns), lwd = 2)
grid()

answer <- shapiro.test(as.numeric(aapl))

# Shapiro-Wilk normality test
# 
# data:  as.numeric(aapl)
# W = 0.89592, p-value < 2.2e-16

#assessing distrubtion
ggplot(AAPL, aes(group ='', x=AAPL$AAPL.Adjusted, y=AAPL$AAPL.Close)) +
  geom_boxplot(outlier.color = "black", outlier.size = 1) +
  labs(x="Adjusted Returns", y="Close Price")

reg <- lm(VXX.Close ~ AAPL.Close, data = sv)



