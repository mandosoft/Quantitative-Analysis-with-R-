
require(quantmod)
require(RODBC)
require(dplyr)
require(urca)
require(GGally)

#Exploratory analysis between AAPL and VIX 

AAPL <- getSymbols("AAPL", auto.assign = FALSE)
VIX <- getSymbols("VIX", auto.assign = FALSE)


prices1 <- AAPL$AAPL.Close
aapl <- AAPL$AAPL.Adjusted

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



#corr plot for visuals
ggcorr(AAPL, VIX)

View(VIX)
candleChart(AAPL, multi.col = TRUE, theme = 'white', subset = '2017::2018')

