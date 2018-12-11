
require(quantmod)
require(RODBC)
require(dplyr)
require(urca)
require(GGally)

#Exploratory analysis between AAPL and VIX 

AAPL <- getSymbols("AAPL", auto.assign = FALSE)
VIX <- getSymbols("VIX", auto.assign = FALSE)


prices1 <- AAPL$AAPL.Close

#compute log prices to analyze stationarity 
returns1 <- diff(log(prices1))
scatter.smooth(returns1)

#corr plot for visuals
ggcorr(AAPL, VIX)

View(VIX)
candleChart(AAPL, multi.col = TRUE, theme = 'white', subset = '2017::2018')

