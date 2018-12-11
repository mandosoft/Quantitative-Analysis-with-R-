library(quantmod)
require(quantmod)
require(RODBC)
require(dplyr)

AAPL <- getSymbols("AAPL", auto.assign = FALSE)

candleChart(AAPL, multi.col = TRUE, theme = 'white', subset = '2017::2018')

