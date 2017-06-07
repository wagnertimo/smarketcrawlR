#
# The test script
#

setLogging(TRUE)



prices <- getDailyOHLC("09.2014-11.2014", "DE0007037145")
head(prices)
log.returns <- addLogReturns(prices, c("Close", "Volume"))
head(log.returns)




dates <- c("25.02.2008","27.02.2008")
isin <- c("DE0007037145","DE0007037145")
input_df <- data.frame(dates, isin)

# Get the prices for RWE AG (DE0007037145) of the 25.02.2008 and 27.02.2008
prices1 <- getDailyOHLC(input_df)

head(prices1)


isin2  <- c("DE0007037145","DE0007664039")
searchDates  <- c("20.09.2014-22.09.2014","20.08.2014","06.05.2014-16.05.2014")

# Get the prices for RWE AG (DE0007037145) and VW VZ (DE0007664039) for different time periods or dates (see searchDates parameter above)
prices2 <- getDailyOHLC(searchDates = searchDates, isin = isin2)

head(prices2)


