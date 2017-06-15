#
# The test script
#


#' --------------------------------------------------------------------------------------------------------
#'
#' Testing price data
#'

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

#'
#' --------------------------------------------------------------------------------------------------------









#' --------------------------------------------------------------------------------------------------------
#'
#' Testing stock info
#'

setLogging(TRUE)


isin <- c("DE0007037145", "DE000ENAG999")

t <- getStockInfoFromIsin(isin)
t


#'
#' --------------------------------------------------------------------------------------------------------






#' --------------------------------------------------------------------------------------------------------
#'
#' Testing get stock indexes
#'

setLogging(TRUE)


# Ultimate goal:
#' List of indexes as info lists
#'
#' e.g.
#'
#'  [[1]]
#       [1] (Name): "DAX"
#       [2] (ISIN): "DE0008469008"
#       [3] (Stocks):
#             [1] (Names): "BMW", "Adidas", .....
#             [2] (Links): "...", "...."

t <- getStockMarketIndexList()



r <- getStockMarketIndexInfoFromLink(t[1,]$Link)

r


remove(list = lsf.str())




#'
#' --------------------------------------------------------------------------------------------------------








