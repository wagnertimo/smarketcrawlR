#
# The test script
#

setLogging(TRUE)



prices <- getDailyOHLC("09.2014-11.2014", "DE0007037145")
head(prices)




dates <- c("25.02.2008","27.02.2008")
isin <- c("DE0007037145","DE0007037145")
input_df <- data.frame(dates, isin)

# Get the prices for RWE AG (DE0007037145) of the 25.02.2008 and 27.02.2008
prices1 <- getDailyOHLC(input_df)

head(prices1)
#         Date    Open    High     Low   Close Volume Turnover         ISIN
# 1 2008-02-25 66.8938 67.3948 65.7217 67.2245 136828  9198202 DE0007037145
# 2 2008-02-27 67.1543 67.6252 66.7436 67.3146 122037  8214851 DE0007037145


isin2  <- c("DE0007037145","DE0007664039")
searchDates  <- c("20.09.2014-22.09.2014","20.08.2014","06.05.2014-16.05.2014")

# Get the prices for RWE AG (DE0007037145) and VW VZ (DE0007664039) for different time periods or dates (see searchDates parameter above)
prices2 <- getDailyOHLC(searchDates = searchDates, isin = isin2)

head(prices2)
#         Date   Open   High    Low  Close Volume Turnover         ISIN
# 1 2014-05-06 20.360 20.775 20.331 20.640 187223  3867569 DE0007037145
# 2 2014-05-07 20.500 21.005 20.445 20.975 108269  2251327 DE0007037145
# 3 2014-05-08 20.990 21.270 20.900 21.195  68254  1442694 DE0007037145
# 4 2014-05-09 21.215 21.405 21.025 21.075  53477  1133517 DE0007037145
# 5 2014-05-12 21.075 21.290 20.850 21.000  84629  1779225 DE0007037145
# 6 2014-05-13 21.000 21.000 20.555 20.720 124451  2580944 DE0007037145







