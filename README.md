# smarketcrawlR <br/> – The R package for crawling data of the stock market - 


## Goal

This R package provides functions to crawl the stock market from the platform http://www.ariva.de.
Although Yahoo has (and maybe several other providers) an API to request financial (stock) data, those platforms lack some information, especially in conjunction with german stock market data. After some reasearch I found that *ariva* provides the most information for all international stocks and its website structure allows *easy* crawling. This package will grow iteratively with additonal functionalities. Nevertheless, the methods are strictly depending on the website structure of ariva and the GET request structure of the csv file!

## TO COME

Here is a list of features which will be added over time:
- Get stock information:
    - list stocks from stock indices --> Ask about e.g. DAX or NASDAQ and retrieve their current stocks
    - further stock information on ISIN with: Yahoo ticker, sector,, location etc. Also dividends, general meetings etc.
    - Build an internal text file with (own) standardized stock names for easier request, such that ISIN can be replaced with the stock name. Therefore crawl all possible stocks (ISIN, Yahoo ticker and name) and put in a txt file. Should be able to be updated.
    
- Get fundamental data:
    - Crawl also fundamental data like PE ratio or equity ratio etc.

- Quick Plot functions:
    - Provide pre-defined plots for price and fundamental data

- Preprocess Price and Fundamental Data:
   - Add functions to process the data. E.g. calculate returns

## Get Started

### Installing

When installing this package you should at least use the *R version 3.3.0 (2016-05-03)*. For the library dependecies see the section below. You can easily install this R package by using the `install_github()` function from the `devtools` package:

```r
library(devtools)
install_github("wagnertimo/smarketcrawlR")
```
### Library dependencies

Before using this R package, please check that you have installed the following R packages. Normally during the installation of the package those dependencies will also be installed. If not you have to do it manually.

- `httr`
- `xml2`
- `XML`
- `lubridate`
- `dplyr`
- `ggplot2`
- `stringr`
- `logging`

### Usage

#### Get the price data



* `getDailyOHLC(searchDates, isin, input_df)`: This function retrieves daily price data (Open, High, Low, Close, Volume, Turnover) from Xetra (Status Quo, will be extended in the future). The price data is cleaned by splits and subscription rights (dividends are priced!, further options to toggle them on/off will be extended in the future). There are different ways to retrieve the data or specify its input parameters:
    1. The first option is to simply declare a date string (date formats are dd.mm.YYYY) and a ISIN string to get the data of that stock on that date. The date can be a single date, a single month, a single year or date periods of days, months and years. So e.g. if you want price data from September 2014 till (including) November 2014, the date format is "09.2014-11.2014". The data for the whole year 2014 you get with "2014".
    2. The second option extends the first one by the possibility to define several date formats and stocks(ISINs) in an array. This is good if you need to compare different time periods for several stocks. All that data will be in one data.frame grouped by the stocks and sorted by the dates.
    3. The third option specifies an input data.frame. The data.frame needs dates and isin as column names. The rows are individual dates with an isin (e.g. "22.09.2014" "DE0007037145"). For this option you need to name the `getDailyOHLCìnput_df = df)` parameter name.


```r
# Activate the package in the workspace
library(smarketcrawlR)

# You have to set logging to TRUE or FALSE if you want logs printed out and written in a file (Good for Debugging)
# No default yet. Will break if not set.
setLogging(TRUE)


# 1. Option: Time Period string and ISIN strings as input params
# Get the price data for RWE (DE0007037145) of week 22.09.2014-26.09.2014

prices <- getDailyOHLC("22.09.2014-26.09.2014", "DE0007037145")
head(prices)

# Output:
#         Date   Open   High    Low  Close Volume Turnover         ISIN
# 1 2014-09-22 23.955 24.355 23.955 24.005  18425   445699 DE0007037145
# 2 2014-09-23 24.250 24.600 23.790 24.070 127625  3096999 DE0007037145
# 3 2014-09-24 23.960 24.345 23.960 24.345  31045   749568 DE0007037145
# 4 2014-09-25 24.450 24.760 24.065 24.080  86060  2103094 DE0007037145
# 5 2014-09-26 24.220 24.220 23.900 24.195  26344   634047 DE0007037145


# 2. Option: Use an array of different request dates (or periods) and stocks
# Get the prices for RWE AG (DE0007037145) and VW VZ (DE0007664039) for different time periods or dates (see searchDates parameter above)

# Build the input arrays of dates (or time periods) and stocks (with isin)
searchDates  <- c("20.09.2014-22.09.2014","20.08.2014","06.05.2014-16.05.2014")
isin  <- c("DE0007037145","DE0007664039")

prices <- getDailyOHLC(searchDates = searchDates, isin = isin)
# Alternative: prices <- getDailyOHLC(searchDates, isin2)
head(prices)

# Output:
#         Date   Open   High    Low  Close Volume Turnover         ISIN
# 1 2014-05-06 20.360 20.775 20.331 20.640 187223  3867569 DE0007037145
# 2 2014-05-07 20.500 21.005 20.445 20.975 108269  2251327 DE0007037145
# 3 2014-05-08 20.990 21.270 20.900 21.195  68254  1442694 DE0007037145
# 4 2014-05-09 21.215 21.405 21.025 21.075  53477  1133517 DE0007037145
# 5 2014-05-12 21.075 21.290 20.850 21.000  84629  1779225 DE0007037145
# 6 2014-05-13 21.000 21.000 20.555 20.720 124451  2580944 DE0007037145


# 3. Option: Use an input data.frame (input_df) 
# Get the prices for RWE AG (DE0007037145) of the 25.02.2008 and 27.02.2008

# Build the input data.frame with dates and isin columns of requested dates and stocks
dates <- c("25.02.2008","27.02.2008")
isin <- c("DE0007037145","DE0007037145")
input_df <- data.frame(dates, isin)

prices <- getDailyOHLC(input_df = input_df)
head(prices)

# Output:
#         Date    Open    High     Low   Close Volume Turnover         ISIN
# 1 2008-02-25 66.8938 67.3948 65.7217 67.2245 136828  9198202 DE0007037145
# 2 2008-02-27 67.1543 67.6252 66.7436 67.3146 122037  8214851 DE0007037145




```




