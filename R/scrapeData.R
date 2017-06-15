# scrapeData
#
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @title setLogging
#'
#' @description This function sets a global options variable called "logging" to TRUE OR FALSE. By default it is FALSE, so no logging is displayed.
#'
#' @param logger - A boolean variable. TRUE for printing out the logging outputs in the console.
#'
#'
#' @export
#'
setLogging <- function(logger) {
  options("logging" = logger)
  ifelse(logger == TRUE, print("Outputs/logs will be displayed!"), print("No console outputs/logs will be displayed!"))
}


#' @title getDailyOHLC
#'
#' @description This method crawls through the ariva.de website and grabs the daily price data out of the html tables.
#' You can get OHLC data from specific dates for specific stocks (ISIN required).
#' Either on putting the different stocks/isins and different dates for prices in a data.frame (input_df). This is good to get different prices on different dates and different stocks.
#' or by delclaring a vector of dates/intervals and stocks/isins. This is good for comparing the prices on different time periods within a stock or with other stocks.
#' The date must be in the right format (dd.mm.yyyy) as a character. It is not checked if the date is a correct trading day.
#' It is also not checked if the date intervals are correct.
#'
#' Status Quo:
#' - Only Xetra price data
#' - price data is cleaned up from stock splits (clean_splits)
#  - price data is cleaned up (recalculated) from dividends (clean_payout)
#  - price data is cleaned up (recalculated) from subscription rights (clean_bezug)
#'
#' The method strictly depends on the website structure of ariva and GET request of the csv file!
#'
#' @param searchDates - vector of charachters with different (formats see examples) Date requests: e.g. 20.09.2014-22.09.2014 (normal time period between two dates) // 08.2014 (short for one month) // 2014 (short for one year) // 20.08.2014 (short for one day) // 2013-2014 (time period between two years) // 08.2014-09.2014 (time period between two months)
#' @param isin - vector of characters with the id number of the stocks (ISIN = International Stock Identification Number)
#' @param input_df - data.frame with dates(as character in format dd.mm.yyyy, e.g. 21.02.2008 (%d.%m.%Y)) and isin(as character, e.g. DE0007037145(RWE AG)). Column names and order --> dates and isin are restricted!
#'
#' @return data.frame : date, isin, open, high, low, close, currency, volume, turnover
#'
#' @examples
#'
#' # First example for the first search option (input parameter: only input_df)
#' #' # Build the input parameter data.frame
#' dates <- c("25.02.2008","27.02.2008")
#' isin <- c("DE0007037145","DE0007037145")
#' input_df <- data.frame(dates, isin)
#'
#' # Get the prices for RWE AG (DE0007037145) of the 25.02.2008 and 27.02.2008
#' prices1 <- getDailyOHLC(input_df = input_df)
#'
#' # Second example for the second search option (input parameter: searchDates and isin)
#' # Build the input parameters
#' isin  <- c("DE0007037145","DE0007664039")
#' searchDates  <- c("20.09.2014-22.09.2014","20.08.2014","06.05.2014-16.05.2014")
#'
#' # Get the prices for RWE AG (DE0007037145) and VW VZ (DE0007664039) for different time periods or dates (see searchDates parameter above)
#' prices2 <- getDailyOHLC(searchDates=searchDates, isin=isin)
#'
#'
#' @export
#'
getDailyOHLC  <- function(searchDates, isin, input_df) {

  ## libraries
  library(XML)
  library(stringr)
  library(logging)
  library(dplyr)
  library(httr)

  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  #nameLogFile <- paste("getDailyOHLC", Sys.time(), ".txt", sep="")
  #addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  df = data.frame()

  # calculate the OHLC data with the input data.frame
  if(missing(searchDates) && missing(isin)) {
    df = input_df

    if(getOption("logging")) loginfo("getDailyOHLC - Calculate the OHLC data with the input data.frame")

    t <- getPriceCSV(df)

    if(getOption("logging")) loginfo("getDailyOHLC - DONE")
    return(t)
  }

  # calculate the OHLC data with the searchDates vector and the isin vector
  if(missing(input_df)) {
    returnList <- list()
    idx <- 1
    if(getOption("logging")) loginfo("getDailyOHLC - Calculate the OHLC data with the searchDates vector and the isin vector")

    # Build the input_df with the individual dates and the isin
    for(i in 1:length(isin)){
      for(j in 1:length(searchDates)) {

        dateSeq <- getDateSequence(searchDates[j])
        returnList[[idx]] = data.frame(dates = dateSeq, isin = rep(isin[i],length(dateSeq)))

        #df = data.frame(dates = dateSeq, isin = rep(isin[i],length(dateSeq)))
        # get the prices data.frame and add to the return list
        #returnList[[idx]]  <- readHTMLPriceTable(df)
        idx  <- idx+1
      }
    }

    edf <- do.call("rbind", returnList)
    t <- getPriceCSV(edf)

    if(getOption("logging")) loginfo("getDailyOHLC - DONE")

    return(t)

  }
}



getPriceCSV <- function(df){

  df$dates <- as.Date(df$dates, "%d.%m.%Y")
  relDates <- unique(df$dates)
  df$isin <- as.character(df$isin)

  r <- df %>% arrange(isin, dates)
  r$isin <- as.factor(r$isin)
  r <- split(r, r$isin)



  #
  # GET REQUEST OF CSV INFO:
  #

  # GET Link: http://www.ariva.de/quote/historic/historic.csv
  # Params Link + ?secu=4510&boerse_id=16&clean_split=1&clean_payout=0&clean_bezug=1&min_time=5.6.2016&max_time=5.6.2017&trenner=;&go=Download
  #
  # Payload:
  # secu = 4510 --> THIS NUMBER defines the security --> has to be read out --> XPATH: //input[contains(@name,'secu')]/@value
  # boerse_id = 6 --> is Xetra
  # clean_split = 1 --> price data is cleaned up (recalculated) from stock splits
  # clean_payout = 0 --> price data is cleaned up (recalculated) from dividends
  # clean_bezug = 1 --> price data is cleaned up (recalculated) from subscription rights
  # min_time = '5.6.2016'
  # max_time = '5.6.2017'
  # trenner = '|' ---> use | as delimiter since , is used for decimal and ; makes stupid errors!!
  # go = 'Download'

  #
  # !!! IMPORTANT !!! ---> Date Format: Without leading zeros!!!

  # Return semicolon seperated file with columns: Datum;Erster;Hoch;Tief;Schlusskurs;Stuecke;Volumen
  # Comma as decimal delimiter
  # blank as NA
  # E.g.:
  #
  # 2017-06-05;13,279;13,2835;13,247;13,247;;
  # 2017-06-04;13,287;13,287;13,275;13,279;;

  res <- data.frame()
  # For every split (= stock) do the csv download request --> later filter for the relevant dates!!
  for(stock in 1:length(r)){

    # Get the secu variable (= security --> internal stock ID) to specify the GET param for the csv download
    url <- paste("http://www.ariva.de/", unique(r[[stock]]$isin), "/historische_kurse", sep = "")
    getResponse <- GET(url, verbose())
    secu <- xpathSApply(htmlParse(content(getResponse, "text")), "//input[contains(@name,'secu')]/@value")

    # convert date in string with no leading zeros for days and months (notice that in year can be also zeros)
    min_time = sub(".", "", gsub("\\.0", ".", format(min(r[[stock]]$dates), ".%d.%m.%Y")), " %d  %m %Y") # sub function only replaces the first occurence
    max_time = sub(".", "", gsub("\\.0", ".", format(max(r[[stock]]$dates), ".%d.%m.%Y")), " %d  %m %Y") # sub function only replaces the first occurence

    # Prepare the downloaded file
    filename = "temp.csv"

    url = 'http://www.ariva.de/quote/historic/historic.csv'
    param = paste("?secu=", secu,
                  "&boerse_id=",6,
                  "&clean_split=",1,
                  "&clean_payout=",0,
                  "&clean_bezug=",1,
                  "&min_time=",min_time,
                  "&max_time=",max_time,
                  "&trenner=","|",
                  "&go=","Download", sep="")
    url = paste(url, param, sep = "")
    # download csv file
    download.file(url, filename, mode="wb")

    # Read in the downloaded csv file
    d <- read.csv(filename, header = TRUE, sep = "|", dec = ",", na.strings = c(""))
    colnames(d) <- c("Date","Open","High","Low","Close","Volume","Turnover")
    # Formatting
    d$Date <- as.Date(d$Date)
    # Format Volume and Turnover
    d$Volume <- as.numeric(gsub("\\.","",d$Volume))
    d$Turnover <-as.numeric(gsub("\\.","",d$Turnover))
    # Add the ISIN column
    d$ISIN <- unique(r[[stock]]$isin)

    # Re order since latest date is begging
    d <- arrange(d, Date)
    # Delete the temporary csv file
    file.remove(filename)

    res <- rbind(res,d)

  }

  # Filter the relevant dates
  res <- filter(res, Date %in% relDates)

  return(res)

}



#' This private helping method processes the request for the dates or date intervals:
#' e.g 20.09.2014-22.09.2014 // 08.2014 // 2014 // 20.08.2014 // 2013-2014 // 08.2014-09.2014
#' and builds an array with the daily dates to crawl through ariva
#'
#' Needs lubridate library
#'
getDateSequence  <- function(searchDate) {

  library(lubridate)
  result = c()

  if(getOption("logging")) loginfo("getDateSequence - Process the searchDate array and build the right date sequences")

  # Get the number of dots and dashes to distinguish the date request formats. E.g. if the recent request date is 08.2014 or 20.08.2014 ...
  dot <- "."
  dash <- "-"
  s1 <- gsub(dot,"",searchDate,fixed = TRUE) # Count the dots
  s2 <- gsub(dash,"",searchDate) # Count the dashes
  numDots <- as.numeric(nchar(searchDate) - nchar(s1)) # number of dots
  numDashes <- as.numeric(nchar(searchDate) - nchar(s2)) # number of dashes

  # Case:
  # dd.mm.yyyy-dd.mm.yyyy
  if(numDots == 4 && numDashes == 1) {
    start = strsplit(searchDate,"-")[[1]][1]
    end = strsplit(searchDate,"-")[[1]][2]
    start = as.Date(start,"%d.%m.%Y")
    end = as.Date(end,"%d.%m.%Y")
    result = format(seq(start,end,by=1), "%d.%m.%Y")
  }
  # Case:
  # mm.yyyy
  else if(numDots == 1 && numDashes == 0) {
    start = as.Date(paste("01.",searchDate,sep=""),"%d.%m.%Y")
    end = as.Date(paste("01.",searchDate,sep=""),"%d.%m.%Y")
    month(end) = month(end) + 1
    X <- seq(start,end,by=1)
    X <- X[1:length(X)-1] # delete last row because it belongs to the other month
    result = format(X, "%d.%m.%Y")
  }
  # Case:
  # yyyy
  else if(numDots == 0 && numDashes == 0) {
    start = as.Date(paste("01.01.",searchDate,sep=""),"%d.%m.%Y")
    end = as.Date(paste("01.01.",searchDate,sep=""),"%d.%m.%Y")
    year(end)  <- year(end) + 1
    X <- seq(start,end,by=1)
    X <- X[1:length(X)-1] # delete last row because it belongs to the other year
    result = format(X, "%d.%m.%Y")
  }
  # Case:
  # dd.mm.yyyy
  else if(numDots == 2 && numDashes == 0) {
    result = searchDate

  }
  # Case:
  # yyyy-yyyy
  else if(numDots == 0 && numDashes == 1) {
    start = strsplit(searchDate,"-")[[1]][1]
    end = strsplit(searchDate,"-")[[1]][2]
    start = as.Date(paste("01.01.",start,sep=""),"%d.%m.%Y")
    end = as.Date(paste("01.01.",end,sep=""),"%d.%m.%Y")
    year(end)  <- year(end) + 1
    X <- seq(start,end,by=1)
    X <- X[1:length(X)-1] # delete last row because it belongs to the other year
    result = format(X, "%d.%m.%Y")
  }
  # Case:
  # mm.yyyy-mm.yyyy
  else if(numDots == 2 && numDashes == 1) {
    start = strsplit(searchDate,"-")[[1]][1]
    end = strsplit(searchDate,"-")[[1]][2]
    start = as.Date(paste("01.",start,sep=""),"%d.%m.%Y")
    end = as.Date(paste("01.",end,sep=""),"%d.%m.%Y")
    month(end)  <- month(end) + 1
    X <- seq(start,end,by=1)
    X <- X[1:length(X)-1] # delete last row because it belongs to the other month
    result = format(X, "%d.%m.%Y")

  }
  return(result)
}




#' @title getStockInfoFromIsin
#'
#' @description This method crawls through the ariva.de website and scrapes out information about the given stocks (isin).
#' Status Quo: ticker info, sector.
#'
#' NOTE: Only the column/variable NominalValue is checked for NA values (== "-")
#'
#' The method strictly depends on the website structure of ariva!
#'
#' @param isin - vector of charachters with different stocks (isin), e.g. DE0007037145(RWE AG)
#'
#' @return data.frame with stock infos
#'
#' @examples
#'
#' # Build the input parameter
#' isin <- c("DE0007037145", "DE000ENAG999")
#'
#' # Get the information about for RWE AG (DE0007037145)
#' stockInfos <- getStockInfoFromIsin(isin = isin)
#'
#' @export
#'
getStockInfoFromIsin <- function(isin) {
  ## libraries
  library(XML)
  library(stringr)
  library(logging)
  library(dplyr)
  library(httr)
  library(tidyr)


  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  #nameLogFile <- paste("getDailyOHLC", Sys.time(), ".txt", sep="")
  #addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  res <- data.frame()

  for(i in 1:length(isin)){

    s <- isin[i]

    if(getOption("logging")) loginfo(paste("getStockInfoFromIsin - Get information for ", s, sep = ""))

    # Get the secu variable // http://www.ariva.de/rwe_vz-aktie/bilanz-guv

    url <- paste("http://www.ariva.de/", s, "/bilanz-guv", sep = "")
    htmlResponse <- htmlParse(content(GET(url, verbose()), "text"))
    # [contains(@name,'secu')]/@value
    mainData <- xpathSApply(htmlResponse, "//div[contains(@class,'stammdaten')]//td")
    # get the values inside the td elements
    t <- lapply(mainData, xmlValue)
    t <- data.frame(matrix(unlist(t), nrow=10, byrow=T))
    t <- spread(t, "X1", "X2")
    t$ISIN <- s

    res <- rbind(res, t)

  }

  # format result data.frame
  colnames(res) <- c("StockType", "Business", "Genus", "ListedSince", "Established", "Country", "NominalValue", "Sector", "Ticker", "Currency", "ISIN")
  res$Ticker <-  as.character(res$Ticker)
  res$Country <-  as.character(res$Country)
  res$Currency <-  as.character(res$Currency)
  res$Business <-  as.character(res$Business)
  res$StockType <-  as.character(res$StockType)
  res$Established <-  as.character(res$Established)
  res$Genus <-  as.character(res$Genus)
  res$ListedSince <- as.Date(res$ListedSince, "%d.%m.%Y")
  # delete all white spcaes and declare "-" as NA value
  res$NominalValue <- gsub("[[:blank:]]", "", res$NominalValue)
  res$NominalValue <- ifelse(res$NominalValue == "-", NA, res$NominalValue)

  if(getOption("logging")) loginfo(paste("getStockInfoFromIsin -DONE", s, sep = ""))

  return(res)

}






#' @title getStockMarketIndexList
#'
#' @description This method crawls through the ariva.de website and scrapes out the stock market indexes
#'
#' The method strictly depends on the website structure of ariva!
#'
#'
#' @return data.frame with stock market indexes
#'
#' @examples
#'
#' # Get a list of stock market indexes from ariva.de
#' indexList <- getStockMarketIndexList()
#'
#' @export
#'
getStockMarketIndexList <- function(isin) {
  ## libraries
  library(XML)
  library(stringr)
  library(logging)
  library(dplyr)
  library(httr)
  library(tidyr)


  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  #nameLogFile <- paste("getDailyOHLC", Sys.time(), ".txt", sep="")
  #addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  url <- paste("http://www.ariva.de/aktien/indizes", sep = "")
  htmlResponse <- htmlParse(content(GET(url, verbose()), "text"))
  # [contains(@name,'secu')]/@value
  indexLinks <- xpathSApply(htmlResponse, "//div[contains(@id,'CONTENT')]//tbody/tr/td/a/@href")
  indexNames <- xpathSApply(htmlResponse, "//div[contains(@id,'CONTENT')]//tbody/tr/td/a/text()", xmlValue)
  #indexNames <- lapply(indexNames, xmlValue)

  res <- data.frame(Name = indexNames, Link = paste("http://www.ariva.de", indexLinks, sep = ""))
  res$Name <- as.character(res$Name)
  res$Link<- as.character(res$Link)
  # Those indexes do not work
  res <- res[!(res$Name %in% c("VIX", "EGX 30 (Kairo)", "S&P/TSX 60", "ISE 30 (Istanbul)", "OMX Kopenhagen", "Mexican Bolsa")),]
  # get isins and add it as new variable to result data.framw
  c <- lapply(res$Link, function(x) getStockMarketIndexInfoFromLink(x))
  c <- as.character(c)
  res$ISIN <- c


  return(res)

}




#' @title getStockMarketIndexInfoFromLink
#'
#' @description This method crawls through the ariva.de website and scrapes out the stock market index info given its link @seealso getStockMarketIndexList
#'
#' The method strictly depends on the website structure of ariva!
#'
#' @param url - link of the ariva website to adress the main page of the stock market index
#'
#' @return data.frame with stock market index info
#'
#' @examples
#'
#' link <-  "http://www.ariva.de/dax-30"
#' # Get a list of stock market indexes from ariva.de
#' daxInfo <- getStockMarketIndexInfoFromLink(link)
#'
#' @export
#'
getStockMarketIndexInfoFromLink <- function(url) {
  ## libraries
  library(XML)
  library(stringr)
  library(logging)
  library(dplyr)
  library(httr)
  library(tidyr)


  # Setup the logger and handlers
  basicConfig(level="DEBUG") # parameter level = x, with x = debug(10), info(20), warn(30), critical(40) // setLevel()
  #nameLogFile <- paste("getDailyOHLC", Sys.time(), ".txt", sep="")
  #addHandler(writeToFile, file=nameLogFile, level='DEBUG')

  htmlResponse <- htmlParse(content(GET(url, verbose()), "text"))
  # [contains(@name,'secu')]/@value
  isin <- xpathSApply(htmlResponse, "id('pageSnapshotHeader')/div[2]/div[3]/div[2]/text()", xmlValue)
  isin <- strsplit(isin, "\\s+")[[1]][2]

  return(isin)

}














#' @title getISINFromYahooTicker
#'
#' @description This function takes an vector of Yahoo Ticker characters and returns the added ISIN number.
#'
#' @param vector of character - Yahoo Ticker (e.g. ADS.DE)
#'
#' @return data.frame : yahooTicker , isin OF THE UNIQUE INPUT VECTOR !!
#'
#' @examples
#'
#'
#' @export
#'
getISINFromYahooTicker  <- function(ytickers) {

  tickers  <- unique(ytickers) # reduce amount of request at yahoo website
  resList <- list()
  idx <- 1
  stock  <- c()
  for(i in 1:length(tickers)) {
    yticker  <- tickers[i]
    URL  <- paste("https://de.finance.yahoo.com/q?s=",yticker,sep="")

    w <- read_html(URL)

    title  <- xml_find_all(w, ".//div[@class='title']")
    name  <- xml_text(xml_children(title)[1])
    nlist  <- strsplit(name, " ")[[1]]
    name  <- as.character(paste(nlist[1:length(nlist)-1], collapse = ' ')) # skip last element -> "(ADS.DE)"

    isin  <- xml_text(xml_children(title)[2])
    isin  <- as.character(strsplit(isin, " ")[[1]][4])

    stock  <- c(name=name, isin=isin, yahooTicker=yticker)

    resList[[idx]]  <- stock
    idx  <- idx+1
  }
  edf <- do.call("rbind", resList)
  return(edf)
}


#' @title addLogReturns
#'
#' @description This method computes the log-difference returns in % ( 100 x diff(log(x)) ) and adds it to the input data.frame as a seperate column.
#' NOTE: First observation cannot have a return value. For computational reasons it has value 0.
#'
#' @param df - a data.frame with financial data @seealso getDailyOHLC
#' @param value - a single character value (e.g. "Close") or a character vector (e.g. c("Close", "Volume")) to define the value(s) on which to compute the return
#'
#' @return a data.frame with additional columns of the log returns in percent of the specified value(s)
#'
#' @examples
#'
#' # Get the financial data
#' prices <- getDailyOHLC("09.2014-11.2014", "DE0007037145")
#' # Calculate the log returns in percent
#' log.returns <- addLogReturns(prices, c("Close", "Volume"))
#'
#' @export
#'
addLogReturns  <- function(df, value){

  prices2returns <- function(x) round(100*diff(log(x)),2)
  # init an extra column otherwise cbind wont work
  res <- data.frame(0)

  for(i in 1:length(value)){
    v <- value[i]
    # add 0 value for first observation. NA or NULL is not possible
    res <- cbind(res, c(0,prices2returns(df[,v])))
  }
  # delete the artificial extra column
  res <- res[,2:ncol(res)]
  # rename the column names
  colnames(res) <- paste(value, "_LogReturn",sep="")
  # add the returns to the input data.frame
  df <- cbind(df,res)

  return(df)
}







#'
#' DEPRICATED
#'
#' This private method helps the @seealso getDailyOHLC function to read out the price table at ariva website.
#' Therefore it gets a data.frame of a stock with the dates for the required prices and returns the prices data.frame.
#'
#'
readHTMLPriceTable  <- function(df) {

  if(getOption("logging")) loginfo("readHTMLPriceTable - Crawl the websites and build the OHLC data.frame")

  # call internal private function to group the stocks on name and date(year/month) to reduce requests!
  groupedInputList <- groupInputData(df)

  #------------- HERE Loop-----------------
  returnList <- list()
  idx <- 1
  for(i in 1:length(groupedInputList)) {

    temp_df= groupedInputList[[i]]
    isin = as.character(temp_df$isin[1])
    m = temp_df$month[1]
    y = temp_df$year[1]
    # start request on ariva
    URL <- paste("http://www.ariva.de/", isin, "/historische_kurse?month=",y,"-",m,"-31", sep="")
    ## grab the page -- the html table is parsed nicely
    table_df <- as.data.frame(readHTMLTable(URL)[2]) # take second -> first table is miniquotes
    table_df <- table_df[1:nrow(table_df)-1,] # remove last row -> only bullshit
    # change names
    colnames(table_df) <- c("date", "open","high","low","close","currency","volume","turnover")

    # loop through data.frame in list[[i]] and get datas on the dates from the html table
    for(j in 1:nrow(temp_df)) {
      d  <-  temp_df[j,]$day

      # take the last two numbers to format the year date -> in the table years are in format "yy"
      search_y = paste(strsplit(y,"")[[1]][3], strsplit(y,"")[[1]][4],sep="")
      # build the search date
      search_date =  paste(d, m, search_y, sep=".")
      res <- table_df[table_df$date==search_date,]

      # Maybe there was no trading day -> skip
      if(nrow(res)>0) {
        res$isin <- isin # add isin to result data.frame
        #format the numbers and dates from the html table to be processable
        res <- formatOHLCTable(res)

        #add result data.frame into returnList
        returnList[[idx]] <- res
        idx  <- idx+1
      }
    }
  }
  # convert list in a single data.frame and reorder the columns
  edf <- do.call("rbind", returnList)
  edf <- edf[c(1,9,2,3,4,5,6,7,8)]
  return(edf[order(edf$isin,edf$date),])

}


#'
#' DEPRICATED
#'
#' private function for @seealso getDailyOHLC. Prepares the input data.frame of dates and corresponding isin.
#' Groups the data to reduce the request on ariva.de
#' Returns a list with the grouped data.frames.
#' Grouping: by year then month and then stock/isin
#'
groupInputData  <- function(dataFrame) {
  # first order by isin then order by date -> so stock is grouped with date order
  dataFrame <- dataFrame[order(dataFrame$isin,dataFrame$dates),]

  # second group stocks and months and year together -> for reduced requests
  # format date character into date to get the month,year,day and add it to input data.frame
  dataFrame$month <- format(as.Date(dataFrame$dates,"%d.%m.%Y"),"%m")
  dataFrame$year <- format(as.Date(dataFrame$dates,"%d.%m.%Y"),"%Y")
  dataFrame$day <- format(as.Date(dataFrame$dates,"%d.%m.%Y"),"%d")
  dataFrame$isin <- as.character(dataFrame$isin)

  resList <- list()
  idx <- 1
  uniy  <- unique(dataFrame$year)
  #go through all several years
  for(h in 1:length(uniy)) {
    c0  <- dataFrame[dataFrame$year==uniy[h],]
    unim  <- unique(c0$month)
    # for loop1 length(unim)
    for(i in 1:length(unim)) {
      c1  <- c0[c0$month==unim[i],]
      unis  <- unique(c1$isin)
      # if unis > 1 -> OTHER STOCK in c1 for loop length(unis)
      if(length(unis)>1) {
        for(j in 1:length(unis)){
          c2  <- c1[c1$isin==unis[j],]
          # add groupedStockByMonth into result list
          resList[[idx]] <- c2
          idx  <- idx+1
        }
      } else {
        # add  c1 directly into result list NO OTHER Stock in c1
        resList[[idx]]  <- c1
        idx  <- idx+1
      }
    }
  }
  return(resList)
}

#'
#' DEPRICATED
#'
#' Internal private function formats the price data read from the html table into processable data types.
#' Also format Date into a date class variable
#' Returns a fuly formatted data.frame row from the values of the html table
#'
#' BUG: Cant format turnover
#'
formatOHLCTable  <- function(inputOHLCTable) {
  open <- inputOHLCTable$open
  high <- inputOHLCTable$high
  low <- inputOHLCTable$low
  close <- inputOHLCTable$close
  volume <- inputOHLCTable$volume
  turnover <-  as.character(inputOHLCTable$turnover)
  date <-   inputOHLCTable$date

  # format res data.frame first: "."-> "" then "," -> "."
  # format characters (Open, High, Low, Close, Volume) into numbers
  inputOHLCTable$open = as.numeric(str_replace_all(gsub(".", "", open, fixed = TRUE),",","."))
  inputOHLCTable$high = as.numeric(str_replace_all(gsub(".", "", high, fixed = TRUE),",","."))
  inputOHLCTable$low = as.numeric(str_replace_all(gsub(".", "", low, fixed = TRUE),",","."))
  inputOHLCTable$close = as.numeric(str_replace_all(gsub(".", "", close, fixed = TRUE),",","."))
  #"." -> "" (in volume)
  inputOHLCTable$volume = as.numeric(gsub(".", "", volume, fixed = TRUE))
  # format turnover-> Mrd(x1000000000) M(x1000000) T(x1000) -> buggy cant see white space
  inputOHLCTable$turnover = str_replace_all(gsub(".", "", turnover, fixed = TRUE),",",".")

  # format date factor into date class
  inputOHLCTable$date = as.Date(date,"%d.%m.%y")

  return(inputOHLCTable)
}











