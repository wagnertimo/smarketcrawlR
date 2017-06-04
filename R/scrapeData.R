# scrapeData
#
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'




#' @title getOHLC
#'
#' @description This method crawls through the ariva.de financal website and grabs price data out of the html tables.
#' You can get OHLC data from specific dates for specific stocks (ISIN required).
#' Either on putting the different stocks/isins and different dates for prices in a data.frame (input_df). This is good to get different prices on different dates and different stocks.
#' or by delclaring a vector of dates/intervals and stocks/isins. This is good for comparing the prices on different time periods within a stock or with other stocks.
#' The date must be in the right format (dd.mm.yyyy) as a character. It is not checked if the date is a correct trading day.
#' It is also not checked if the date intervals are correct.
#'
#' The method strictly depends on the website structure of ariva!
#'
#' Needs libraries RCurl, XML and stringr
#'
#' Ex. 1: dates <- c("25.02.2008","27.02.2008"); isin <- c("DE0007037145","DE0007037145"); input_df <- data.frame(dates,isin); getOHLC(input_df=input_df)
#' Ex. 2: isin  <- c("DE0007037145","DE0007664039") searchDates  <- c("20.09.2014-22.09.2014","20.08.2014","06.05.2014-16.05.2014"); getOHLC(searchDates=searchDates, isin=isin)
#'
#' @param input_df - data.frame with dates(as character in format dd.mm.yyyy e.g. 21.02.2008 (%d.%m.%Y)) and isin(as character - international stock identification number (e.g. DE0007037145 // RWE AG)
#'      searchDate = vector of charachters with Date request e.g. 20.09.2014-22.09.2014 // 08.2014 // 2014 // 20.08.2014 // 2013-2014 // 08.2014-09.2014
#'      isin = vector of the id number of the stocks
#'
#' @return data.frame : date,isin,open,high,low,close,currency,volume,turnover
#'
#' @export
#'
getOHLC  <- function(searchDates,isin,input_df) {

  ## libraries
  library(RCurl)
  library(XML)
  library(stringr)

  df = data.frame()

  # calculate the OHLC data with the input data.frame
  if(missing(searchDates) && missing(isin)) {
    df = input_df
    return(readHTMLPriceTable(df))
  }

  # calculate the OHLC data with the searchDates vector and the isin vector
  if(missing(input_df)) {
    returnList <- list()
    idx <- 1
    for(i in 1:length(isin)){
      for(j in 1:length(searchDates)) {

        dateSeq <- getDateSequence(searchDates[j])
        df = data.frame(dates = dateSeq, isin = rep(isin[i],length(dateSeq)))

        # get the prices data.frame and add to the return list
        returnList[[idx]]  <- readHTMLPriceTable(df)
        idx  <- idx+1
      }
    }

    edf <- do.call("rbind", returnList)
    return(edf[order(edf$isin,edf$date),])
  }
}

#'
#' This private method helps the getOHLC function to read out the price table on ariva website.
#' Therefore it gets a data.frame of a stock with the dates for the required prices and returns the prices data.frame.
#'
#'
readHTMLPriceTable  <- function(df) {

  # call internal private function to group the stocks on name and date(year/month) to reduce requests
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
#' private function for getOHLC. Prepares the input data.frame of dates and corresponding isin.
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

#' This private helping method gets the request for the dates or date intervals:
#' 20.09.2014-22.09.2014 // 08.2014 // 2014 // 20.08.2014 // 2013-2014 // 08.2014-09.2014
#' and builds an array with the daily dates to crawl through ariva
#'
#' Needs lubridate library
#'
getDateSequence  <- function(searchDate) {

  library(lubridate)
  result = c()
  # The character to search for
  dot <- "."
  dash <- "-"
  # Replace all occurrences by the empty string - note that gsub uses regular expressions, so escape p accordingly
  s1 <- gsub(dot,"",searchDate,fixed = TRUE) # Count the length difference
  s2 <- gsub(dash,"",searchDate) # Count the length difference
  numDots <- as.numeric(nchar(searchDate) - nchar(s1)) # numOcc n
  numDashes <- as.numeric(nchar(searchDate) - nchar(s2)) # numOcc n

  if(numDots == 4 && numDashes == 1) {
    # dd.mm.yyyy-dd.mm.yyyy
    start = strsplit(searchDate,"-")[[1]][1]
    end = strsplit(searchDate,"-")[[1]][2]
    start = as.Date(start,"%d.%m.%Y")
    end = as.Date(end,"%d.%m.%Y")
    result = format(seq(start,end,by=1), "%d.%m.%Y")
  }
  else if(numDots == 1 && numDashes == 0) {
    # mm.yyyy
    start = as.Date(paste("01.",searchDate,sep=""),"%d.%m.%Y")
    end = as.Date(paste("01.",searchDate,sep=""),"%d.%m.%Y")
    month(end) = month(end) + 1
    X <- seq(start,end,by=1)
    X <- X[1:length(X)-1] # delete last row because it belongs to the other month
    result = format(X, "%d.%m.%Y")
  }
  else if(numDots == 0 && numDashes == 0) {
    # yyyy
    start = as.Date(paste("01.01.",searchDate,sep=""),"%d.%m.%Y")
    end = as.Date(paste("01.01.",searchDate,sep=""),"%d.%m.%Y")
    year(end)  <- year(end) + 1
    X <- seq(start,end,by=1)
    X <- X[1:length(X)-1] # delete last row because it belongs to the other year
    result = format(X, "%d.%m.%Y")
  }
  else if(numDots == 2 && numDashes == 0) {
    # dd.mm.yyyy
    result = searchDate

  }
  else if(numDots == 0 && numDashes == 1) {
    # yyyy-yyyy
    start = strsplit(searchDate,"-")[[1]][1]
    end = strsplit(searchDate,"-")[[1]][2]
    start = as.Date(paste("01.01.",start,sep=""),"%d.%m.%Y")
    end = as.Date(paste("01.01.",end,sep=""),"%d.%m.%Y")
    year(end)  <- year(end) + 1
    X <- seq(start,end,by=1)
    X <- X[1:length(X)-1] # delete last row because it belongs to the other year
    result = format(X, "%d.%m.%Y")
  }
  else if(numDots == 2 && numDashes == 1) {
    # mm.yyyy-mm.yyyy
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


#' @title getISINFromYahooTicker
#'
#' @description This function takes an vector of Yahoo Ticker characters and returns the added ISIN number.
#'
#' @param vector of character - Yahoo Ticker (e.g. ADS.DE)
#'
#' @return data.frame : yahooTicker , isin OF THE UNIQUE INPUT VECTOR !!
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


#' @title computePerformanceDF
#'
#' @description This method reads in a data frame with stocks(isin) and their OHLC data and then computes the respective daily performance. E.g. to be plotted in a chart.
#'
#' @param data.frame - with isin and OHLC data in a time period(s)
#' @return data.frame : performanceTable,
#'
computePerformanceDF  <- function(input_df){

  input_df$perf  <- 0 # set new variable in data.frame
  for(i in 2:nrow(input_df)) {
    price_t <- input_df[i,6] #close
    price_t1 <- input_df[i-1,6] #close in t-1

    input_df[i,]$perf  <- round(((price_t - price_t1)/price_t1)*100,2)
  }

  return(input_df)
}














