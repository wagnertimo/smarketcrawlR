# smarketcrawlR <br/> – The R package for crawling data of the stock market - 


## Goal

This R package provides functions to crawl the stock market from the platform http://www.ariva.de.
Although Yahoo has (and maybe several other providers) an API to request financial (stock) data, those platforms lack some information, especially in conjunction with german stock market data. After some reasearch I found that *ariva* provides the most information for all international stocks and its website structure allows *easy* crawling. This package will grow iteratively with additonal functionalities. 

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


### Usage





