## Functions to download the air quality data from the website
##
## Author: Tuan Vu; v.vu@bham.ac.uk
###############################################################################
## Sources of Air Quality Data sets in mega-cities:
## EU:http://airindex.eea.europa.eu/ (using a Python package- Airbase)
## UK: https://uk-air.defra.gov.uk/data/data_selector_service#mid
## China: http://beijingair.sinaapp.com/
## Germany:https://www.umweltbundesamt.de/ or https://www.env-it.de/stationen/public/downloadRequest.do
## London:https://www.londonair.org.uk/LondonAir/Default.aspx
## Paris:https://www.airparif.asso.fr/en/indices/resultats-jour-citeair#jour
## Berlin:https://luftdaten.berlin.de/lqi
## Hong Kong: aqhi.gov.hk 
## Istabul: http://laboratuvar.cevre.gov.tr/Default.ltr.aspx

library(openair)
library(lubridate)
library(tibble)
library(janitor)
library(dplyr)
library (RCurl)

#link_download<-getURL(url, ..., .opts = list())  ### Get data from URL
# Example for download Air quality data in Berlin from the website: https://luftdaten.berlin.de/
# https://luftdaten.berlin.de/station/mc027?period=1h&timespan=custom&start%5Bdate%5D=27.06.2009&start%5Bhour%5D=00&end%5Bdate%5D=27.06.2009&end%5Bhour%5D=23#station-data

### Function to download the data
download.data <- function(x) {
   date<-as.Date(x, origin = "1900-01-01")
   y <- lubridate::year(date)
   m <- lubridate::month(date)
   d <- lubridate::day(date)
   # use paste0 to make full path with years. Change the link. Find the download link by "Ctrl+J"
   path <- paste0("https://luftdaten.berlin.de/station/mc027.csv?group=pollution&period=1h&timespan=custom&start%5Bdate%5D=",d,".",m,".",y,
                 "&start%5Bhour%5D=00&end%5Bdate%5D=",d,".",m,".",y,"&end%5Bhour%5D=23")
   # Skip several 4 first lines & empty lines
   data_download <- try(read.table(path, skip = 4, header = FALSE, sep=";"))
   if (!inherits(data_download, 'try-error'))
   return(data_download)
   }

# Start_date & end_date for a period(01/01/2009-31/12/2018)
# 01/01/2009: 39812 & 31/12/2018: 43463 --Excel date

data.site <- ldply(39812:43463, download.data)
     names(data.site) <- c("date2", "NO","NO2","NOx","o3")
     # make a proper date/time, can paste together: data.site$date <- paste(o3$date,o3$time)
     data.site$date<-dmy_hm(data.site$date2, tz = "UTC")
# Save the dataset
write.csv(data.site(workingDirectory,"data.site.csv",sep=""))
     
