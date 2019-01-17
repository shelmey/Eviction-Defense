# The xls file with the MPIA requested Eviction Tracking Log
# Source data, as received from the Sherriff's Office
in.evict<-"C:/Bmore City/Eviction Defense/Source data/Eviction Tracking Log Without Tenant Names.xls"
google.api.key<-""
mapout<-"filepath"

source(geocoding_utils.R)
# install.packages("zoo")
# Load Required Packages
require("gdata")
require("plyr")
require("dplyr")
require("reshape2")
require("lubridate")
require("zoo")
require("RCurl")
require("rjson")
library(stringr)

# We need a Perl executable to read .xls files. Make sure you have one.                    
# File path to the perl executable 
perl<-"C:/Strawberry/perl/bin/perl.exe"



# Read Sheet 1, Sep-Dec 2017
evictions.raw<-read.xls(in.evict,skip=8,sheet=1,
         perl=perl,
         as.is=T)

# Drop the superfluous columns and the date header rows.
evictions.cleaning <-  evictions.raw[!grepl("[^0-9]", evictions.raw$COUNT), # rows where the count column contains only digits
                names(evictions.raw)[!grepl("^X\\.", names(evictions.raw))]] # columns that don't start with X. (R reformatting for starting with a number)

# Clean dates 
evictions.cleaning<-within(evictions.cleaning,{
  DATE<-as.POSIXct(paste(DATE.1, TIME.1),format="%m/%d/%y %H:%M") 
  rm(list=c("DATE.1","TIME.1"))
})

# ll<-as.data.frame(table(evictions.cleaning$LAND.LORD...NOTES)) %>% arrange(desc(Freq))
summary(evictions.cleaning$DATE)
# Read the big dataset
evictions.raw.2<-read.xls(xls=in.evict,sheet=2, skip=1,
                        perl=perl,
                        as.is=T)
# evict.csv<-read.csv(evictions.raw.2)
# Drop header rows
# evictions.raw.2[evictions.raw.2$CASE..=="",]

evictions.cleaning.2<-within(evictions.raw.2,{
  rm(list=names(evictions.raw.2)[grepl("^X\\.", names(evictions.raw.2))])
  Date.str<-trimws(ifelse(CASE..=="" & DEPUTY=="",ADDRESS,NA)) %>%
    gsub("20013","2013",.)
  Date<-as.Date(DATE.2009)
  rm(DATE.2009)
  }
  )

evictions.cleaning.2<-evictions.cleaning.2[!(evictions.cleaning.2$CASE..==""
                                             & evictions.cleaning.2$DEPUTY==""
                                             & evictions.cleaning.2$TIME==""
                                             & evictions.cleaning.2$ADDRESS==""),]
evictions.cleaning.2<-evictions.cleaning.2[!grepl("[^0-9-]",evictions.cleaning.2$CASE..),]
# filter(evictions.cleaning.2,Date<as.Date('2009-01-01'))
filter(evictions.cleaning.2,Date>as.Date('2017-12-31'))
evictions.cleaning.2$Date[evictions.cleaning.2$Date<as.Date('2009-01-01') & evictions.cleaning.2$Date.str=="JAN/12/2017"]<-as.Date('2017-01-12')
# evictions.cleaning.2$Date.str<-na.locf(evictions.cleaning.2$Date.str,na.rm=F)
evictions.cleaning.2$Date[evictions.cleaning.2$CASE..=="532844"]<-as.Date('2016-12-12')
evictions.cleaning.2$Date[evictions.cleaning.2$CASE..=="107731"]<-as.Date('2013-11-14')
filter(evictions.cleaning.2,Date<as.Date('2009-01-01'))

evictions.cleaning.2$fixed.dates<-as.Date(ifelse((evictions.cleaning.2$Date<as.Date('2009-01-01') & lag(evictions.cleaning.2$Date)==lead(evictions.cleaning.2$Date)),
                                                 lag(evictions.cleaning.2$Date),
                                                 evictions.cleaning.2$Date))
# evictions.cleaning.2$Date<-fixed.dates
 write.csv(evictions.cleaning.2,
           "C:/Bmore City/Eviction Defense/Source data/cleaning.csv")
 
cleaning.2017<-filter(evictions.cleaning.2, Date>as.Date('2016-12-31'))

cleaning.2017$ADDRESS <- gsub("#.*$","",cleaning.2017$ADDRESS) %>% gsub("  "," ",.)
cleaning.2017$ADDRESS<-paste(cleaning.2017$ADDRESS,"Baltimore","MD")
geocode.results <- do.call(
    rbind.data.frame,
    lapply(cleaning.2017$ADDRESS,
           bmoreGeocode,
            google.api.key))

# cleaning.2017$ADDRESS<-paste(cleaning.2017$ADDRESS,"Baltimore","MD")
bmore.coded.full<-filter(geocode.results,between(Latitude,39.1963325151,39.3826883618) & between(Longitude,-76.7198786773,-76.5184626337)) 

# bmore.coded.cache<-transmute(bmore.coded.full,
#           GEO_address=NA,
#           Error.Status,
#             Longitude,
#           Latitude,
#        Address=ADDRESS)
# 
# bmore.coded.full.cache<-rbind(geocode.results.new,
#       bmore.coded.cache)
# write.csv(bmore.coded.full.cache,"C:/Bmore City/Eviction Defense/Source data/addresses.csv")

bmore.locations<-transmute(bmore.coded.full,
                           Address=sub(" Baltimore MD", "", Address),
                           Latitude,
                           Longitude
                           ) %>% filter(!duplicated(Address))
cleaning.2017.ad <- mutate(cleaning.2017,
                           Address=sub(" Baltimore MD", "", ADDRESS))

cleaned.coded <- inner_join(cleaning.2017.ad,
                            bmore.locations)

library(mapview)
library(sf)
library(htmlwidgets)

sf.2017<-st_as_sf(filter(cleaned.coded,between(Latitude,39.1963325151,39.3826883618) & between(Longitude,-76.7198786773,-76.5184626337)), coords = c("Longitude","Latitude"))

st_crs(sf.2017) <- 4269

evictions.2017.map <- mapview(sf.2017, xcol="Longitude", ycol="Latitude", cex=.3)

mapshot(evictions.2017.map,
        file = mapout)
