# The xls file with the MPIA requested Eviction Tracking Log
# Source data, as received from the Sherriff's Office

# Program Parameters

# The xls file with the MPIA requested Eviction Tracking Log
# Source data, as received from the Sherriff's Office
in.evict<-"Eviction Tracking Log Without Tenant Names.xls"

# Output csv
outfile <- paste0("cleaning", today(), ".csv")

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


# Read the big dataset
evictions.raw.2<-read.xls(xls=in.evict,sheet=2, skip=1,
                        perl=perl,
                        as.is=T)

# Remove fields with no data and Clean dates
evictions.cleaning.2<-within(evictions.raw.2,{
  rm(list=names(evictions.raw.2)[grepl("^X\\.", names(evictions.raw.2))])
  Date.str<-trimws(ifelse(CASE..=="" & DEPUTY=="",ADDRESS,NA)) %>%
    gsub("20013","2013",.)
  Date<-as.Date(DATE.2009)
  rm(DATE.2009)
  }
  )

# Removing the date header rows 
evictions.cleaning.2<-evictions.cleaning.2[!(evictions.cleaning.2$CASE..==""
                                             & evictions.cleaning.2$DEPUTY==""
                                             & evictions.cleaning.2$TIME==""
                                             & evictions.cleaning.2$ADDRESS==""),]

evictions.cleaning.2<-evictions.cleaning.2[!grepl("[^0-9-]",evictions.cleaning.2$CASE..),]

# Correcting a known formatting problem (gross)
evictions.cleaning.2$Date[evictions.cleaning.2$Date<as.Date('2009-01-01') & evictions.cleaning.2$Date.str=="JAN/12/2017"]<-as.Date('2017-01-12')

# Correcting a couple individual records (gross)
evictions.cleaning.2$Date[evictions.cleaning.2$CASE..=="532844"]<-as.Date('2016-12-12')
evictions.cleaning.2$Date[evictions.cleaning.2$CASE..=="107731"]<-as.Date('2013-11-14')


fixed.dates<-as.Date(ifelse((evictions.cleaning.2$Date<as.Date('2009-01-01') & lag(evictions.cleaning.2$Date)==lead(evictions.cleaning.2$Date)),
                                                 lag(evictions.cleaning.2$Date),
                                                 evictions.cleaning.2$Date))
evictions.cleaning.2$Date<-fixed.dates

names(evictions.cleaning)<-c("COUNT", 
"DEPUTY", 
"CLERK", 
"TIME", 
"DATE", 
"DEPUTY.NAME", 
"RECORDED.BY", 
"ADDRESS", 
"CASE", 
"LAND.LORD.NOTES", 
"T.P", 
"T.N.P", 
"X", 
"ANIMAL", 
"NO.SHOW", 
"COMMERCIAL", 
"CANCELATIOND")

names(evictions.cleaning.2) <-
c("CASE", 
"X", 
"DEPUTY", 
"TIME", 
"ADDRESS", 
"CLERK", 
"COMMENTS", 
"DATE", 
"Date.str"
)

evictions.cleaning$COMMENTS <- NA

evictions.cleaning.3 <-
rbind(select(evictions.cleaning,
             CASE,
             DEPUTY,
             TIME,
             ADDRESS,
             CLERK,
             COMMENTS,
             DATE),
      select(evictions.cleaning.2,
             CASE,
             DEPUTY,
             TIME,
             ADDRESS,
             CLERK,
             COMMENTS,
             DATE))

# Write clean dataset 
 write.csv(evictions.cleaning.3,
           outfile,
           row.names = FALSE)
