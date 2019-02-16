
# Load program functions
source("geocoding_utils.R")

# Load required packages
library(dplyr)
library(lubridate)
library(mapview)
library(sf)
library(htmlwidgets)
library(RCurl)
library(rjson)

# Parameters
# Clean dataset
in.file<-"C:/Bmore City/Eviction Defense/Source data/cleaning.csv"
cache <- "C:/Bmore City/Eviction Defense/Outputs/addresses2019-01-27.csv"
# Dates for filtering the dataset to geocode
start.date <- as.Date('2014-12-31')
end.date <- as.Date('2016-01-01')
output <- paste0("Geocoded Evictions ", start.date," to ",end.date,".csv")
mapout <- paste0("Evictions ", start.date," to ",end.date,".html")
cache.out <- paste0("addresses",today(),".csv")

# Google API key
google.api.key<-""

evictions<-read.csv(in.file,
                    strip.white = TRUE,
                    stringsAsFactors = FALSE) %>%
  filter(Date>start.date & Date<end.date)

address.cache <- read.csv(cache,
                          strip.white = TRUE,
                          stringsAsFactors = FALSE)

# Get rid of pound/number/"hashtag" and anything follow it. Google geocoder doesn't get it and it's easier to just not mess with apartment numbers


evictions<-mutate(evictions, ADDRESS=gsub("#.*$","",ADDRESS) %>% gsub("  "," ",.) %>% sub(" Baltimore MD","",.) %>%
                       sub(" AVE .*$"," AVE",.) %>%
                       sub(" CT .*$"," CT",.) %>%
                       sub(" CR .*$"," CR",.) %>%
                       sub(" RD .*$"," RD",.) %>%
                       sub(" BLVD .*$"," RD",.))
evictions$ADDRESS <- ifelse(grepl(" ST .* ST ",evictions$ADDRESS),
                               regmatches(evictions$ADDRESS,regexpr("^.*ST[^(ST)]{,}", evictions$ADDRESS, perl=TRUE)),
                               sub(" ST .*$"," ST",evictions$ADDRESS))
evictions$ADDRESS <- ifelse(grepl(" DR .* DR ",evictions$ADDRESS),
                            regmatches(evictions$ADDRESS,regexpr("^.*ST[^(ST)]{,}", evictions$ADDRESS, perl=TRUE)),
                            sub(" DR .*$"," DR",evictions$ADDRESS))
# Add Baltimore MD to the end of the address
evictions$ADDRESS<-paste(evictions$ADDRESS,"Baltimore","MD")
in.cache <- filter(evictions,ADDRESS %in% address.cache$Address)
not.in.cache <- filter(evictions,!(ADDRESS %in% address.cache$Address))
# Apply the geocoding function to every address
geocode.results <- do.call(
  rbind.data.frame,
  lapply(not.in.cache$ADDRESS,
         bmoreGeocode,
         google.api.key))

# If this returns any addresses outside the Baltimore bbox, drop them. We tried...
bmore.coded<-filter(geocode.results,
                         between(Latitude,39.1963325151,39.3826883618) 
                         & between(Longitude,-76.7198786773,-76.5184626337) 
                    & Error.Status=="OK") 
wrong<-filter(geocode.results,
                    !(between(Latitude,39.1963325151,39.3826883618) 
                    & between(Longitude,-76.7198786773,-76.5184626337)
                    & Error.Status=="OK") )

# Write address cache
write.csv(unique(rbind(bmore.coded,address.cache)),
          cache.out,
          row.names = F)

# Join coordinates to main data, deduplicating on address first. At one point, an attempt with apartment numbers and without Baltimore MD was made and there were duplicate address matches with different coordinates
bmore.locations<-transmute(bmore.coded,
                           Address=sub(" Baltimore MD", "", Address),
                           Latitude,
                           Longitude
) %>% filter(!duplicated(Address))

evictions.ad <- mutate(not.in.cache,
                           Address=sub(" Baltimore MD", "", ADDRESS))

cleaned.coded <- inner_join(evictions.ad,
                            bmore.locations)
cleaned.already.coded<-inner_join(mutate(in.cache,Address=sub(" Baltimore MD", "", ADDRESS)),
                                  unique(transmute(address.cache,Address=sub(" Baltimore MD", "", Address),
                                            Latitude,
                                            Longitude)) %>% filter(!duplicated(Address)))
coded.full <- rbind(rbind(cleaned.coded,cleaned.already.coded),
                    filter(not.in.cache, ADDRESS %in% wrong$Address) %>% 
                      mutate(Latitude=NA,
                             Longitude=NA,
                             Address=NA))

write.csv(coded.full,output,row.names = F)
# Make a simple feature geometry 
sf.w.owners<-st_as_sf(filter(cleaned.coded,between(Latitude,39.1963325151,39.3826883618) & between(Longitude,-76.7198786773,-76.5184626337)), coords = c("Longitude","Latitude"))

# project to NAD83
st_crs(sf.2017) <- 4269

# Map it
evictions.map <- mapview(sf.w.owners,#sf.2017, 
                              xcol="Longitude", ycol="Latitude", 
                              cex=.5)

# Save it
mapshot(evictions.map,
        file = "Evctions 2015.htm")
