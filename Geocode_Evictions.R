
# Load program functions
source("geogoding_utils.R")

# Load required packages
library(dplyr)
library(lubridate)
library(mapview)
library(sf)
library(htmlwidgets)


# Parameters
# Clean dataset
in.file<-"C:/Bmore City/Eviction Defense/Source data/cleaning.csv"

# Dates for filtering the dataset to geocode
start.date <- as.Date('2016-12-31')
end.date <- as.Date('2019-01-01')

cache.out <- paste0("addresses",today(),".csv")

# Google API key
google.api.key<-""

evictions<-read.csv(in.file,
                    strip.white = TRUE,
                    stringsAsFactors = FALSE) %>%
  filter(Date>start.date & Date<end.date)

# Get rid of pound/number/"hashtag" and anything follow it. Google geocoder doesn't get it and it's easier to just not mess with apartment numbers

evictions$ADDRESS <- gsub("#.*$","",evictions$ADDRESS) %>% gsub("  "," ",.)

# Add Baltimore MD to the end of the address
evictions$ADDRESS<-paste(evictions$ADDRESS,"Baltimore","MD")

# Apply the geocoding function to every address
geocode.results <- do.call(
  rbind.data.frame,
  lapply(evictions$ADDRESS,
         bmoreGeocode,
         google.api.key))

# If this returns any addresses outside the Baltimore bbox, drop them. We tried...
bmore.coded<-filter(geocode.results,
                         between(Latitude,39.1963325151,39.3826883618) 
                         & between(Longitude,-76.7198786773,-76.5184626337)) 
# Write address cache
write.csv(bmore.coded,
          cache.out,
          row.names = F)

# Join coordinates to main data, deduplicating on address first. At one point, an attempt with apartment numbers and without Baltimore MD was made and there were duplicate address matches with different coordinates
bmore.locations<-transmute(bmore.coded,
                           Address=sub(" Baltimore MD", "", Address),
                           Latitude,
                           Longitude
) %>% filter(!duplicated(Address))

evictions.ad <- mutate(evictions,
                           Address=sub(" Baltimore MD", "", ADDRESS))

cleaned.coded <- inner_join(evictions.ad,
                            bmore.locations)


# Make a simple feature geometry 
sf.2017<-st_as_sf(filter(cleaned.coded,between(Latitude,39.1963325151,39.3826883618) & between(Longitude,-76.7198786773,-76.5184626337)), coords = c("Longitude","Latitude"))

# project to NAD83
st_crs(sf.2017) <- 4269

# Map it
evictions.2017.map <- mapview(sf.2017, xcol="Longitude", ycol="Latitude", cex=.3)

# Save it
mapshot(evictions.2017.map,
        file = mapout)
