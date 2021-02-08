library(dplyr)
library(jsonlite)
library(RCurl)
library(leaflet)
library(sp)
library(rgdal)
library(sf)
# install.packages("geosphere")
# library(geosphere)

# Put a google geocoding API key here 
google.key <- ""

# The max cluster size
Cluster_Sizes = 10

cases.in <- "/Housing/Case search/Cases_2.7.2021+60Days.csv"

# geocoding function (calsl the google API)
geocode <- function(address1="",
                    city="",
                    zip="",
                    API)
{
  
  
  # Convert spaces in the address into plusses
  prefix <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  address<-trimws(paste(address1, city, zip)) 
  address.parsed <- gsub("\\s",
                         "\\+",
                         address,
                         perl = TRUE)
  
  # Retrieve the results
  geocode.results <- fromJSON(
    getURL(paste(prefix,
                 address.parsed,
                 "&bounds=39.3719,-76.7122|39.1977,-76.5294&key=",
                 API,
                 sep = "")
    )
  )
  
  # Check the status of the query
  status <- geocode.results$status
  
  # If the query worked correctly, extract latitute and longitude
  if(status == "OK"){
    components <- geocode.results$results$address_components[[1]]
    components$types<-unlist(lapply(components$types, `[[`, 1))
    zip <- components$short_name[components$types=="postal_code"]
    lat <- geocode.results$results$geometry$location$lat[[1]]
    long <- geocode.results$results$geometry$location$lng[[1]]
    geo.address<-sub(", USA","",geocode.results$results$formatted_address[[1]])
    # Otherwise, try to code again dropping city name and using ZIP code only
  }else{
    
    address.nocity.parsed <- gsub("\\s",
                                  "\\+",
                                  address1,
                                  perl = TRUE)
    
    geocode.results.nocity <- fromJSON(
      getURL(paste(prefix,
                   address.nocity.parsed,
                   "&bounds=39.3719,-76.7122|39.1977,-76.5294&key=",
                   API,
                   sep = "")
      )
    )
    
    status.nocity <- geocode.results.nocity$status
    
    if(status.nocity == "OK")
    {
      components <- geocode.results$results$address_components[[1]]
      components$types<-unlist(lapply(components$types, `[[`, 1))
      zip <- components$short_name[components$types=="postal_code"]
      lat <- geocode.results.nocity$results$geometry$location$lat[[1]];
      long <- geocode.results.nocity$results$geometry$location$lng[[1]];
      status <- status.nocity;
      geo.address<-sub(", USA","",geocode.results.nocity$results$formatted_address[[1]])
    }else {
      zip <- NA
      lat <- NA;
      long <- NA;
      geo.address<-NA
    }
    
  }
  
  # Combine results into a list
  geocode.outcome <- list(address, lat, long, zip, status, geo.address)
  names(geocode.outcome) <- c("Address", 
                              "Latitude", 
                              "Longitude", 
                              "Postal_Code",
                              "Error.Status",
                              "GEO_Address")
  
  # Return
  return(unlist(geocode.outcome))
}



# Read the docket source data
cases <- cases.in %>% 
  read.csv(stringsAsFactors = F, strip.white = T)

# convert date to date
cases <- cases %>% mutate(date = date %>% as.Date())

# clean addresses at least a little (remove apartment/unit numbers. Most importantly remove #)
addresses <- trimws(cases$address_1)
addresses <- gsub("[0-9]+$","",addresses) %>% trimws 
addresses <- gsub("#$|APT\\.{0,1}$|STE\\.{0,1}$|RO{0,2}M\\.{0,1}$","",addresses) %>% trimws
addresses <- gsub("#","",addresses) %>% trimws

# Stick that "clean" address field to the main dataframe
cases$Address <- addresses

# duplicates??
casedups <- cases %>% filter(duplicated(cases))

# geocode the addresses
coords <- lapply(addresses, geocode, city = "Baltimore", API=google.key) 
# goodtogo <- lapply(coords, is.list )%>% unlist
# addresses[!goodtogo]

Geocoded <- coords %>% lapply(function(x)as.data.frame(t(unlist(x)))) %>% plyr::rbind.fill()

geocoded <- Geocoded %>% filter(!duplicated(Geocoded))

# Cache the addresses to potentially add to a reference database 
geocoded %>% filter(Error.Status == "OK") %>% write.csv(paste0("/Housing/Case search/address cache/Addresses cached ",Sys.Date(),".csv"), row.names = F)

# remove duplicates. This should be moved upstream of the geocoding                              
dups <- Geocoded  %>% filter(duplicated(Geocoded))

# Check for errors
geocoded$Error.Status %>% table

# Stick the original address and date to the geocoded dataset                              
geocoded <- cbind(geocoded, cases %>% select(address_1, date))

# Results that don't make any sense. We'll output these later but for now remove them
notgood <- geocoded %>% filter(!grepl(", MD",GEO_Address))
                              
geocoded <- geocoded %>% filter(grepl(", MD",GEO_Address))

# K Means Clustering (start with 2 clusters)
groups <- kmeans(geocoded[,c("Longitude","Latitude")], 2)

geocoded$cluster <- as.character(groups$cluster)

# check for clusters that are too big                              
clustcounts <- table(geocoded$cluster)
needs.split <- names(clustcounts)[clustcounts>Cluster_Sizes]
done <- length(needs.split) == 0 

#Split the clusters that are too big in 2. Iterate until all the groups are small enough                              
while(!done){

for (C in needs.split) {
  splitme <- geocoded %>% filter(cluster == C)
  
  groups <- kmeans(splitme[,c("Longitude","Latitude")], 2)
  
  geocoded$cluster[geocoded$cluster == C] <-   paste(C,groups$cluster, sep = "-")
  
}
  clustcounts <- table(geocoded$cluster)
  needs.split <- names(clustcounts)[clustcounts>Cluster_Sizes]
  done <- length(needs.split) == 0 
  
}

# Name the groups with an integer (1:number of unique groups)
geocoded$Group <- unlist(lapply(geocoded$cluster, function(x)which(names(clustcounts)==x)))

# convert to sf                                
addresses_sf <- geocoded %>% st_as_sf(coords = c("Longitude","Latitude"))
#table(addresses_sf$Group)


# extremely inelegant way to make a big qualitative color palette 
coolpal <- structure(list(r = c(0.374, 0.374, 0.051, 0.096, 0.876, 0.415, 
                     0.596, 0.724, 0.847, 0.588, 0.481, 0.142, 0.819, 0.91, 0.969, 
                     0.887, 0.432, 0.927, 0.381, 0.04, 0.374, 0.381, 0.819, 0.91, 0.847, 0.876, 0.887, .569), g = c(0.183, 0.905, 0.662, 
                                                              0.706, 0.461, 0.845, 0.07, 0.101, 0.434, 0.885, 0.366, 0.075, 
                                                              0.737, 0.722, 0.012, 0.536, 0.967, 0.125, 0.646, 0.898, .054, .894, .675, .378, .984, .285, .369, .456), b = c(0.528, 
                                                                                                                             0.337, 0.028, 0.898, 0.628, 0.286, 0.523, 0.673, 0.937, 0.604, 
                                                                                                                             0.337, 0.276, 0.658, 0.979, 0.451, 0.123, 0.446, 0.332, 0.656, 
                                                                                                                             0.798, .852, .963, .741, .123, .456, .789, .321, .654, .987)), .Names = c("r", "g", "b"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                     -20L))
cool=rgb(coolpal$r, coolpal$g, coolpal$b)
cool_pal <- colorFactor(cool, addresses_sf$Group)
                                
# Map it                                 
map <- leaflet() %>%
  addTiles(group = "Google Satellite Imagery", urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
  addTiles(group = "Google Street Map", urlTemplate = "https://mt0.google.com/vt/lyrs=m&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google') %>%
  # addProviderTiles(providers$Hydda.Full) %>%
  # addTiles() %>%
  addCircleMarkers(group="Cases",
                   data = addresses_sf ,
                    fillColor = ~cool_pal(Group),
                   # fillColor = "red",
                   fillOpacity = .5,
                   color = 'white',
                   popup = ~paste(Group)
  )%>%
  addLayersControl(
    baseGroups = c("Google Street Map", "Google Satellite Imagery"),
    options = layersControlOptions(collapsed = FALSE)
  )
# map

# CSV outputs
geocoded %>% write.csv(paste0("/Housing/Case search/Grouped Outputs/geocoded and grouped ",Sys.Date(),".csv"), row.names = F)
dups %>% write.csv(paste0("/Housing/Case search/Grouped Outputs/duplicates (removed) ",Sys.Date(),".csv"), row.names = F)
notgood %>% write.csv(paste0("/Housing/Case search/Grouped Outputs/Failed to Geocode ",Sys.Date(),".csv"), row.names = F)

