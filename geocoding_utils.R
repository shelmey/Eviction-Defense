# Update 1/11/2017 -- Modified Address concatenation function to replace ampersands
# with &s
#
# Update 2/7/2017 -- Major flaw. Wasn't pulling the proper location, was pulling location of "viewpoint"
# These were close to but NOT the proper locations
# Has been changed. Will have to nuke the cache, though, and start over.
#
# Also updated concatenation function to not append "NA"s. Blanks, instead
#
# Update 2/15/2017
# In the event that a call fails, make a second attempt dropping the city and using the 
# zip code instead.
#
# Update 3/28/2017
# Added function to assign Census Tract to geocoded data
# Note: we know that this method is not terribly accurate.

# Function calls the Google API for an address
# Requires the address plus a Google project API
bmoreGeocode <- function(address,
                       API
                            )
{
  
  # Confirm that required packages have been loaded
  if(!all(c("RCurl", "rjson") %in% (.packages())))
  {
    stop("Function requires the packages RCurl and rjson to execute. Please load these and retry.")
  }
  
  # Confirm that the API has been loaded
  if(missing(API))
  {
    stop("Please enter the Google Geocoding API and try again.")
  }

  # Convert spaces in the address into plusses
  prefix <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  address.parsed <- gsub("\\s",
                         "\\+",
                         address,
                         perl = TRUE)
  
  # Retrieve the results
  geocode.results <- fromJSON(
    getURL(paste(prefix,
                 address.parsed,
                 "&bounds=39.1963325151,-76.7198786773,39.3826883618,-76.5184626337&key=",
                 
                  API,
                 sep = "")
    )
  )
  
  # Check the status of the query
  status <- geocode.results$status
  
  # If the query worked correctly, extract latitute and longitude
  if(status == "OK")
  {
    lat <- geocode.results$results[[1]]$geometry$location$lat
    long <- geocode.results$results[[1]]$geometry$location$lng
    geo.address <- geocode.results$results[[1]]$formatted_address
  }else{
      lat <- NA;
      long <- NA;
      geo.address <- NA
    }
    

  
  # Combine results into a list
  geocode.outcome <- list(address, geo.address, lat, long, status)
  names(geocode.outcome) <- c("Address",
                              "GEO_address",
                              "Latitude", 
                              "Longitude", 
                              "Error.Status"
  )
  
  # Return
  return(geocode.outcome)
  
}


# keyless

atlGeocodekeyless <- function(address
)
{
  
  # Confirm that required packages have been loaded
  if(!all(c("RCurl", "rjson") %in% (.packages())))
  {
    stop("Function requires the packages RCurl and rjson to execute. Please load these and retry.")
  }
  
 
  
  # Convert spaces in the address into plusses
  prefix <- "https://maps.googleapis.com/maps/api/geocode/json?address="
  address.parsed <- gsub("\\s",
                         "\\+",
                         address,
                         perl = TRUE)
  
  # Retrieve the results
  geocode.results <- fromJSON(
    getURL(paste(prefix,
                 address.parsed,
                 "&bounds=39.1963325151,-76.7198786773,39.3826883618,-76.5184626337&key=",
                 
                 sep = "")
    )
  )
  
  # Check the status of the query
  status <- geocode.results$status
  
  # If the query worked correctly, extract latitute and longitude
  if(status == "OK")
  {
    lat <- geocode.results$results[[1]]$geometry$location$lat
    long <- geocode.results$results[[1]]$geometry$location$lng
  }
  # Otherwise, try to code again dropping city name and using ZIP code only
  else{
    address.nocity <- gsub("(\\N+),(\\N+)(\\d{5})(\\N+)",
                           "\\1, \\3\\4",
                           address,
                           perl = TRUE)
    address.nocity.parsed <- gsub("\\s",
                                  "\\+",
                                  address.nocity,
                                  perl = TRUE)
    
    geocode.results.nocity <- fromJSON(
      getURL(paste(prefix,
                   address.nocity.parsed,
                   "&bounds=39.1963325151,-76.7198786773,39.3826883618,-76.5184626337&key=",
                   
                   sep = "")
      )
    )
    
    status.nocity <- geocode.results.nocity$status
    
    if(status.nocity == "OK")
    {
      lat <- geocode.results.nocity$results[[1]]$geometry$location$lat;
      long <- geocode.results.nocity$results[[1]]$geometry$location$lng;
      status <- status.nocity
    }
    else {
      lat <- NA;
      long <- NA
    }
    
  }
  
  # Combine results into a list
  geocode.outcome <- list(address, lat, long, status)
  names(geocode.outcome) <- c("Address", 
                              "Latitude", 
                              "Longitude", 
                              "Error.Status"
  )
  
  # Return
  return(geocode.outcome)
  
}



# Function concatenates addresses in the format expected by the geocoding repository
concatenate.address <- function( address,
                                 city,
                                 state,
                                 zipcode)
{
  address.concatenated <- paste(address,
                                ",",
                                city,
                                state,
                                zipcode)
  
  address.concatenated <- gsub("\\b&\\b",
                               "and",
                               address.concatenated,
                               perl = TRUE)
  
  address.concatenated <- gsub("\\B&\\B",
                               " and ",
                               address.concatenated,
                               perl = TRUE)
  
  address.concatenated <- gsub("\\b(NA)\\b",
                               "",
                               address.concatenated,
                               perl = TRUE)
  
  address.concatenated <- gsub("\\s,\\s{3}",
                               "",
                               address.concatenated,
                               perl = TRUE)
  
  return(address.concatenated)
}

# Assign census tract to a lat/long pair

# Function to determine census tract using Census tract shapefile
# You first need to import a shapefile. Example:
# ctract2010 <- readOGR(census.2010.dsn,
#                       census.2010.layer)
# (Uses the rgdal library)

assign.census.tract <- function(in.file,
                                in.ctract,
                                sf.variable)
{
  # Split into records that can and cannot have a tract assigned
  in.file.has.coords <- filter(in.file,
                               !is.na(Latitude) &
                                 !is.na(Longitude))
  
  in.file.no.coords <- filter(in.file,
                              is.na(Latitude) |
                                is.na(Longitude))
  
  # Assign GeoID
  coordinates(in.file.has.coords) <- c("Longitude", "Latitude")
  proj4string(in.file.has.coords) <- proj4string(in.ctract)
  
  sf.extract <- over(in.file.has.coords,
                     in.ctract)[,sf.variable]
  
  return(bind_rows(
    cbind(
      as.data.frame(in.file.has.coords),
      sf.extract),
    in.file.no.coords
  )
  )
}