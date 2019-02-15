library(sf)

baci.in <- "C:/Bmore City/Shapefiles/BACI Parcels/BACI.shp"

baci <- read_sf(baci.in)

st.types <- read.csv("C:/utilities/street types.csv", strip.white = T, stringsAsFactors = F)

address.clean <- function(x){
  
  y <- gsub("\\.","",toupper(x))
  
  z <- gsub(" [NSEW]{1,2} "," ",y) 
which.type <- function(type){
  grepl(paste0(" ",type," .*$"),z)
}
sts<-unique(c(st.types$Postal.Service.Standard.Suffix.Abbreviation,st.types$Commonly.Used.Street.Suffix.or.Abbreviation, st.types$Primary.Street.Suffix.Name))
  type<-sts[which(unlist(lapply(sts,which.type)))]
  z<- sub(paste0(" ",type," .*$"),paste0(" ",type),z)  
   # lapply(st.types$Postal.Service.Standard.Suffix.Abbreviation, trunc.ad)
z <- ifelse(grepl(" ST .* ST ",z),
                            regmatches(z,regexpr("^.*ST[^(ST)]{,}", z, perl=TRUE)),
                            sub(" ST .*$"," ST",z))
z <- ifelse(grepl(" DR .* DR ",z),
                            regmatches(z,regexpr("^.*ST[^(ST)]{,}", z, perl=TRUE)),
                            sub(" DR .*$"," DR",z))
  
  ifelse(length(z)==0,"",z)
  
}

evictions.cleaning.3$ADDRESS <- unlist(lapply(evictions.cleaning.3$ADDRESS,address.clean))
# baci$ADDRESS <- unlist(lapply(baci$ADDRESS,address.clean))
