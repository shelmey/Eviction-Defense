library(sf)
library(zoo)
baci.in <- "C:/Bmore City/Shapefiles/BACI Parcels/BACI.shp"

baci <- read_sf(baci.in)

st.types <- read.csv("C:/utilities/street types.csv", strip.white = T, stringsAsFactors = F)
st.types<- within(st.types,{
  Primary.Street.Suffix.Name <- ifelse(Primary.Street.Suffix.Name=="",NA,Primary.Street.Suffix.Name)
  Commonly.Used.Street.Suffix.or.Abbreviation<- ifelse(Commonly.Used.Street.Suffix.or.Abbreviation=="",NA,Commonly.Used.Street.Suffix.or.Abbreviation)
  Primary.Street.Suffix.Name <- na.locf(Primary.Street.Suffix.Name)
  Commonly.Used.Street.Suffix.or.Abbreviation <- na.locf(Commonly.Used.Street.Suffix.or.Abbreviation)
  Postal.Service.Standard.Suffix.Abbreviation<- ifelse(Postal.Service.Standard.Suffix.Abbreviation=="",NA,Postal.Service.Standard.Suffix.Abbreviation)
  Postal.Service.Standard.Suffix.Abbreviation <- na.locf(Postal.Service.Standard.Suffix.Abbreviation)
})

sts<-unique(c(st.types$Postal.Service.Standard.Suffix.Abbreviation,st.types$Commonly.Used.Street.Suffix.or.Abbreviation, st.types$Primary.Street.Suffix.Name))
streets<-unique(rbind(select(st.types,standard=Postal.Service.Standard.Suffix.Abbreviation,actual=Primary.Street.Suffix.Name),
select(st.types,standard=Postal.Service.Standard.Suffix.Abbreviation,actual=Commonly.Used.Street.Suffix.or.Abbreviation)))
same<-cbind.data.frame(unique(streets$standard),unique(streets$standard))
names(same) <- c("standard","actual")
streets <- unique(rbind(streets,same))
streets<-filter(streets,!(standard=="MDWS" & actual=="MDW"))
address.clean <- function(x){
  
  y <- gsub("\\.","",toupper(x))
  
  z <- gsub(" [NSEW]{1,2} "," ",y) 
which.type <- function(type){
  grepl(paste0(" ",type," .*$"),z)
}

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
evictions.cleaning.3$end<-gsub("^.* ","",evictions.cleaning.3$ADDRESS)
ev.ad.clean <- left_join(evictions.cleaning.3,dplyr::rename(streets,end=actual))

