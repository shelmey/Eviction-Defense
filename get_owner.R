infile <- "C:/Bmore City/Eviction Defense/Outputs/coded_evictions_2017.csv"


source("C:/Bmore City/Bmore code/R_utilities.R")

library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(RSelenium)
library(docker)
library(httr)
library(rvest)
library(beepr)
library(stringr)

cleaned.coded <- read.csv(infile,
                          stringsAsFactors = FALSE,
                          strip.white = TRUE)

search.owner<-mutate(cleaned.coded, ADDRESS=sub(" Baltimore MD","",ADDRESS) %>%
                       sub(" AVE .*$"," AVE",.) %>%
                       sub(" RD .*$"," RD",.) %>%
                       sub(" BLVD .*$"," RD",.))

search.owner$ADDRESS <- ifelse(grepl(" ST .* ST ",search.owner$ADDRESS),
                               regmatches(search.owner$ADDRESS,regexpr("^.*ST[^(ST)]{,}", search.owner$ADDRESS, perl=TRUE)),
                               sub(" ST .*$"," ST",search.owner$ADDRESS))
# Initialize row for failures
replacement<-cbind.data.frame(NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,NA)

names(replacement)<-c("Owner_1", "Owner_2", "Owner_Ad", "Value", "Price.1", "Price.2", "Price.3", "Seller_1", "Seller_2", "Seller_3", "Sale_date_1", "Sale_date_2", "Sale_date_3","Note")

# Initialize row for failures because of no information
replacement.1<-cbind.data.frame(NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,"No information available")

# Initialize row for failures because the address wasn't found
names(replacement.1)<-c("Owner_1", "Owner_2", "Owner_Ad", "Value", "Price.1", "Price.2", "Price.3", "Seller_1", "Seller_2", "Seller_3", "Sale_date_1", "Sale_date_2", "Sale_date_3","Note")
replacement.2<-cbind.data.frame(NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,
                              NA, 
                              NA, 
                              NA, 
                              NA,"Bad address")

names(replacement.2)<-c("Owner_1", "Owner_2", "Owner_Ad", "Value", "Price.1", "Price.2", "Price.3", "Seller_1", "Seller_2", "Seller_3", "Sale_date_1", "Sale_date_2", "Sale_date_3","Note")

# Code map/SDAT Scraping function
get.owner<-function(n){
  
  try({
   
   
    
    
    remDr$navigate(mapurl)
    
    search.input<-remDr$findElement(using = 'css',  "[id = 'search_input']")
    search.input$sendKeysToElement(list(search.owner[n,"ADDRESS"]))
    go.map <- remDr$findElement(using = 'css',"[title='Search']")
    go.map$clickElement()
    Sys.sleep(2)
    if(grepl("There were no results found",read_html(remDr$getPageSource()[[1]]))){
      return(replacement.2)}else{
    dot <- remDr$findElement(using = 'css',"[id='map_graphics_layer']")
    dot$clickElement()
    Sys.sleep(2)
    if(grepl("Searching...",read_html(remDr$getPageSource()[[1]]))){Sys.sleep(1)}
    if(grepl("Searching...",read_html(remDr$getPageSource()[[1]]))){Sys.sleep(1)}
    if(grepl("Searching...",read_html(remDr$getPageSource()[[1]]))){Sys.sleep(1)}
    if(grepl("Searching...",read_html(remDr$getPageSource()[[1]]))){Sys.sleep(1)}
    if(grepl("Searching...",read_html(remDr$getPageSource()[[1]]))){Sys.sleep(1)}
    if(grepl("No information available.",read_html(remDr$getPageSource()[[1]]))){
      return(replacement.1)}else{
    sdat<-read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes( css = "li a")
    sdat.ref<-xml_attr(sdat[[5]],"href")
    remDr$navigate(sdat.ref)
    Sys.sleep(2)
    if(grepl("There was a problem",read_html(remDr$getPageSource()[[1]]))){ return(replacement)}else{
    
    # Ok now we're on the page to scrape
    Owner1<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_lblOwnerName_0']")
    
    Owner2<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_lblOwnerName2_0']")
    Owner.Address<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_lblMailingAddress_0']")
    Owner_1<-Owner1$getElementText()[[1]]
    Owner_2<-Owner2$getElementText()[[1]]
    Owner_Ad<-Owner.Address$getElementText()[[1]]
    
    value<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_lblBaseTotalNow_0']")
    Value<-value$getElementText()[[1]]
    
    Sale.Price.1<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label40_0']")
    Price.1<-Sale.Price.1$getElementText()[[1]]
    
    Sale.Price.2<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label46_0']")
    Price.2<-Sale.Price.2$getElementText()[[1]]
    
    Sale.Price.3<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label52_0']")
    Price.3<-Sale.Price.3$getElementText()[[1]]
    
    Seller.1<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label38_0']")
    Seller_1<-Seller.1$getElementText()[[1]]
    
    Seller.2<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label44_0']")
    Seller_2<-Seller.2$getElementText()[[1]]
    
    Seller.3<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label50_0']")
    Seller_3<-Seller.3$getElementText()[[1]]
    
    Sale.Date.1<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label39_0']")
    Sale_date_1<-Sale.Date.1$getElementText()[[1]]
    
    Sale.Date.2<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label45_0']")
    Sale_date_2<-Sale.Date.2$getElementText()[[1]]
    
    Sale.Date.3<-remDr$findElement(using = 'css',  "[id = 'MainContent_MainContent_cphMainContentArea_ucSearchType_wzrdRealPropertySearch_ucDetailsSearch_dlstDetaisSearch_Label51_0']")
    Sale_date_3<-Sale.Date.3$getElementText()[[1]]
    
    new.row<-cbind.data.frame(Owner_1, Owner_2, Owner_Ad, Value, Price.1, Price.2, Price.3, Seller_1, Seller_2, Seller_3, Sale_date_1, Sale_date_2, Sale_date_3)
    
    return(new.row) }
    }
    
    }
    })
  
}

# Start RSelenium driver
driver<- rsDriver(browser=c("chrome"))

# Open window
remDr <- driver[["client"]]

# Apply the function to every row number of input dataset
new.data.3<-lapply(1:nrow(search.owner), get.owner)

lengths<-unlist(lapply(new.data,length))



classes<-unlist(lapply(new.data,class))
errors<-unique(new.data[classes!="data.frame"])
class.index<-which(classes=="data.frame")
df.data<-new.data[class.index] %>%
rbind.fill()
# df.data<-df.data[,1:13]
df.data$id <- class.index

fail.index<-which(classes!="data.frame")
initial<-as.data.frame(matrix(nrow = 4277,ncol = 13))
names(initial)<-the.names
initial$id<-fail.index

all.new.data<-rbind(df.data,initial) %>% arrange(id)
# write.csv(all.new.data,"C:/Bmore City/Eviction Defense/Outputs/owners_2017.csv")


# Join to original dataset

evictors<-cbind(evictions,all.new.data)


# Output
write.csv(evictors,"C:/Bmore City/Eviction Defense/Outputs/evctions_w_owner_2017.csv")


 