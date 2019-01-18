
source("C:/Bmore City/Bmore code/R_utilities.R")

required.packages(c("rgdal",
                    "ggplot2",
                    "dplyr",
                    "RSelenium",
                    "docker",
                    "httr",
                    "rvest",
                    "beepr",
                    "stringr"))

# parcels<-read_sf("C:/Bmore City/Shapefiles/BACI Parcels/BACI.shp")
# res<-parcels %>%
#   filter(DESCLU %in% c("Apartments",
#                        "Commercial Condominium",
#                        "Commercial Residential",
#                        "Residential",
#                        "Residential Commercial", 
#                        "Residential Condominium"))
well<-inner_join(mutate(cleaned.coded, ADDRESS=word(ADDRESS,2)),mutate(res, ADDRESS=word(ADDRESS,2)))
search.owner<-mutate(cleaned.coded, ADDRESS=sub(" Baltimore MD","",ADDRESS))
driver<- rsDriver(browser=c("chrome"))
remDr <- driver[["client"]]
get.owner<-function(n){
  
  try({
   
   
    
    
    remDr$navigate(mapurl)
    
    search.input<-remDr$findElement(using = 'css',  "[id = 'search_input']")
    search.input$sendKeysToElement(list(search.owner[n,"ADDRESS"]))
    go.map <- remDr$findElement(using = 'css',"[title='Search']")
    go.map$clickElement()
    Sys.sleep(2)
    dot <- remDr$findElement(using = 'css',"[id='map_graphics_layer']")
    dot$clickElement()
    Sys.sleep(2)
    if(grepl("No information available.",read_html(remDr$getPageSource()[[1]]))){
      return(cbind.data.frame(NA,
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
                              NA))}else{
    sdat<-read_html(remDr$getPageSource()[[1]]) %>%
      html_nodes( css = "li a")
    sdat.ref<-xml_attr(sdat[[5]],"href")
    remDr$navigate(sdat.ref)
    Sys.sleep(2)
    if(grepl("There was a problem",read_html(remDr$getPageSource()[[1]]))){ return(cbind.data.frame(NA,
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
                                                                                                    NA))}else{
    
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

    
    return(cbind.data.frame(Owner_1,
             Owner_2, 
             Owner_Ad, 
             Value, 
             Price.1, 
             Price.2,
             Price.3, 
             Seller_1, 
             Seller_2, 
             Seller_3,
             Sale_date_1,
             Sale_date_2,
             Sale_date_3)) }
    }
    
    })
  
}
new.data<-lapply(1:nrow(search.owner), get.owner)
