library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(RSelenium)

data_final = data.frame()
hotel_reviews =  data.frame()
invalid_re_v_u = c()
invalid_re_v_u_t = c()
invalid_h_name = c()

remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L)
remDr$open()

getReviews<- function(hotel_url){
  
  for(i in 1:length(hotel_url)){
    
    # if (!require(rvest)) {install.packages("rvest")}
    # if (!require(stringr)) {install.packages("stringr")}
    # if (!require(xml2)) {install.packages("xml2")}
    # if (!require(dplyr)) {install.packages("dplyr")}
    
    remDr$navigate(hotel_url[i])
    Sys.sleep(10)
    remDr$findElement("css selector",".ulBlueLinks")$clickElement()
    ps <- remDr$getPageSource()
    webpage <- read_html(ps[[1]])
    
    #webpage<-read_html(hotel_url[i])
    review_xml <- html_nodes(webpage,'.reviewItemInline~ .prw_reviews_text_summary_hsx .partial_entry')
    review_title_xml <-html_nodes(webpage,'.noQuotes')
    review_rating_xml <-html_nodes(webpage,'.reviewItemInline') %>% html_node("span") %>% str_extract("[[:digit:]]+")%>%as.integer()
    

    hotel_name_xml <- html_nodes(webpage,'#HEADING')
    hotel_class_xml <- html_nodes(webpage,'div.starRating.detailListItem') %>% html_children()
    
    regexp <- "[[:digit:]]+" # regex for picking the hotel class as a number
    reviews <- html_text(review_xml)
    review_title <- html_text(review_title_xml)
    review_rating <- review_rating_xml %>% gsub(" of 5 stars", "", .) %>% as.integer()
    hotel_name <- html_text(hotel_name_xml)
    regexp <- "[[:digit:]]+" # regex for picking the hotel class as a number
    hotel_class <- html_text(hotel_class_xml)[2] %>% str_extract(regexp) %>% as.integer()
    
    if(hotel_class %in% c(1,2,3,4,5)){
      hc = rep(hotel_class,length(review_title))
    } else{
      hc=(rep(NA, length(review_title)))
    }
    
    
    if(length(reviews)==length(review_title)){
      temp<-data.frame(HotelName=rep(hotel_name,length(review_title)),
                       HotelClass= hc,
                       ReviewRating =review_rating,
                       Title=review_title,
                       Text=reviews)
      hotel_reviews <-rbind(hotel_reviews,temp)
    }
    
  }
  return(hotel_reviews)
}


#***********************#

#***** Get Hotels URLs**********#

hotels_Goa <- read_html('https://www.tripadvisor.in/Hotels-g297604-Goa-Hotels.html')
baseurl = hotels_Goa  %>% 
  html_nodes('a.review_count') %>% 
  html_attr("href") %>%
  xml2::url_absolute("https://www.tripadvisor.in")




for(i in 1:length(baseurl))
{
  newurl <- baseurl[i]
  webpage<-read_html(baseurl[i])
  page_xml <- html_nodes(webpage,'.pageNum')
  page_num <- html_text(page_xml)
  temp <- getReviews(newurl)
  data_final <- rbind(data_final,temp)
  n<-max(as.integer(page_num))
  
  if(!is.infinite(n)){
    for(j in 1:5){
      x<-paste0("Reviews-","or",as.character(5*j),"-")
      newurl1 <- gsub("(Reviews-)",x,newurl)
      temp <- getReviews(newurl1)
      data_final <- rbind(data_final,temp)
    }
  }
}

data_final1 <- data_final %>% mutate(ReviewRating = ReviewRating/10)
