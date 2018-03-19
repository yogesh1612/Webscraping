library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(RSelenium)
library(rvest)
library(stringr)
#********** Create directory for data storage *******#
mainDir <-"D:"
subDir <-"HotelReviews"
filepath<- file.path(mainDir, subDir)
dir.create(filepath,showWarnings = FALSE)
setwd(filepath)
#****************************************************#


#********** Create Temporary Datastructures**********#
data_final = data.frame()
hotel_reviews =  data.frame()
invalid_re_v_u = c()
invalid_re_v_u_t = c()
invalid_h_name = c()
output = list()
#****************************************************#


#****************Connect to selenium server**********#
remDr <- remoteDriver(remoteServerAddr = "192.168.99.100",port = 4445L)
remDr$open()
remDr$maxWindowSize()
# remDr$closeall()

#***************************************************#


#******************** Get revies from URL *********#
getReviews<- function(hotel_url){

    for(i in 1:length(hotel_url)){
   # hotel_url ="https://www.tripadvisor.in/Hotel_Review-g306992-d1380556-Reviews-The_Banyan_Soul-Anjuna_Bardez_North_Goa_District_Goa.html#REVIEWS"
    cat("HotelUrl:",hotel_url)
    tryCatch({suppressMessages({
      remDr$navigate(hotel_url)
      Sys.sleep(10)
      remDr$findElements("css selector",".ulBlueLinks")$clickElement()
     })},error = function(e){NA_character_})
    
    #ps <- remDr$getPageSource()
    #webpage <- read_html(ps[[1]])
    
    webpage<-read_html(hotel_url[i])
    
    review_xml <- html_nodes(webpage,'.reviewItemInline~ .prw_reviews_text_summary_hsx .partial_entry')
    #review_xml <- html_nodes(webpage,'.partial_entry')
    review_title_xml <-html_nodes(webpage,'.noQuotes')
    review_rating <-html_nodes(webpage,'.reviewItemInline') %>% html_node("span") %>% str_extract("[[:digit:]]+")%>%as.integer()
    
    
    hotel_name_xml <- html_nodes(webpage,'#HEADING')
    hotel_class_xml <- html_nodes(webpage,'div.starRating.detailListItem') %>% html_children()
    
    regexp <- "[[:digit:]]+" # regex for picking the hotel class as a number
    reviews <- html_text(review_xml)
    cat("reviews length",length(reviews))
    review_title <- html_text(review_title_xml)
    #review_rating <- review_rating_xml %>% gsub(" of 5 stars", "", .) %>% as.integer()
    
    hotel_name <- html_text(hotel_name_xml)
    #regexp <- "[[:digit:]]+" # regex for picking the hotel class as a number
    hotel_class <- html_text(hotel_class_xml)[2] %>% str_extract(regexp) %>% as.integer()
    
    if(hotel_class %in% c(1,2,3,4,5)){
      hc = rep(hotel_class,length(review_title))
    } else{
      hc=(rep(NA, length(review_title)))
    }
    if(length(reviews)==0){next}
    
    if(length(reviews)==length(review_title)){
      tempdf <- data.frame()
      tempdf<-data.frame(HotelName=rep(hotel_name,length(review_title)),
                       HotelClass= hc,
                       ReviewRating =review_rating,
                       Title=review_title,
                       Text=reviews,stringsAsFactors = FALSE)
      hotel_reviews <-rbind(hotel_reviews,tempdf)
      output <- list(hotel_name,hotel_reviews)
    }
    return(output)
}
}

#***********************#

#***** RUN FROM HERE *********#

hotels_Goa <- read_html('https://www.tripadvisor.in/Hotels-g297604-Goa-Hotels.html')
baseurl = hotels_Goa  %>% 
  html_nodes('a.review_count') %>% 
  html_attr("href") %>%
  xml2::url_absolute("https://www.tripadvisor.in")



for(i in 1:length(baseurl))
{
  
  cat("Extracting Hotel Number:",i,"out of",length(baseurl),"\n")
  newurl <- baseurl[i]
  webpage<-read_html(baseurl[i])
  page_xml <- html_nodes(webpage,'.pageNum')
  page_num <- html_text(page_xml)
  temp <- getReviews(newurl)
  #temp <- output
  data_final <- rbind(data_final,temp[[2]])
  n<-max(as.integer(page_num))
  
  if(!is.infinite(n)){
    for(j in 1:5){
      x<-paste0("Reviews-","or",as.character(5*j),"-")
      newurl1 <- gsub("(Reviews-)",x,newurl)
      #print(newurl1)
      #cat("Review Page Url :",newurl,"\n")
      temp <- getReviews(newurl1)
      data_final <- rbind(data_final,temp[[2]])
    }
  }
  cat("Writing file",temp[[1]],".csv")
  filename <-paste0(gsub(" ","",temp[[1]]),".csv")
  write.csv(x = data_final,file = filename)
  data_final<-NULL
}

#remDr$close
#remDr$open()
# data_final1 <- data_final %>% mutate(ReviewRating = ReviewRating/10)

#baseurl <-c("https://www.tripadvisor.in/Hotel_Review-g57044-d6443232-Reviews-Courtyard_Lehi_at_Thanksgiving_Point-Lehi_Utah.html#REVIEWS",
          #  "https://www.tripadvisor.in/Hotel_Review-g57044-d6443232-Reviews-Courtyard_Lehi_at_Thanksgiving_Point-Lehi_Utah.html#REVIEWS")


#***************************MERGING DATA FILES*********************************************#
files = list.files(pattern="*.csv")
myfiles = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))
myfiles$city <- "GOA"
myfiles1<- myfiles %>% mutate(ReviewRating = ReviewRating/10)
saveRDS(myfiles1, file = "goa.rds")
getwd()
