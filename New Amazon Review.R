if(!require(dplyr)){install.packages("dplyr")}else{ library("dplyr")}
if(!require(stringr)){install.packages("stringr")}else{library("stringr")}
if(!require(tidyr)){install.packages("tidyr")}else{library("tidyr")}
if(!require(tidytext)){install.packages("tidytext")}else{library("tidytext")}
if(!require(topicmodels)){install.packages("topicmodels")}else{library("topicmodels")}
if(!require(ggplot2)){install.packages("ggplot2")}else{library("ggplot2")}
if(!require(rvest)){install.packages("rvest")}else{library("rvest")}
if(!require(scales)){install.packages("scales")}else{library("scales")}

extract_reviews<-function(baseurl,filename){
  #get number of pages to extract for any product
  n = read_html(baseurl)%>% html_nodes('.page-button   a')%>%html_text() 
  n = as.numeric(gsub(",", "", n))
  n = max(n)
  
  # baseurl
  loop_url <- substr(baseurl,1,nchar(baseurl)-1)
  f_name = paste0(filename,".csv")
  
  #empty dataframe to store data
  review_data <- data.frame(review = character(), 
                            review_title = character(),
                            review_author = character(),
                            review_author_link= character(),
                            review_rating = character(),
                            found_helpful = character())
  
 # loop and extract data for all webpages
  
  for(i in 1:n){
    #newpg = "https://www.amazon.com/Apple-iPhone-Unlocked-Smartphone-Space/product-reviews/B01LY0GBAG/ref=cm_cr_getr_d_paging_btm_123?ie=UTF8&reviewerType=all_reviews&pageNumber=123"
    newpg = paste0(loop_url,as.character(i))
    page = read_html(newpg)
    
    
    review<-page%>%html_nodes('.review-text')%>%html_text()
    review_title <- page%>%html_nodes('a.a-size-base.review-title')%>%html_text()
    review_author <- page %>% html_nodes('span.a-size-base a.a-size-base')%>% html_text()
    review_author_link <- paste0("https://www.amazon.com",page %>% html_nodes('span.a-size-base a.a-size-base')%>% html_attr('href'))
    review_rating <- page %>% html_nodes('div.a-row:nth-of-type(1) a.a-link-normal span.a-icon-alt')%>%html_text()
    found_helpful <- page %>% html_nodes('.cr-vote') %>% html_text()
    temp <- gsub("One", "1", found_helpful) # Replacing "One" in text to "1"
    found_helpful <- as.integer( str_extract(temp, "[0-9]+")) # Na indicates no one found it useful
    
    
    review_data <- rbind(review_data, data.frame(review = review, 
                                       review_title = review_title,
                                       review_author = review_author,
                                       review_author_link= review_author_link,
                                       review_rating = review_rating,
                                       found_helpful = found_helpful))
    # review_text <- append(review_text,review)
    cat("writing page number:",i,"of",n,"\n")
    write.csv(x = review_data,file = f_name,col.names = FALSE,append = TRUE)
    #Sys.sleep(5)
  }
  
  paste0(f_name,".csv","is stored at","\n",getwd())
}




#Example

#baseurl = "https://www.amazon.com/Samsung-Galaxy-S8-Unlocked-64GB/product-reviews/B06Y14T5YW/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=1"
#extract_reviews(baseurl,"samsung")














