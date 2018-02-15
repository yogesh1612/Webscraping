
library(dplyr)
library(tidytext)
library(tm)
library(ggplot2)
library(RSelenium)
library(rvest)
library(stringr) 


baseurl = "https://www.amazon.in/Optimum-Nutrition-Standard-Protein-Powder/product-reviews/B000QSNYGI/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber=1"
n = read_html(baseurl)%>% html_nodes('.page-button a')%>%html_text()%>%as.integer()%>%max()
print(n)
review_no=c()
review_text = c()
for(i in 1:n){
  newpg = paste0("https://www.amazon.in/Optimum-Nutrition-Standard-Protein-Powder/product-reviews/B000QSNYGI/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber=",as.character(i))
  review<-read_html(newpg)%>%html_nodes('.review-text')%>%html_text()
  review_text <- append(review_text,review)
  cat("writing page number:",i,"of",n,"\n")
  write.csv(x = review_text,file = "protienpowder.csv",col.names = FALSE,append = TRUE)
  Sys.sleep(5)
}
