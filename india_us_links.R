IPhone6s_India = "https://www.amazon.in/Apple-iPhone-6S-Space-Grey/product-reviews/B01LX3A7CC/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber=1"

IPhone6s_US = "https://www.amazon.com/Apple-iPhone-Unlocked-Smartphone-Space/product-reviews/B01LY0GBAG/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=1"

#baseurl = IPhone6s_India
extract_reviews(IPhone6s_India,"iphone_india_hlp")
extract_reviews(IPhone6s_US,"iphone_us_hlp")



ppowder_india = "https://www.amazon.in/Optimum-Nutrition-Standard-Protein-Powder/product-reviews/B000QSNYGI/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber=1"
ppowder_us = "https://www.amazon.com/Optimum-Nutrition-Standard-Protein-Chocolate/product-reviews/B000QSNYGI/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=650"

extract_reviews(ppowder_india,"ppowder_india")
extract_reviews(ppowder_us,"ppowder_us_700")



ipad_india = "https://www.amazon.in/Apple-iPad-Tablet-Wi-Fi-Space/product-reviews/B06Y67N3JY/ref=cm_cr_arp_d_paging_btm_2?showViewpoints=1&pageNumber=1"
ipad_us = "https://www.amazon.com/Apple-iPad-WiFi-Space-Model/product-reviews/B01LTIORC8/ref=cm_cr_arp_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&pageNumber=1"

extract_reviews(ipad_india,"ipad_india")
extract_reviews(ipad_us,"ipad_us")
