setwd("D:/lynn/Agency Tracking/GA")

##----------------------------------------------------------------------------------##
## 7-11
##----------------------------------------------------------------------------------##
library(rjson)

json_file <- "https://allmap.cpall.co.th/arcgis/rest/services/DML/MapServer/0/query?f=json&where=%201=1%20%20AND%20(%20((REPLACE(STORECODE,%27%20%27,%27%27)%20like%20%27%%%27)%20OR%20(REPLACE(STORECODE,%27-%27,%27%27)%20like%20%27%%%27))%20OR%20((REPLACE(STORENAME,%27%20%27,%27%27)%20like%20%27%%%27)%20OR%20(REPLACE(STORENAME,%27-%27,%27%27)%20like%20%27%%%27))%20OR%20((REPLACE(FULL_ADDRESS_DML_TH,%27%20%27,%27%27)%20like%20%27%%%27)%20OR%20(REPLACE(FULL_ADDRESS_DML_TH,%27-%27,%27%27)%20like%20%27%%%27))%20)%20AND%20IS_ACTIVE%20=%20%27Y%27%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=STORENAME&outSR=102100"
json_data <- fromJSON(file=json_file)

json_data
json_data$features[[1]]$attributes$ZIP_CODE

length(json_data$features)

# m <- unlist(lapply(1:1000, function(x) json_data$features[[x]]$attributes$ZIP_CODE))
m <- sapply(1:1000, function(x) json_data$features[[x]]$attributes$ZIP_CODE)
m2 <- data.frame(zip_code=m)
write.csv(m2, file = "seven-eleven.csv", row.names=FALSE) 

# storecode and zip_code
sv_data <- lapply(1:1000, function(x){
  c(json_data$features[[x]]$attributes$PROV_CODE,
    json_data$features[[x]]$attributes$AMP_CODE,
    json_data$features[[x]]$attributes$TAM_CODE,
    json_data$features[[x]]$attributes$STORECODE,
    json_data$features[[x]]$attributes$ZIP_CODE)
  })

sv <- as.data.frame(t(as.data.frame(sv_data)))
rownames(sv) <- NULL
colnames(sv) <- c("prov_code", "amp_code", "tam_code", "store_code", "zip_code")
write.csv(sv, file = "seven-eleven.csv", row.names=FALSE)

##----------------------------------------------------------------------------------##
## Big-C
##----------------------------------------------------------------------------------##
library(rvest)
library(stringr)
# bigC <- read_html("https://corporate.bigc.co.th/th/stores/bigc_stores/")
url_bc <- "https://corporate.bigc.co.th/th/stores/bigc_stores/"
download.file(url_bc, destfile = "BigC.html", quiet=TRUE)
bigC <- read_html("BigC.html")

bc_nodes <- bigC %>% 
  html_nodes(".col2") %>% html_text()
bc_nodes[1]

str_sub(bc_nodes[1],-5,-1) #get zip_code

bc_data <- sapply(1:length(dt_nodes), function(x){
  str_sub(dt_nodes[x],-5,-1)
})

bc <- data.frame(zip_code=bc_data)
write.csv(bc, file = "big-c.csv", row.names=FALSE)


###
# url <- "https://en.wikipedia.org/wiki/Web_scraping"
# download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
# scraping_wiki <- read_html("scrapedpage.html")
# 
# scraping_wiki %>% html_nodes("h1")
# 
# scraping_wiki %>%
#   html_nodes("h1") %>%
#   html_text()
# 
# p_nodes <- scraping_wiki %>% 
#   html_nodes("p")

##----------------------------------------------------------------------------------##
## Tops
##----------------------------------------------------------------------------------##
url_tops <- "https://topsmarket.tops.co.th/th/store-locations/"
download.file(url_tops, destfile = "Tops.html", quiet=TRUE)
tops <- read_html("Tops.html")

tops_nodes <- tops %>% 
  html_nodes("div.info") %>% html_text()
tops_nodes[1]

length(tops_nodes)

tops_data1 <- sapply(1:5, function(x){
  tel <- str_locate_all(tops_nodes[x], "â·ÃÈÑ¾·ì")
  tel2 <- str_sub(tops_nodes[x],1,tel[[1]][1,1]-1)
  str_sub(tel2,-5,-1)
})

tops_data2 <- sapply(7:93, function(x){
  tel <- str_locate_all(tops_nodes[x], "â·ÃÈÑ¾·ì")
  tel2 <- str_sub(tops_nodes[x],1,tel[[1]][1,1]-1)
  str_sub(tel2,-5,-1)
})

tops_data <- c(tops_data1, tops_data2)

tel <- str_locate_all(tops_nodes[6], "Tel")
tel2 <- str_sub(tops_nodes[6],1,tel[[1]][1,1]-1)

tops_data <- c(tops_data, str_sub(tel2,-5,-1))



##----------------------------------------------------------------------------------##
## 
##----------------------------------------------------------------------------------##

