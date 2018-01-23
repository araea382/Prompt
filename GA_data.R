setwd("D:/lynn/Agency Tracking/GA")
library(rjson)
library(rvest)
library(stringr)

##----------------------------------------------------------------------------------##
## Big-C
##----------------------------------------------------------------------------------##
setwd("D:/lynn/Agency Tracking/GA/BigC")
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
write.csv(bc, file = "BigC.csv", row.names=FALSE)


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
data_tops <- read_html("Tops.html")

tops_nodes <- data_tops %>% 
  html_nodes("div.info") %>% html_text()
tops_nodes[1]

length(tops_nodes)

tops1 <- sapply(1:5, function(x){
  tel <- str_locate_all(tops_nodes[x], "â·ÃÈÑ¾·ì")
  tel2 <- str_sub(tops_nodes[x],1,tel[[1]][1,1]-1)
  str_sub(tel2,-5,-1)
})

tops2 <- sapply(7:93, function(x){
  tel <- str_locate_all(tops_nodes[x], "â·ÃÈÑ¾·ì")
  tel2 <- str_sub(tops_nodes[x],1,tel[[1]][1,1]-1)
  str_sub(tel2,-5,-1)
})

tops <- c(tops1, tops2)

# for the SIXTH 
tel <- str_locate_all(tops_nodes[6], "Tel")
tel2 <- str_sub(tops_nodes[6],1,tel[[1]][1,1]-1)

tops <- c(tops, str_sub(tel2,-5,-1))
tops <- data.frame(tops)
colnames(tops) <- "zip_code"
write.csv(tops, file="D:/lynn/Agency Tracking/GA/Tops/TOPS.csv", row.names=FALSE)

##----------------------------------------------------------------------------------##
## 7-11
##----------------------------------------------------------------------------------##
.old <- function(x){
# json_file <- "https://allmap.cpall.co.th/arcgis/rest/services/DML/MapServer/0/query?f=json&where=%201=1%20%20AND%20(%20((REPLACE(STORECODE,%27%20%27,%27%27)%20like%20%27%%%27)%20OR%20(REPLACE(STORECODE,%27-%27,%27%27)%20like%20%27%%%27))%20OR%20((REPLACE(STORENAME,%27%20%27,%27%27)%20like%20%27%%%27)%20OR%20(REPLACE(STORENAME,%27-%27,%27%27)%20like%20%27%%%27))%20OR%20((REPLACE(FULL_ADDRESS_DML_TH,%27%20%27,%27%27)%20like%20%27%%%27)%20OR%20(REPLACE(FULL_ADDRESS_DML_TH,%27-%27,%27%27)%20like%20%27%%%27))%20)%20AND%20IS_ACTIVE%20=%20%27Y%27%20&returnGeometry=true&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=STORENAME&outSR=102100"
# json_data <- fromJSON(file=json_file)
# 
# json_data
# json_data$features[[1]]$attributes$ZIP_CODE
# 
# length(json_data$features)
# 
# # m <- unlist(lapply(1:1000, function(x) json_data$features[[x]]$attributes$ZIP_CODE))
# m <- sapply(1:1000, function(x) json_data$features[[x]]$attributes$ZIP_CODE)
# m2 <- data.frame(zip_code=m)
# write.csv(m2, file = "seven-eleven.csv", row.names=FALSE) 
# 
# # storecode and zip_code
# sv_data <- lapply(1:1000, function(x){
#   c(json_data$features[[x]]$attributes$PROV_CODE,
#     json_data$features[[x]]$attributes$AMP_CODE,
#     json_data$features[[x]]$attributes$TAM_CODE,
#     json_data$features[[x]]$attributes$STORECODE,
#     json_data$features[[x]]$attributes$ZIP_CODE)
#   })
# 
# sv <- as.data.frame(t(as.data.frame(sv_data)))
# rownames(sv) <- NULL
# colnames(sv) <- c("prov_code", "amp_code", "tam_code", "store_code", "zip_code")
# write.csv(sv, file = "seven-eleven.csv", row.names=FALSE)
# seven_eleven <- fromJSON(file="result-10.json")
# seven_eleven$features[[1]]$attributes$ZIP_CODE
}

setwd("D:/lynn/Agency Tracking/GA/7-11/result")
name_sv <- c(10:27,30:58,60:67,70:77,80:86,90:96)

files_sv <- list.files(pattern="*.json")
data_sv <- lapply(files_sv, function(x) fromJSON(file=x))
data_sv[[1]]$features[[1]]  # in the first JSON file, first store
data_sv[[1]]$features[[1]]$attributes$ZIP_CODE

# extract
sv <- lapply(1:length(data_sv), function(y){
  lapply(1:length(data_sv[[y]]$features), function(x){
    c(data_sv[[y]]$features[[x]]$attributes$PROV_CODE,
      data_sv[[y]]$features[[x]]$attributes$ZIP_CODE)
  })
})

sv2 <- as.data.frame(t(as.data.frame(sv)))
rownames(sv2) <- NULL
colnames(sv2) <- c("prov_code", "zip_code")
write.csv(sv2, file = "D:/lynn/Agency Tracking/GA/7-11/seven_eleven.csv", row.names=FALSE)


sv3 <- as.character(sv2[,2])

count<-NULL
for(i in 1:length(sv3)){
  if(nchar(sv3[i]) != 5){
    count= c(count,i)
  }
}

df <- sv3[count]

##----------------------------------------------------------------------------------##
## BBL
##----------------------------------------------------------------------------------##
setwd("D:/lynn/Agency Tracking/GA/BBL/result")
name_bbl <- c(1:77)

files_bbl <- list.files(pattern="*.json")
data_bbl <- lapply(files_bbl, function(x) fromJSON(file=x))
data_bbl[[1]]$d[1][[1]]$Postcode # in the first JSON file, first store
data_bbl[[1]]$d[2][[1]]$Postcode # second store


# extract
bbl <- lapply(1:length(data_bbl), function(y){
  lapply(1:length(data_bbl[[y]]$d), function(x){
      data_bbl[[y]]$d[x][[1]]$Postcode
  })
})

bbl2 <- data.frame(zip_code=unlist(bbl))
write.csv(bbl2, file = "D:/lynn/Agency Tracking/GA/BBL/BBL.csv", row.names=FALSE)


##----------------------------------------------------------------------------------##
## KBANK
##----------------------------------------------------------------------------------##
setwd("D:/lynn/Agency Tracking/GA/KBANK")
data_kb <- fromJSON(file="result.json")

data_kb[[1]]$`_Child_Items_`[[1]]$ADDRESS_E # address

# extract
kb <- lapply(1:length(data_kb[[1]]$`_Child_Items_`), function(x){
    data_kb[[1]]$`_Child_Items_`[[x]]$ADDRESS_E
})

kb2 <- data.frame(zip_code=unlist(kb))
kb3 <- data.frame(zip_code = sapply(1:nrow(kb2), function(x) str_sub(kb2[x,],-5,-1)))
write.csv(kb3, file = "D:/lynn/Agency Tracking/GA/KBANK/KBANK.csv", row.names=FALSE)


##----------------------------------------------------------------------------------##
## MTL
##----------------------------------------------------------------------------------##
setwd("D:/lynn/Agency Tracking/GA/MTL")
data_mtl <- fromJSON(file="MTL_branches.json")

data_mtl$branch[[1]]$zipcode

# extract
mtl <- lapply(1:data_mtl$count, function(x){
  data_mtl$branch[[x]]$zipcode
})

mtl2 <- data.frame(zip_code=unlist(mtl))
write.csv(mtl2, file = "D:/lynn/Agency Tracking/GA/MTL/MTL.csv", row.names=FALSE)


##----------------------------------------------------------------------------------##
## zip_code
##----------------------------------------------------------------------------------##
setwd("D:/lynn/Agency Tracking/GA")

url_zp <- "https://th.wikipedia.org/wiki/ÃÒÂª×èÍÃËÑÊä»ÃÉ³ÕÂìä·Â#"
download.file(url_zp, destfile = "zipcode.html", quiet=TRUE)
data_zp <- read_html("zipcode.html")


