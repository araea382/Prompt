library(rjson)

## 7-11
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
m3 <- lapply(1:1000, 
    function(x){
        c(json_data$features[[x]]$attributes$PROV_CODE,
          json_data$features[[x]]$attributes$AMP_CODE,
          json_data$features[[x]]$attributes$TAM_CODE,
          json_data$features[[x]]$attributes$STORECODE,
          json_data$features[[x]]$attributes$ZIP_CODE)
    })

df <- as.data.frame(t(as.data.frame(m3)))
rownames(df) <- NULL
colnames(df) <- c("prov_code", "amp_code", "tam_code", "store_code", "zip_code")




