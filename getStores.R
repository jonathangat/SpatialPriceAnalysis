#specifying input in Hebrew
Sys.setlocale("LC_ALL", "Hebrew")

#installing the XML package
install.packages("xml")

#installing the readtext package
install.packages("readtext")

#loading the XML package
library(XML)

#loading the readtext package
library(readtext)

#loading the methods library
library(methods)

#parsing the first file
tmpFile <- xmlParse("C:/Users/jonat/Documents/GISModule/finalAssessment/stores/Stores0000072906390-202001012330.xml",
                    encoding = "UTF-16LE")



install.packages("httr")
library(httr)
library(dplyr)

install.packages("jsonlite")
library(jsonlite)

r <- GET("https://api.superget.co.il")
r

status_code(r)
headers(r)
str(content(r))
http_status(r)
r$status_code
headers(r)
cookies(r)
content(r, "text")
content(r, "raw")
content(r, "parsed")

url <-"https://api.superget.co.il"
key <- "c194ad0c878dc086e935d2ecb4a1e27c00587645"
action <- "TestFunction"

r <- GET(url, query = list(api_key = key, action = action))
content(TLVStores, "text", encoding = "UTF-8")

TLVCityCode <- "1180"
getStoresAction <- "GetStoresByCityID"
TLVStores <- GET(url, query=list(api_key = key, action = getStoresAction, city_id = TLVCityCode))
headers(TLVStores)


parsed_stores <-  content(TLVStores, "parsed", encoding = "UTF-8")
parsed_stores

?names
names(TLVStores)
TLVStores$headers$`content-type`
str(parsed_stores$doc)

content(TLVStores, "text")
Sys.setlocale("LC_ALL", "Hebrew")

TLVStores$headers$`content-type`
singleStore$headers$`content-type`


content(TLVStores,"text", encoding = "UTF-8")
todel <-  content(TLVStores, "text")
Encoding(todel) <- "UTF-8"
todel
rm(todel)
parsed_stores
?htmlParse

install.packages("xml2")
library(xml2)
tat <- content(TLVStores,"text", encoding = "UTF-8")
tat                  
tata <-  fromJSON(tat)
?fromJSON

typeof(tata)
tata$chain_id

storesDF <- data.frame(tata)
typeof(stores)
typeof(storesDF)
tata[,11:12]

filter(tata,chain_code == "7290172900007")
tata$chain_code

newdf <-data_frame(tata$store_id,
                   tata$chain_id)
typeof(newdf)

filteredStores <- dplyr::filter(tata, chain_code != "7290172900007" #Excluding the "SuperPharm" chain
                                & sub_chain_code != "5" #Excluding the "Be Pharm" sub chain (belongs to Shufersal)
                                & store_code != "539") #Exluding the "AM:PM online" store

#Adding manualy xy coordinates for a single store that has no xy data
filteredStores[114,11] = "32.0624141"
filteredStores[114,12] = "34.7732659"

#Fixing wrong coordinates for store 1196 (line 111)
filteredStores[111,11] = "32.0824511"
filteredStores[111,12] = "34.7784227"

#Fixing wrong coordinates for store 1075 (line 93)
filteredStores[93,11] = "32.057635"
filteredStores[93,12] = "34.811125"

#Fixing wrong coordinates for store 160 (line 12)
filteredStores[12,11] = "32.0509574"
filteredStores[12,12] = "34.7516468"

#Fixing wrong coordinates for store 1222 (line 113)
filteredStores[113,11] = "32.0954439"
filteredStores[113,12] = "34.7756062"



library(tmap)
library(tmaptools)
tmap_mode("plot")

summary(filteredStores)

#transfer xy coordinates to numeric
filteredStores$store_gps_lat <- as.numeric(filteredStores$store_gps_lat)
filteredStores$store_gps_lng <- as.numeric(filteredStores$store_gps_lng)

library(sp)
library(sf)
library(rgdal)
library(maptools)
library(rgeos)

filteredStoressp <-  SpatialPointsDataFrame(filteredStores[,c(12,11)],filteredStores[,-c(12,11)],
                                            proj4string = CRS("+proj=longlat +datum=WGS84"))

str(filteredStoressp)


tmap_mode("view")
tm_shape(filteredStoressp)



cityLimits <-  st_read("cityArea/City Limits.shp")
TLVQuarters <- st_read("TLVQuarters/Quarters.shp")
#selecting the city centre - quarters 3, 4, 6 and 5
TLVCityCentre <- TLVQuarters[TLVQuarters$krova %in% c('3','4','6','5'),]




     
     
class(TLVCityCentre)
TLVCityCentreUnion <- st_combine(TLVCityCentre)
class(TLVCityCentreUnion)

class(TLVCityCentreUnion)
TLVCityCentreUnionSp <- as(TLVCityCentreUnion,'Spatial')
class(TLVCityCentreUnionSp)
TLVCityCentreUnionSp
qtm(TLVCityCentreUnionSp)




summary(TLVQuarters)
typeof(TLVQuarters)
qtm(cityLimits)



library(raster)
library(dismo)
library(PBSmapping)

vor <- voronoi(filteredStoressp)

tm_shape(vor) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(TLVCityCentreUnion) + 
  tm_polygons(col = "red", alpha = 0.5) +
  tm_shape(filteredStoressp) +
  tm_dots(col = "blue")

TLVCityCentreSp <- SpatialPolygons2PolySet(TLVCityCentre)
print(TLVCityCentre)
qtm(TLVCityCentreUnion)
class(TLVCityCentreSp)

print(vor)

#https://r-spatial.github.io/sf/reference/geos_combine.html

tm_shape(vor) +
  tm_polygons("chain_name",alpha = 0.5) +
  tm_shape(TLVCityCentreUnionSp) + 
  tm_polygons(col = "red", alpha = 0.5) +
  tm_shape(filteredStoressp) +
  tm_dots(col = "blue")


tm_shape(vor) +
  tm_polygons("chain_name",alpha = 0.5) +
  tm_shape(filteredStoressp) +
  tm_dots(col = "blue")

qtm(filteredStoressp)


vor$chain_name



?CRS

#project to wgs84
TLVCityCentreUnionSpWGS84 <- spTransform(TLVCityCentreUnionSp,CRS("+proj=longlat +datum=WGS84"))
#exctracting city centre stores
filteredStoresCityCentre <- filteredStoressp[TLVCityCentreUnionSpWGS84,]

#building a prices list
storeIDs <- filteredStoresCityCentre$store_id



#defining product barcode
productBarcode <- "7290000112220"

prices <- numeric(length(storeIDs))
pricesFail <- prices[]
pricesFail

getPriceFunc = "GetPriceByProductBarCode"




for (i in 1:(length(storeIDs))) {
  
  singleStore <- GET(url, query=list(api_key = key, action = getPriceFunc, product_barcode = productBarcode, store_id = storeIDs[i]))
  singlePrice <- content(singleStore,"text", encoding = "UTF-8")
  SinglePriceValue <-  fromJSON(singlePrice)
  if(is.null(SinglePriceValue$store_product_price))
  {
    prices[i] = "NA"
  }
  else {
    prices[i] = SinglePriceValue$store_product_price
  }
  
}

SinglePriceValue$error_id
storeIDs


for(i in 1:(length(storeIDs)))
{
  print(paste(i,storeIDs[i]))
}


prices[13] = "17.2"
prices[14] = "NA"
prices[15] = "16.9"
prices[16] = "16.9"
prices[17] = "16.9"
prices[18] = "16.9"
prices[19] = "16.9"
prices[20] = "16.9"
prices[21] = "16.9"
prices[22] = "16.9"
prices[23] = "16.9"
prices[24] = "16.9"
prices[25] = "16.9"
prices[26] = "16.9"
prices[27] = "16.9"
prices[28] = "16.9"
prices[29] = "16.9"
prices[30] = "16.9"
prices[31] = "16.9"
prices[32] = "16.9"
prices[33] = "16.9"
prices[34] = "16.9"
prices[35] = "16.9"
prices[36] = "16.9"
prices[37] = "18.9"
prices[38] = "16.9"
prices[39] = "16.9"
prices[40] = "16.9"
prices[41] = "16.9"
prices[42] = "16.9"
prices[43] = "16.9"
prices[44] = "16.9"
prices[45] = "16.9"
prices[46] = "16.9"
prices[47] = "16.9"
prices[48] = "16.9"
prices[49] = "14.2"
prices[50] = "14.2"
prices[51] = "14.2"
prices[52] = "10.9"
prices[53] = "16.9"
prices[54] = "11.9"
prices[55] = "10"
prices[56] = "10"
prices[57] = "10"
prices[58] = "16.9"
prices[59] = "10"
prices[60] = "10"
prices[61] = "17.7"
prices[62] = "10"
prices[63] = "10"
prices[64] = "10"
prices[65] = "10"
prices[66] = "14.9"
prices[67] = "14.2"
prices[68] = "15.9"
prices[69] = "16.9"

prices

prices
storesID


pricesFloat <- as.double(prices)
pricesFloat
pricesDF <- data.frame(storeIDs,pricesFloat)
pricesDF$prices

hist(pricesDF$prices)



hist(pricesDF$prices,
     main="Histogram for Tahini Prices in Central Tel Aviv",
     xlab="Price (in NIS)",
     border="black",
     col="gray",
     xlim=c(10,20),
     ylim=c(0,40),
     las=1,
     breaks = 9)
max(pricesDF$prices, na.rm = TRUE)

summary(tata)
filteredStoresCityCentre$chain_name
chainNameHeb <- unique(filteredStoressp$chain_name,incomparables = FALSE)
chainNameEng <- c("Shufersal", "Osher Ad", "Dor Alon", "Tiv Taam", "Lahav", "Victory", "Fresh Market",
                  "Rami Levy", "Yohananof", "Bitan", "Mega", "Eden Teva")
chainNameDic <- data.frame(chainNameHeb,chainNameEng)

chainNameDic
chainNameDic$chainNameHeb
filteredStoresCityCentre$
class(filteredStoresCityCentre)



filteredStoresCityCentre@data <- data.frame(filteredStoresCityCentre@data,chainNameDic[match(
  filteredStoresCityCentre@data[,"chain_name"],chainNameDic[,"chainNameHeb"]),])
filteredStoresCityCentre$chainNameEng


storeIDs
prices
qtm(filteredStoressp)

class(filteredStoresCityCentre)
summary(filteredStoresCityCentre)

tmap_mode("view")
qtm(filteredStoresCityCentre)

filteredStoressp
TLVCityCentreUnionSp

class(TLVCityCentre)
class(filteredStoressp)

qtm(BoroughMapSP)


class(vor)
qtm(filteredVorCityCentre)
qtm(TLVCityCentreUnionSpWGS84)
class(TLVCityCentreUnionSpWGS84)

TLVCityCentrebbox <- st_bbox(TLVCityCentreUnionSpWGS84)
TLVCityCentrebboxString <- paste(TLVCityCentrebbox["x","min"],TLVCityCentrebbox["y","min"],
                                 TLVCityCentrebbox["x","max"],TLVCityCentrebbox["y","max"],
                                 sep = ",")

paste("A", 1:6, sep = "")
TLVCityCentrebbox
TLVCityCentrebboxString

osmURL <- "https://api.openstreetmap.org/api/0.6/map"

osmRawData <- GET(osmURL, query=list(bbox = TLVCityCentrebboxString))
osmRawContent <- content(osmRawData,"text", encoding = "UTF-8")
tata <-  fromJSON(tat)
osmRawContent

#locating the xyz tile for Tel Aviv
#https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames

deg2num<-function(lat_deg, lon_deg, zoom){
  lat_rad <- lat_deg * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((lon_deg + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  return( c(xtile, ytile))
}



deg2num.all<-function(lat_deg, lon_deg){
  nums <- as.data.frame(matrix(ncol=6,nrow=21))
  colnames(nums) <- c('zoom', 'x', 'y', 'mapquest_osm', 'mapquest_aerial', 'osm')
  rownames(nums) <- 0:20
  for (zoom in 0:20) {
    num <- deg2num(lat_deg, lon_deg, zoom)
    nums[1+zoom,'zoom'] <- zoom
    nums[1+zoom,'x'] <- num[1]
    nums[1+zoom,'y'] <- num[2]
    nums[1+zoom,'mapquest_osm'] <- paste('http://otile1.mqcdn.com/tiles/1.0.0/map/', zoom, '/', num[1], '/', num[2], '.jpg', sep='')
    nums[1+zoom,'mapquest_aerial'] <- paste('http://otile1.mqcdn.com/tiles/1.0.0/sat/', zoom, '/', num[1], '/', num[2], '.jpg', sep='')
    nums[1+zoom,'osm'] <- paste('https://a.tile.openstreetmap.org/', zoom, '/', num[1], '/', num[2], '.png', sep='')
  }
  return(nums)
}



lat_deg <- mean(TLVCityCentrebbox[2,])
lon_deg <- mean(TLVCityCentrebbox[1,])
tile <- deg2num(lat_deg,lon_deg,15)
tiles <-  deg2num.all(lat_deg,lon_deg)

tile

TLVBuildingRaw <- GET(sprintf("https://data.osmbuildings.org/0.2/anonymous/tile/15/%s/%s.json",tile[1],tile[2]))
TLBBuildingContent <- content(TLVBuildingRaw, "text")
library(geojsonio)
TLBBuildingSp <- geojson_sp(TLVBuildingRaw)

TLBBuildingJSON
buildings <- rgdal::readOGR(TLBBuildingContent)
class(try1)
qtm(try1)

try1@data[which(try1@data$id=="488475407"),]

TLBBuildingContent

install.packages("slippymath")
library(slippymath)

#finding the relevant tiles for our area of interest
tiles <- bbox_to_tile_grid(TLVCityCentrebbox,zoom=15)

#cleaning the buildings object
buildings <-buildings[0,]

#iterating over the tiles and populating the buildings object
for(i in (1:nrow(tiles$tiles))){
  getBuildingsRaw <- GET(sprintf("https://data.osmbuildings.org/0.2/anonymous/tile/15/%s/%s.json",
                                 tiles$tiles[i,"x"],tiles$tiles[i,"y"]))
  if (getBuildingsRaw$status_code != "200")
  {
    next
  }
  else 
  {
    getBuildingsContent <- content(getBuildingsRaw, "text",encoding="UTF-8")
    getBuildingsSp <- rgdal::readOGR(getBuildingsContent)
    buildings <- bind(buildings,getBuildingsSp)
  }
}

qtm(buildings)

che <- table(buildings$id)
che1 <- data.frame(che)
rm(che,che1)

#removing duplicates
buildings <- buildings[!duplicated(buildings@data),]

#cropping buildings by area of interest
buildingsCrop <- crop(buildings,TLVCityCentreUnionSpWGS84)

#projecting voronoi polygons to WGS84
buildingsCropWGS84 <- spTransform(buildingsCrop,CRS("+proj=longlat +datum=WGS84"))

qtm(buildingsCrop)
summary(buildingsCrop)
summary(vorWGS84)

#projecting voronoi polygons to WGS84
vorWGS84 <- spTransform(vor,CRS("+proj=longlat +datum=WGS84"))

#exctracting city centre voronoi polygons
filteredVorCityCentre <- vorWGS84[TLVCityCentreUnionSpWGS84,]

#projecting voronoi polygons to WGS84
filteredVorCityCentre <-spTransform(filteredVorCityCentre,CRS("+proj=longlat +datum=WGS84"))

#merging adjecent voronoi polygons who share the same chain
vorWGS84Union <- gUnaryUnion(filteredVorCityCentre, id = filteredVorCityCentre@data$chain_id)

#restoring data frame
row.names(vorWGS84Union) <- as.character(1:length(vorWGS84Union))
chain_ids <- unique(filteredVorCityCentre@data$chain_id)
chain_ids <- as.data.frame(chain_ids)
colnames(chain_ids) <- "catchment_chain_id"
vorWGS84Union <- SpatialPolygonsDataFrame(vorWGS84Union, chain_ids)

#splitting multipart polygons
vorWGS84UnionDis <- disaggregate(vorWGS84Union)

#adding a catchment id column
vorWGS84UnionDis@data$catchment_id <- 1:nrow(vorWGS84UnionDis@data)


#intersecting voronoi polygons and buildingsCrop
buildingsVoronoi <- intersect(buildingsCropWGS84,vorWGS84UnionDis)

#claculating area of each polygon (in square meters)
buildingsVoronoi@data$buildingArea <- area(buildingsVoronoi)

#summaring area by catchment_id
sumArea <- aggregate(buildingsVoronoi@data$buildingArea, by=list(catchment_id=buildingsVoronoi@data$catchment_id), FUN=sum)
colnames(sumArea) <- c("catchment_id","catchment_sum_area")

#joining sumArea to vorWGS84UnionDis
vorWGS84UnionDis@data <- data.frame(vorWGS84UnionDis@data,
                                    sumArea[match(
                                      vorWGS84UnionDis@data[,"catchment_id"],sumArea[,"catchment_id"]),])


#extracting stores in city centre
stores <- filteredStoressp[TLVCityCentreUnionSpWGS84,]


#joining prices to stores
stores@data <- data.frame(stores@data,
                          pricesDF[match(
                            stores@data[,"store_id"],pricesDF[,"storeIDs"]),])

#converting sp to sf for a spatial join
storesSF <- st_as_sf(stores)
catchmentSF <- st_as_sf(vorWGS84UnionDis)
sjoin <- sf::st_join(storesSF,catchmentSF)

#ommiting rows with missing price values
sjoinClean <- sjoin[!is.na(sjoin$pricesFloat),]

#tabular join for English chain Names
sjoinClean <- merge(sjoinClean,
                    chainNameDic,
                    by.x = "chain_name",
                    by.y = "chainNameHeb")

#converting sf to data.frame
sjoinCleanDF <- as.data.frame(sjoinClean)


p <- ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()
p + facet_wrap(~cyl)


library(ggplot2)
library(sf)
try1 <- ggplot(data=sjoinCleanDF, aes(x=sjoinCleanDF$catchment_sum_area,
                                                 y=sjoinCleanDF$pricesFloat)) + geom_point()

try1 + facet_wrap(~chainNameEng)


#exporting sjoinClean to shapefile
st_write(obj = sjoinClean,
         dsn = "C:/Users/jonat/Documents/GISModule/finalAssessment/export",
         layer = "sjoinClean",
         driver = "ESRI Shapefile")







cor(sjoinClean$catchment_sum_area,sjoinClean$pricesFloat)



qtm(sjoin)

library(maptools)

tmap_mode("view")





tm_shape(vorWGS84) +
  tm_polygons("chain_name",alpha = 0.5)

tm_shape(vorWGS84UnionDis) +
  tm_polygons(alpha = 0.5) +
  tm_shape(buildingsVoronoi) +
  tm_polygons(alpha = 1)


plot(sjoinClean$catchment_sum_area,
     sjoinClean$pricesFloat)

nrow(sjoin)


nrow(sjoinClean)

install.packages("ggpubr")
library("ggpubr")
ggscatter(sjoinClean, x = "catchment_sum_area", y = "pricesFloat", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

BoroughDataMap<-merge(BoroughMapSF, 
                      LondonData, 
                      by.x="GSS_CODE", 
                      by.y="New.code",
                      no.dups = TRUE) chainNameDic$chainNameHeb





