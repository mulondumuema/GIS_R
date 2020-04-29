#function to geocode a csv file and return a file with coordinates and also a shapefile. It saves these files in the set directory.
#the input file should have a column called addresses with the address to be geocoded.

#load the required libraries
library(ggmap)
library(maptools)
library(rgdal)
library(sp)
library(raster)

#define the function which you define the input csv file with the addresses, output csv that will have coordinates, output shapefile, and the api key
geocoding_func <- function(setDir, inputCSV, outputCSV, outputShp, apiKey){
  setwd(setDir)
  origAddress <- read.csv(inputCSV, stringsAsFactors = FALSE)
  geocoded <- data.frame(stringsAsFactors = FALSE)
  register_google(key = apiKey)
  ggmap(get_googlemap())
  for(i in 1:nrow(origAddress))
  {
    result <- geocode(origAddress$addresses[i], output = "latlona", source = "google")
    origAddress$lon[i] <- as.numeric(result[1])
    origAddress$lat[i] <- as.numeric(result[2])
    origAddress$geoAddress[i] <- as.character(result[3])
  }
  write.csv(origAddress, outputCSV, row.names=FALSE)
  origShp <-  origAddress
  coordinates(origShp) <- ~lon+lat #column names of the longitude and latitude columns
  proj4string(origShp) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  raster::shapefile(origShp, outputShp)
  return(origAddress)
}

#putting the geocoding_func to use.
origAddr <- geocoding_func(setDir = "~/excel_files", inputCSV = "input.csv", outputCSV = "output.csv", outputShp = "output.shp", apiKey = "Google_API_Key")
View(origAddr)

