library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(data.table)
library(dplyr)

### Start with NLCD raster and crop down to the size of lancaster county ###

#' Load 2011 NLCD raster 
#'      Reproject to WGS84 Zone 14N in QGIS before loading
NLCD <- raster("/Users/kdougherty8/Documents/Working Documents from Box/NLCD_2011_Projected.tif")

#' Dimensions: size of file in pixels (nrow, ncol, ncells)
#' Resolution: size of each pixel
#' Extent: spatial extent of the raster
#' Coord. Ref.: the coordinate reference system string for the raster
NLCD

# Create a new projection for UTM zone 14
UTM_Projection <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

#'Read Lancaster County Census Tracts shapefile,
#'      Note that in dsn argument only go to folder that contains shapefile
#'      then use shapefile name for layer argument
Lancaster_County_Census_Tracts <- readOGR("/Users/kdougherty8/Documents/Working Documents from Box/Lancaster County Census Tracts", layer = "Lancaster County Census Tracts")

# Crop raster to extent of Lancaster County
NLCD <- crop(NLCD, extent(Lancaster_County_Census_Tracts))

plot(NLCD)

### Fox Observations ###

#'load CSV from iNaturalist
iNaturalist_Observations <- read.csv("/Users/kdougherty8/Documents/Working Documents from Box/observations-26772.csv")

#' Some observations do not have coordinates included, use na.omit to remove them
iNaturalist_Observations<- na.omit(iNaturalist_Observations)

#'Conver Observations to a spatial object
coordinates(iNaturalist_Observations) <- ~longitude+latitude

#Add a coordinate reference system for the original coordinates from iNaturalist
proj4string(iNaturalist_Observations) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

#Project observations to UTM 
iNaturalist_Observations <- spTransform(iNaturalist_Observations, 
                                        CRS(UTM_Projection))

#'Create buffer of 1 km around each observation
#'         Note that byid must = TRUE so that each observation is buffered
Buffer_1k <- gBuffer(iNaturalist_Observations, 
                     byid = TRUE, 
                     width = 1000)

#' Plot NLCD
#'    Add inaturalist observations and buffers to NLCD using the add argument
plot(NLCD)
plot(Buffer_1k, 
     col = "blue", 
     add = TRUE)
plot(iNaturalist_Observations, 
     pch=16, 
     col="red", 
     add=TRUE)


#'Extract land cover under buffers
#'        weights=TRUE and normalizeWeights=False gives the approximate fraction of each cell that is covered by the polygon
#'        df=TRUE results are returned as a dataframe and first colum is a sequential ID
Landcover <- extract(NLCD, 
                     Buffer_1k, 
                     weights = TRUE, 
                     normalizeWeights=FALSE, 
                     df=TRUE)

#' Calculate Area for each row in dataframe
Landcover$Area <- (30.15922^2)*Landcover$weight







