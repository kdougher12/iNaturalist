library(raster)
library(sp)
library(rgdal)

### Start with NLCD raster and crop down to the size of lancaster county ###

# Load 2011 NLCD raster
NLCD <- raster("/Users/kdougherty8/Documents/Working Documents from Box/NLCD_2011.tif")

#' Dimensions: size of file in pixels (nrow, ncol, ncells)
#' Resolution: size of each pixel
#' Extent: spatial extent of the raster
#' Coord. Ref.: the coordinate reference system string for the raster
NLCD

# Create a new projection for UTM zone 14
newprojection <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Reproject NLCD with to UTM zone 14
NLCD <- projectRaster(NLCD, crs = newprojection)

#Verify that reprojection was successful
NLCD

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

#Project observations to UTM using newprojection
iNaturalist_Observations <- spTransform(iNaturalist_Observations, CRS(newprojection))

#' Plot NLCD
#'    Add inaturalist observations to NLCD using the add argument
plot(NLCD)
plot(iNaturalist_Observations, pch=16, col="red", add=TRUE)
