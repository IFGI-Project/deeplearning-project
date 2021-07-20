require(raster)
require(sf)
library(rgdal)
library(maptools)
library("jpeg")
library("tiff")

#file.rename("~/Images/2017May(Original).tif", "~/Images/2017August(Original).tif")
Image2017_32bits <- stack("~/Images/2017August(Original).tif")
raster::plotRGB(Image2021_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
StudyArea <- st_read("./Shapefiles/study_area.shp")

raster::plotRGB(Image2017_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
e <- extent(StudyArea)
myraster.crop2017 <- crop(Image2017_32bits, e, snap="out", file="~/Images/2017August.tif")
raster::plotRGB(myraster.crop2017,r=1,g=2,b=3,scale=800,stretch="lin", )
plot(myraster.crop2021)

