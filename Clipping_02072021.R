require(raster)
require(sf)
library(rgdal)
library(maptools)
library("jpeg")
library("tiff")

Image1_32bits <- stack("~/Images/May2021Rivers.tif")
#Image2_32bits <- stack("~/Images/20210508_145950_90_2413_3B_AnalyticMS_SR_clip.tif")
#Image3_32bits <- stack("~/Images/20210509_145712_16_2407_3B_AnalyticMS_SR_clip.tif")
#Image4_32bits <- stack("~/Images/20210510_150021_07_240c_3B_AnalyticMS_SR_clip.tif")
#Image5_32bits <- stack("~/Images/20210519_140944_68_2465_3B_AnalyticMS_SR_clip.tif")
#Image6_32bits <- stack("~/Images/20210526_151907_66_1065_3B_AnalyticMS_SR_clip.tif")
#Image7_32bits <- stack("~/Images/20210529_140607_40_241e_3B_AnalyticMS_SR_clip.tif")

raster::plotRGB(Image1_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#raster::plotRGB(Image2_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#raster::plotRGB(Image3_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#raster::plotRGB(Image4_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#raster::plotRGB(Image5_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#raster::plotRGB(Image6_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#raster::plotRGB(Image7_32bits,r=1,g=2,b=3,scale=800,stretch="lin")

Training1_shp <- st_read("./Shapefiles/training_rivers/Mask Rivers.shp")
raster::plotRGB(Image1_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
plot(Training1_shp, add=FALSE, col="red")
#Training2_shp <- st_read("./Shapefiles/training_02072021/20210508_145950_90_2413_3B_AnalyticMS_SR_clip.shp")
#raster::plotRGB(Image2_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#plot(Training2_shp, add=TRUE, col="red")
#Training3_shp <- st_read("./Shapefiles/training_02072021/20210509_145712_16_2407_3B_AnalyticMS_SR_clip.shp")
#raster::plotRGB(Image3_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#plot(Training3_shp, add=TRUE, col="red")
#Training4_shp <- st_read("./Shapefiles/training_02072021/20210510_150021_07_240c_3B_AnalyticMS_SR_clip.shp")
#raster::plotRGB(Image4_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#plot(Training4_shp, add=TRUE, col="red")
#Training5_shp <- st_read("./Shapefiles/training_02072021/20210519_140944_68_2465_3B_AnalyticMS_SR_clip.shp")
#raster::plotRGB(Image5_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#plot(Training5_shp, add=TRUE, col="red")
#Training6_shp <- st_read("./Shapefiles/training_02072021/20210526_151907_66_1065_3B_AnalyticMS_SR_clip.shp")
#raster::plotRGB(Image6_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#plot(Training6_shp, add=TRUE, col="red")
#Training7_shp <- st_read("./Shapefiles/training_02072021/20210529_140607_40_241e_3B_AnalyticMS_SR_clip.shp")
#raster::plotRGB(Image7_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
#plot(Training7_shp, add=TRUE, col="red")




sampling <- function (raster,df,outputfolder){
  for(i in 1:nrow(df)){
    row <- df[i,]
    crop <- crop(raster, extent(row))
    masked <- mask(crop, row)
    print(paste0("Image #: ",i))
    print(masked)
    e<-541
    name_jpeg <- paste(outputfolder,
                       if(i+e<10){paste0(0,0,0,0,i+e)} 
                       else if(10<=i+e & i+e <100){paste0(0,0,0,i+e)} 
                       else if(100<=i+e & i+e <1000){paste0(0,0,i+e)} 
                       else if(1000<=i+e & i+e <10000){paste0(0,i+e)}
                       else{i},".jpg",sep="")
    
    #Part to adjust polygons out of the image (only for Image 2017)
    
    #if (i %in% c(83,87,88,186,187,191,192,193,194,195,196,197,282)) {
    # print("Previous image out of range")
    #} else {
    jpeg(filename = name_jpeg, width = 128, height = 128, units = "px",res = 300)
    raster::plotRGB(masked,r=1,g=2,b=3,stretch="lin")
    dev.off()
    #}
    #writeJPEG(img, target = name_jpeg, quality = 0.3)
  }
}

sampling(Image1_32bits,Training1_shp,"~/training_part1/false/")



