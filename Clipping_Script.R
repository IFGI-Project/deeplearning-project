# Add required packages
require(raster)
require(sf)
library(rgdal)
library(maptools)
library("jpeg")
library("tiff")

wd = getwd()

# Importing SHP
Training_shp <- st_read("./Shapefiles/training/Training_2.shp")
Test_shp <- st_read("./Shapefiles/test/test_all.shp")

# Creating Subsets based on the class
True_shp <- subset(Training_shp, Clase=="Yes")
False_shp <- subset(Training_shp, Clase=="No")

# Importing Image
Image2021_32bits <- stack("~/Images/Merge_2021May.tif")
Image2017_32bits <- stack("~/Images/2017August.tif")

# Changing from 32bits to 16bits
# Image2021 <- writeRaster(Image2021_32bits, '2021May16bits.tif', datatype='INT2U', overwrite=TRUE, format="GTiff")
# Image2017 <- writeRaster(Image2017_32bits, '2017August16bits.tif', datatype='INT2U', overwrite=TRUE, format="GTiff")
raster::plotRGB(Image2017_32bits,r=1,g=2,b=3,scale=800,stretch="lin")
plot(Test_shp, add = TRUE)

# Creating the fuction for cropping and save the samples in their respective folder
sampling <- function (raster,df,outputfolder){
  for(i in 1:nrow(df)){
    row <- df[i,]
    crop <- crop(raster, extent(row))
    masked <- mask(crop, row)
    print(paste0("Image #: ",i))
    print(masked)
    name_jpeg <- paste(outputfolder,
                       if(i<10){paste0(0,0,0,0,i)} 
                       else if(10<=i & i <100){paste0(0,0,0,i)} 
                       else if(100<=i & i <1000){paste0(0,0,i)} 
                       else if(1000<=i & i <10000){paste0(0,i)}
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


# Running the functions
sampling(Image2021_32bits,True_shp,"~/training_part1/true/")
sampling(Image2021_32bits,False_shp,"~/training_part1/false/")
sampling(Image2017_32bits,Test_shp,"~/testarea_part1/subsets/")

Image2021_32bits <- stack("~/Images/Merge_2021May.tif")
plotRGB(Image2021_32bits,r=1,g=2,b=3,stretch = 'lin')
plot(True_shp, add =TRUE , col="green")
plot(False_shp, add =TRUE , col="red")

dev.off()
