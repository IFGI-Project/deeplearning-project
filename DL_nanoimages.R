###########################################################
#   Introduction to Deep Learning in R for the            #
#   Analysis of UAV-based Remote Sensing Data             #
#   OpenGeoHub summer school 2020, Christian Knoth        #
#   Aug 17, 2020                                          #
###########################################################


# LIBRARIES -----------------------------------------------------------------------------------

library(keras)
library(tensorflow)
library(tfdatasets)
library(purrr)
library(ggplot2)
library(rsample)
library(stars)
library(raster)
library(reticulate)
library(mapview)
library(magick)
library(leaflet)
library(leafem)     #Add RGB image a leaflet


setwd(".")

# Solution to problem of (Error: Python module tensorflow.keras was not found.)
# tensorflow::install_tensorflow()
# tensorflow::tf_config()  No run

# BUILDING YOUR  MODEL ------------------------------------------------------------------------

#initiate an empty model
first_model <- keras::keras_model_sequential()
#add first layer, the expected input is of shape 128 by 128 on three channels (we will be dealing with RGB images)
layer_conv_2d(first_model,filters = 32,kernel_size = 3, activation = "relu",input_shape = c(128,128,3))
summary(first_model)

layer_max_pooling_2d(first_model, pool_size = c(2, 2))
layer_conv_2d(first_model, filters = 64, kernel_size = c(3, 3), activation = "relu")
layer_max_pooling_2d(first_model, pool_size = c(2, 2))
layer_conv_2d(first_model, filters = 128, kernel_size = c(3, 3), activation = "relu")
layer_max_pooling_2d(first_model, pool_size = c(2, 2))
layer_conv_2d(first_model, filters = 128, kernel_size = c(3, 3), activation = "relu")
layer_max_pooling_2d(first_model, pool_size = c(2, 2))
layer_flatten(first_model)
layer_dense(first_model, units = 256, activation = "relu")
layer_dense(first_model, units = 1, activation = "sigmoid")

summary(first_model)


# PREPARING YOUR DATA -------------------------------------------------------------------------

# get all file paths of the images containing our target
subset_list <- list.files("./training_part1/true", full.names = T)

# create a data.frame with two columns: file paths, and the labels (1)
data_true <- data.frame(img=subset_list,lbl=rep(1L,length(subset_list)))

# get all file paths of the images containing non-targets
subset_list <- list.files("./training_part1/false", full.names = T)

#creating a data.frame with two columns: file paths, and the labels (0)
data_false <- data.frame(img=subset_list,lbl=rep(0L,length(subset_list)))

#merge both data.frames
data <- rbind(data_true,data_false)

# randomly split data set into training (~75%) and validation data (~25%)
# use `lbl` as stratum, so that the split is being done proportional for
# targets and non-targets
set.seed(2020)
data <- initial_split(data,prop = 0.75, strata = "lbl")

# access the datacue
training(data)
testing(data)

#training
head(training(data)) # returns the the first few entries in the training data.frame

# compare the number of files in the training data, that show non-targets vs, those that
# show targets -> should be similiar
c(nrow(training(data)[training(data)$lbl==0,]), nrow(training(data)[training(data)$lbl==1,]))

#prepare training dataset
training_dataset <- tensor_slices_dataset(training(data))

#if you want to get a list of all tensors, you can use the as_iterator() and iterate() functions
dataset_iterator <- as_iterator(training_dataset)
dataset_list <- iterate(dataset_iterator)
#check the first few items of that list, each item one is again a list with two items: img and lbl
head(dataset_list)

#get input shape expected by first_model
subset_size <- first_model$input_shape[2:3]

# apply function on each dataset element: function is list_modify()
#->modify list item "img" three times:

# 1 read decode jpeg
training_dataset <-
  dataset_map(training_dataset, function(.x)
    list_modify(.x, img = tf$image$decode_jpeg(tf$io$read_file(.x$img))))

# 2 convert data type
training_dataset <-
  dataset_map(training_dataset, function(.x)
    list_modify(.x, img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32)))

# 3 resize to the size expected by model
training_dataset <-
  dataset_map(training_dataset, function(.x)
    list_modify(.x, img = tf$image$resize(.x$img, size = shape(subset_size[1], subset_size[2]))))

training_dataset <- dataset_shuffle(training_dataset, buffer_size = 10L*128)
training_dataset <- dataset_batch(training_dataset, 10L)
training_dataset <- dataset_map(training_dataset, unname)

dataset_iterator <- as_iterator(training_dataset)
dataset_list <- iterate(dataset_iterator)
dataset_list[[1]][[1]]

dataset_list[[1]][[1]]$shape

dataset_list[[1]][[2]]


#validation
validation_dataset <- tensor_slices_dataset(testing(data))

validation_dataset <-
  dataset_map(validation_dataset, function(.x)
    list_modify(.x, img = tf$image$decode_jpeg(tf$io$read_file(.x$img))))

validation_dataset <-
  dataset_map(validation_dataset, function(.x)
    list_modify(.x, img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32)))

validation_dataset <-
  dataset_map(validation_dataset, function(.x)
    list_modify(.x, img = tf$image$resize(.x$img, size = shape(subset_size[1], subset_size[2]))))

validation_dataset <- dataset_batch(validation_dataset, 10L)
validation_dataset <- dataset_map(validation_dataset, unname)

# TRAINING YOUR MODEL -------------------------------------------------------------------------

compile(
  first_model,
  optimizer = optimizer_rmsprop(lr = 5e-5),
  loss = "binary_crossentropy",
  metrics = "accuracy"
)

diagnostics <- fit(first_model,
                   training_dataset,
                   epochs = 15,
                   validation_data = validation_dataset)

# PREDICTING WITH YOUR MODEL ------------------------------------------------------------------

predictions <- predict(first_model,validation_dataset)
head(predictions)
tail(predictions)

nrow(testing(data))
par(mfrow=c(3,3),mai=c(0.1,0.1,0.3,0.1),cex=0.8)
for(i in 1:9){
  #sample <- floor(runif(n = 1,min = 1,max = 56))
  img_path <- as.character(testing(data)[[i,1]])
# Name for plotting
  img_name <- gsub(pattern = "\\.jpg$", "", basename(img_path))
  img <- stack(img_path)
  plotRGB(img,margins=T,main = paste0("pred:",round(predictions[i],digits=3)," | ","label:",as.character(testing(data)[[i,2]])," | ", "ID:",img_name))
}


# Redo this part changing the test set ------------------------------------------------------------------------------

# get all file paths of the images of our test area
subset_list <- list.files(path="./testarea_part1/subsets/", full.names = T)

dataset <- tensor_slices_dataset(subset_list)
dataset <- dataset_map(dataset, function(.x)
  tf$image$decode_jpeg(tf$io$read_file(.x)))
dataset <- dataset_map(dataset, function(.x)
  tf$image$convert_image_dtype(.x, dtype = tf$float32))
dataset <- dataset_map(dataset, function(.x)
  tf$image$resize(.x, size = shape(128, 128)))
dataset <- dataset_batch(dataset, 10L)
dataset <-  dataset_map(dataset, unname)

#save the model
save_model_hdf5(first_model,filepath = "./my_first_model_2021_T3_no_rivers.h5")


#load other model
first_model <- load_model_hdf5("./my_first_model_2021_T3_no_rivers.h5")

#Predict for the whole area
predictions_test <- predict(first_model, dataset)
head(predictions_test)


#crs transformation
Pred_shp = st_read("./Shapefiles/test/grid_all_image.shp")
Pred_shp_wgs84_test = st_transform(Pred_shp, crs = 4326)
Pred_shp_wgs84_test$Prediction = predictions_test

write.csv(Pred_shp_wgs84_test, "Prediction_test_all_2021_T3_no_rivers.csv")

#Creation of shp with right order (in case that numbers of images are not in order)
img_name_subset <- gsub(pattern = "\\.jpg$", "", subset_list)
img_name_subset <- gsub(pattern = "./testarea_part1/subsets//", "", img_name_subset)
subset_df = data.frame(Id = img_name_subset, Pred_test = predictions_test)

# Pred_shp_wgs84$Id = 1:nrow(Pred_shp_wgs84_test)
# Pred_shp_wgs84_test = merge(Pred_shp_wgs84_test,subset_df,by.x="Id", by.y="Id")

popup <- paste0("<strong>Name: </strong>", 
                Pred_shp_wgs84_test$Prediction)
pal <- colorNumeric(
  palette = c("red","yellow","green"),
  domain = Pred_shp_wgs84_test$Prediction)

#Leaflet visualization
leaflet() %>% 
  addProviderTiles("Esri.WorldImagery") %>% 
  #addRasterRGB(Image2021_32bits,r=1,g=2,b=3) %>%    
  addPolygons(data=Pred_shp_wgs84_test, weight = 1, opacity = 0.2, color = "white", fillColor = ~pal(Prediction), popup=popup,   dashArray = "0",
              fillOpacity = 0.5) %>% addLegend("topright", pal = pal, values = Pred_shp_wgs84_test$Prediction,
                                               title = "Prediction",
                                               labFormat = labelFormat(prefix = ""),
                                               opacity = 1
              )

write_sf(Pred_shp_wgs84_test, "./Predictions/Final_Pred_shp_wgs84_test_2021_no_rivers.shp")

