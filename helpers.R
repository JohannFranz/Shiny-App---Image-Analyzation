library(ColorPalette)
library(SuperpixelImageSegmentation)
library(raster)
library(rgdal)
library(rasterVis) 

init = Image_Segmentation$new()

#reshape the image into a data frame
transformImageToDataFrame <- function(img) {
  df = data.frame(
    red = matrix(img[,,1], ncol=1),
    green = matrix(img[,,2], ncol=1),
    blue = matrix(img[,,3], ncol=1)
  )
  return (df)
}

#Use K-Means Segmentation
segmentDataFrame  <- function(df, numClusters){
  ### compute the k-means clustering
  K = kmeans(df,numClusters)
  df$label = K$cluster
  
  # get the coloring
  colors = data.frame(
    label = 1:nrow(K$centers), 
    R = K$centers[,"red"],
    G = K$centers[,"green"],
    B = K$centers[,"blue"]
  )
  
  # merge color codes on to df
  # IMPORTANT: we must maintain the original order of the df after the merge!
  df$order = 1:nrow(df)
  df = merge(df, colors)
  df = df[order(df$order),]
  df$order = NULL
  
  return (df)
}

#transform datafram back into a segmented image
getSegmentedImage  <- function(img, df){
  # get mean color channel values for each row of the df.
  R = matrix(df$R, nrow=dim(img)[1])
  G = matrix(df$G, nrow=dim(img)[1])
  B = matrix(df$B, nrow=dim(img)[1])
  
  # reconstitute the segmented image in the same shape as the input image
  img = array(dim=dim(img))
  img[,,1] = R
  img[,,2] = G
  img[,,3] = B
  return (img)
}

#plots the segmented image for color space tab
plotSegmentedImage  <- function(df){
  return (ggplot(df, aes(x=u, y=v, col=rgb(R,G,B))) + 
    geom_point(size=2) + scale_color_identity())
}

#plots the original image for color space tab
plotNonSegmentedImage <- function(df) {
  # image
  ggplot(df, aes(x=u, y=v, col=rgb(red,green,blue))) + 
    geom_point(size=2) + scale_color_identity()
}

performPrincipalComponentsAnalysis <- function(df) {
  # perform PCA on the mandril data and add the uv coordinates to the dataframe
  PCA = prcomp(df[,c("red","green","blue")], center=TRUE, scale=TRUE)
  df$u = PCA$x[,1]
  df$v = PCA$x[,2]
  
  return (df)
}

#create an image temporarily and save it on the server's hard disk
generateImage <- function(img, extension) {
  outfile <- tempfile(pattern = "temp_", tmpdir = "www", fileext = extension)
  writePNG(img, target = outfile)
  return (outfile)
}

#converts an image from rgb-color space to hsv-space
#Important: The conversion to rgb is done by the function "rgb2hsv" from grDevices-Packege. 
#           In tests the expected results differ slightly from actual results. 
#           But for the purpose of this app, it should not matter.
RGBtoHSV <- function(img){
  #make a copy of the image
  temp <- img
  #loop over every row and column
  for(row in 1:nrow(temp)) {
    for(col in 1:ncol(temp)) {
      rgbval <- rgb2hsv(temp[row,col,1], temp[row,col,2], temp[row,col,3], maxColorValue = 1.0)
      temp[row,col,1:3] = rgbval
    }
  }
  return (temp)
}

#converts an image from hsv-space to rgb-color space 
#Important: The conversion to rgb is done by the function "hsv2rgb" from ColorPalette-Packege. 
#           In tests the expected results differ slightly from actual results. 
#           But for the purpose of this app, it should not matter.
HSVtoRGB <- function(img){
  #make a copy of the image
  temp <- img
  #loop over every row and column
  for(row in 1:nrow(temp)) {
    for(col in 1:ncol(temp)) {
      #the hue value needs to be in range 0-360, but is currently in range 0-1
      temp[row,col,1] <- temp[row,col,1] * 360
      hexValue <- hsv2rgb(temp[row,col,1], temp[row,col,2], temp[row,col,3])
      #hsv2rgb returns a hex-color-value like "FFAB12", transform into decimal values in range 0-255
      rgbValue <- col2rgb(hexValue)
      #change range to 0-1
      temp[row,col,1:3] <- rgbValue / 255
    }
  }
  return (temp)
}


loadTimeSeriesData <- function(path) {
  data <- list.files(path, full.names = TRUE, pattern = ".tif$")
  return (data)
}

createRasterStack <- function(data) {
  # Create a raster stack of the time series
  stackData <- stack(data)
  
  #Scale stack data
  stackData <- stackData/10000
  
  return (stackData)
}

#files contains a string vector of paths for every RGB-file in the time series
#data contains the rasterstack created with "files"
#day is a string with the selected day
getRealColorFilePathForTimeSeries <- function(files, data, day) {
  idx <- getTimeSeriesIndex(data, day)
  file <- str_replace(files[idx], fixed("/NDVI"), "/RGB")
  file <- str_replace(file, fixed("ndvi_crop"), "landRGB")
  return (file)
}


#converts the rasterstack into a string vector with the days of every element in the time series
getDayVectorForTimeSeries <- function(data) {
  rasterNames  <- gsub("X","Day ", names(data))
  # Remove HARV_NDVI_crop from the second part of the string 
  rasterNames  <- gsub("_HARV_ndvi_crop","",rasterNames)
  
  return (rasterNames)
}


#get the index of the element in the time series data
getTimeSeriesIndex <- function(files, value) {
  files <- getDayVectorForTimeSeries(files)
  i <- 1
  for (element in files){
    if (element == value)
      return (i)
    i <- i+1
  }
  return (1)
}

#data is the rasterstack of the time series data
calculateMeanNDVIFromRaster <- function(data) {
  # calculate mean NDVI for each raster
  meanNDVI <- cellStats(data,mean)
  
  # convert output array to data.frame
  meanNDVI <- as.data.frame(meanNDVI)
  
}

#format the column names, add new columns, etc.
formatMeanNDVIdata <- function(data) {
  # rename the NDVI column
  names(data) <- "meanNDVI"
  
  # add a site column to our data
  data$site <- "HARV"
  
  # add a "year" column to our data
  data$year <- "2011"
  
  # note the use of the vertical bar character ( | ) is equivalent to "or". This
  # allows us to search for more than one pattern in our text strings.
  days <- gsub(pattern = "X|_HARV_ndvi_crop", #the pattern to find 
                     x = row.names(data), #the object containing the strings
                     replacement = "") #what to replace each instance of the pattern with
  
  # add julianDay values as a column in the data frame
  data$day <- days
  
  #convert the days to date class
  # set the origin for the julian date (1 Jan 2011)
  origin <- as.Date("2011-01-01")
  
  # convert "day" from class character to integer
  data$day <- as.integer(data$day)
  
  # create a date column; -1 added because origin is the 1st. 
  # If not -1, 01/01/2011 + 5 = 01/06/2011 which is Julian day 6, not 5.
  data$Date<- origin + (data$day-1)
  
  return (data)
}


removeIncorrectTimeSeriesData <- function(data){
  # retain only rows with meanNDVI>0.1
  data <- subset(data, meanNDVI>0.1)
  
  return (data)
}


getCompressedPlot <- function(data) {
  
  # use colorbrewer which loads with the rasterVis package to generate
  # a color ramp of yellow to green
  cols <- colorRampPalette(brewer.pal(9,"YlGn"))
  
  rasterNames <- getDayVectorForTimeSeries(data)
  
  # use level plot to create a nice plot with one legend and a 4x4 layout.
  levelplot(data, 
            layout=c(4, 4), # create a 4x4 layout for the data
            col.regions=cols, # add a color ramp
            names.attr=rasterNames,
            main = "NDVI for several days among the year 2011")
}



#Transform into HSV-Color Space and maximize the brightness, afterwards transform back to RGB-Space
processBrightness <- function(image, brightness){
  if (brightness == TRUE){
    image <- RGBtoHSV(image)
    image[,,3] = 1.0
    return (HSVtoRGB(image))
  }
  return (image)
}

#Transform into HSV-Color Space and maximize the saturation, afterwards transform back to RGB-Space
processSaturation <- function(image, saturation){
  if (saturation == TRUE){
    image <- RGBtoHSV(image)
    image[,,2] = 1.0
    return (HSVtoRGB(image))
  }
  return (image)
}

#Depending on which color channel is supposed to be shown (Red,Green or Blue), the other channels 
#are set to 0
processChannelChoices <- function(image, choices){
  if (choices == "2"){
    image[,,2:3]=0
  }
  else if (choices == "3") {
    image[,,1]=0
    image[,,3]=0
  }
  else if (choices == "4"){
    image[,,1:2]=0
  }
  return (image)
}

#Transforms the image using SLIC segmentation
#Regions of pixels are summed up into super-pixels. 
#Afterwards the Affinity-Propagation algorithm is run on those super-pixels for segmentation. 
processSLIC <- function(image, showGrid){
  if (showGrid == TRUE){
    image = superpixels(input_image = image, method = "slic", superpixel = 200, 
                                compactness = 20, return_slic_data = TRUE, return_labels = TRUE, 
                                write_slic = "", verbose = TRUE)$slic_data
    image <- NormalizeObject(image)
  } else {
    spix <- init$spixel_segmentation(input_image = image, method = "slic", superpixel = 200, 
                                     AP_data = TRUE, use_median = TRUE, sim_color_radius = 10)
    image <- spix$AP_image_data
  }
  return (image)
}

#Transforms the image using SLICO segmentation
#Regions of pixels are summed up into super-pixels. 
#Afterwards the Affinity-Propagation algorithm is run on those super-pixels for segmentation. 
#The main difference to the SLIC-Method is that the size of the super-pixels is mostly uniform
processSLICO <- function(image, showGrid){
  if (showGrid == TRUE){
    image = superpixels(input_image = image, method = "slico", superpixel = 200, 
                                return_slic_data = TRUE, return_labels = TRUE, write_slic = "", 
                                verbose = TRUE)$slic_data 
    image <- NormalizeObject(image)
  } else {
    spix <- init$spixel_segmentation(input_image = image, method = "slico", superpixel = 200, 
                                     AP_data = TRUE, use_median = TRUE, sim_color_radius = 10)
    image <- spix$AP_image_data
  }
  return (image)
}

#Helper Method to check which segmentation method was selected
processSegmentation <- function(image, segmentationChoices, segmentClusters, slicmethod, slicGrid)
{
  if (segmentationChoices == "2")
  {
    df <- transformImageToDataFrame(image)
    df <- segmentDataFrame(df, segmentClusters)
    image <- getSegmentedImage(image, df)
  } else if (segmentationChoices == "3")
  {
    if (slicmethod == "1")
    {
      image <- processSLIC(image, slicGrid)
    } else if (slicmethod == "2")
    {
      image <- processSLICO(image, slicGrid)
    }
  }
  return (image)
}

#show widgets for the first two tabs
showSegmentation <- function(choice){
  shinyjs::show("selectImage")
  shinyjs::show("brightness")
  shinyjs::show("saturation")
  shinyjs::show("hr1")
  shinyjs::hide("seriesChoices")
  shinyjs::hide("incorrectData")
}

#Hide unnecessary widgets, show necessary widgets
showColorSpacePanel <- function(choice){
  showSegmentation(choice)
  shinyjs::hide("hr2")
  shinyjs::hide("channelChoices")
  shinyjs::hide("segmentationChoices")
  shinyjs::hide("segmentClusters")
  shinyjs::hide("slicmethod")
  shinyjs::hide("slicGrid")
  shinyjs::show("clusterColorSpace")
}

#Hide unnecessary widgets, show necessary widgets
showSegmentationPanel <- function(choice){
  showSegmentation(choice)
  shinyjs::show("channelChoices")
  shinyjs::show("segmentationChoices")
  shinyjs::hide("clusterColorSpace")
  if (choice == "2"){
    shinyjs::show("segmentClusters")
  } else if(choice == "3"){
    shinyjs::show("slicmethod")
    shinyjs::show("slicGrid")
  }
}

#Hide unnecessary widgets, show necessary widgets
showTimeSeriesPanel <- function(){
  shinyjs::show("seriesChoices")
  shinyjs::show("incorrectData")
  shinyjs::hide("selectImage")
  shinyjs::hide("channelChoices")
  shinyjs::hide("segmentationChoices")
  shinyjs::hide("segmentClusters")
  shinyjs::hide("slicmethod")
  shinyjs::hide("slicGrid")
  shinyjs::hide("brightness")
  shinyjs::hide("saturation")
  shinyjs::hide("clusterColorSpace")
  shinyjs::hide("hr1")
}
