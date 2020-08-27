library(shiny)
library("png")
library("rgl")
library(OpenImageR)
require("ggplot2")
library(raster)
library(rgdal)
library("stringr")

#code for Super-Pixel Segmentation is based on: https://cran.r-project.org/web/packages/OpenImageR/vignettes/Image_segmentation_superpixels_clustering.html
#code for Time Series Analysis is based on: https://www.neonscience.org/raster-time-series
#code for K-Means Segmentation is based on: https://www.r-bloggers.com/color-quantization-in-r/

#This App is partitioned in 3 parts. The first part is available through choosing the tab "Image Segmentation".
#In this tab you can choose a png-image from your computer and have it segmented by using either the
#K-Means Segmentation or Super-Pixel Segmentation. K-Means Segmentation partitions the image into
#clusters. In each cluster the average color is calculated and all pixels in that cluster are set to
#the average color. The amount of clusters can be chosen via a slider-widget.
#In Super-Pixel Segmentation the image will be partitioned into Super-Pixels. A region of pixels with
#similar attributes represents one super-pixel. In this app every image is partitioned into 200 
#super-pixels. Afterwards the similarities of all super-pixels will be calculated. As there are far
#fewer super-pixels than pixels, this calculation can be done quite fast. In the end the super-pixels 
#are passed to the Affinity Propagation (AP) Algorithm for cluster creation. This app contains 2 
#Super-Pixel Algorithms, SLIC and SLICO. While SLIC creates super-pixels of different sizes due to its 
#compactness parameter, SLICO creates mostly equally sized super-pixels. For a better understanding the
#user may choose to show the Super-Pixel grid created by each method via the checkbox "Show SLIC-Grid".
#
#The second part of this app shows the color space of the image after the principal component analysis has been
#used on the image. The PCA algorithm transforms the RGB color space into a new uvw-coordinate system.
#The u-coordinate is responsible for capturing as much variance from the original image as possible.
#Afterwards the u coordinate will be factored out and in the resulting image the v coordinate is responsible 
#for capturing as much variance as possible. The image is then plotted in the uv-plane. During testing phase 
#several problems occured with the k-means algorithm, which led to the decision that a static cluster number of
#4 clusters was chosen for the segmentation in this part.
#
#The third part of this app is independant of the former two parts. It analyses the Normalized Difference 
#Vegetation Index (NDVI) using a Time Series dataset. In short NDVI is the amount of greenness in an image. 
#This is useful in analysing the vegetational changes in a set area over a period of time. 
#This app uses the NEON Harvard Forest field site data. In the year 2011 in 13 days, throughout the 
#whole year, satellite images of the forest region were taken and are available. Those images can be 
#seen as original RGB images or compressed images.
#The compressed images are further used for creating a mean NDVI per image and plotted in a coordinate
#system. At last the user can choose to view the histogram of the different NDVI's per day.
#In the compressed-version it can be observed that days 277 and 293 show almost no amount of greenness
#while the day before and after those two has a NDVI above 0.5. The reason for this occurence can be 
#deducted in the original RGB-Image of days 277 and 293.

#Problems: For some strange reasons, the K-Means algorithm is having problems with some images that have been
#selected via the fileInput-Widget, even when the selected image is of the same dimensions. 
#These problems do not occur with the defualt image, the "mandrill.png". The SLIC-Method is not having these
#issues and can be tested on any image. 

# Source helper functions -----
source("helpers.R")



ui <- fluidPage(
    shinyjs::useShinyjs(),
    sidebarLayout(
          position = "right",
          sidebarPanel(
              fileInput("selectImage", "Select image"),
              radioButtons("channelChoices", label = "Choose color channel",
                           choices = list("show original" = 1, 
                                          "show red channel" = 2, 
                                          "show green channel" = 3,
                                          "show blue channel" = 4), 
                           selected = 1),
              radioButtons("seriesChoices", label = "Choose time series visualization",
                           choices = list("show mean NDVI" = 1,
                                          "show compressed version" = 2, 
                                          "show original version" = 3, 
                                          "show histogram" = 4
                                          ), 
                           selected = 1),
              hr(id="hr1"),
              checkboxInput("incorrectData", label = "Remove incorrect data", value = FALSE),
              uiOutput("timeSeriesInstance"),
              checkboxInput("brightness", label = "Maximize brightness", value = FALSE),
              checkboxInput("saturation", label = "Maximize saturation", value = FALSE),
              checkboxInput("clusterColorSpace", label = "Show K-Means Color Space (4 clusters)", value = FALSE),
              hr(id="hr2"),
              radioButtons("segmentationChoices", label = "Choose segmentation",
                           choices = list("None" = 1, 
                                          "K-Means" = 2, 
                                          "Super-Pixel" = 3), 
                           selected = 1),
              sliderInput("segmentClusters", label = h3("clusters"), min = 2, max = 10, value = 4),
              radioButtons("slicmethod", label = "Choose SLIC-Method", 
                           choices = list("SLIC" = 1, "SLICO" = 2), selected = 1),
              checkboxInput("slicGrid", label = "Show SLIC-Grid", value = FALSE),
          ),
          
          mainPanel(
              tabsetPanel(id = "tabsPanel", type = "tabs",
                          tabPanel("Image Segmentation", id = "imagePanel", value ="1", imageOutput("standardImage"),),
                          tabPanel("Color Space Analysis", id = "colSpacePanel", value ="2", 
                                   helpText("Important: When opening this tab for the first time the computation may take some time."), 
                                   plotOutput("colorSpacePlot")),
                          tabPanel("NDVI Analysis", id = "timeSeriesPanel", value ="3", 
                                   helpText("Important: When opening this tab for the first time the computation may take some time."),
                                   helpText("The NDVI-Analysis uses Time-Series-Data. NDVI is an index for the amount of 
                                            greenness in the range of 0.0 to 1.0. There is no connection to the previous
                                            tabs (segmentation or color space)."), 
                                   plotOutput("timeSeriesPlot")
                            )
              ),
         )
    )
)


server <- function(input, output, session) {
    
    #loads and returns the selected image. If none was selected, return default mandrill image
    selectedImg <- reactive({
        if(is.null(input$selectImage$datapath) == FALSE){
            return (readPNG(input$selectImage$datapath))
        } 
        return (readPNG("www/mandrill.png"))
    })
    
    #returns a vector with the path and name for every file in the time series folder 
    #Initially intended to load a selected folder, but there is no InputFolder-Widget available
    selectedTimeSeries <- reactive({
        return (loadTimeSeriesData("www/NEON-DS-Landsat-NDVI/HARV/2011/NDVI"))
    })
    
    #create the rasterstack for the time series
    timeSeriesData <- reactive({
        files <- selectedTimeSeries()
        createRasterStack(files)
    })
    
    
    observeEvent(input$seriesChoices, {
        if (input$seriesChoices == "1"){
            shinyjs::show("incorrectData")
        } else {
            shinyjs::hide("incorrectData")
        }
    })
    
    observeEvent(input$segmentationChoices, {
        if (input$segmentationChoices == "1"){
            shinyjs::hide("segmentClusters")
            shinyjs::hide("slicmethod")
            shinyjs::hide("slicGrid")
        } else if (input$segmentationChoices == "2"){
            shinyjs::show("segmentClusters")
            shinyjs::hide("slicmethod")
            shinyjs::hide("slicGrid")
        } else if (input$segmentationChoices == "3"){
            shinyjs::show("slicmethod")
            shinyjs::show("slicGrid")
            shinyjs::hide("segmentClusters")
        }
    })
    
    observeEvent(input$tabsPanel, {
        if(input$tabsPanel == "1"){
            showSegmentationPanel(input$segmentationChoices)
        }else if (input$tabsPanel == "2"){
            showColorSpacePanel(input$segmentationChoices)
        }else if (input$tabsPanel == "3"){
            showTimeSeriesPanel()
        }
    })
    
    
    #creates dynamic checkboxes for every element in the time series.
    output$timeSeriesInstance <- renderUI({
        if ((input$seriesChoices == "3" || input$seriesChoices == "4") && input$tabsPanel == "3") {
            names <- getDayVectorForTimeSeries(timeSeriesData())
            choice <-  unique(names)
            radioButtons("chooseDay", label = "Choose day", choices = choice, selected = choice[1]) 
        }
    })
    
    output$timeSeriesPlot <- renderPlot({
        data <- timeSeriesData()
        day <- "Day 1"
        if (is.null(input$chooseDay) == FALSE) {
            day <- input$chooseDay
        }
        
        if (input$seriesChoices == "1") {
            meanNDVI <- calculateMeanNDVIFromRaster(data)
            data <- formatMeanNDVIdata(meanNDVI)
            if (input$incorrectData){
                data <- removeIncorrectTimeSeriesData(data)
            }
            
            # plot NDVI
            return (ggplot(data, aes(day, meanNDVI), na.rm=TRUE) +
                        geom_point(size=4,colour = "SpringGreen4") + 
                        ggtitle("Landsat Derived NDVI - 2011\n NEON Harvard Forest Field Site") +
                        xlab("Days") + ylab("Mean NDVI") +
                        theme(text = element_text(size=20)))
        }else if (input$seriesChoices == "2"){
            #plot the compressed version
            return (getCompressedPlot(data))
        } else if (input$seriesChoices == "3") {
            #plot the original RGB-Image
            file <- getRealColorFilePathForTimeSeries(selectedTimeSeries(), timeSeriesData(), day)
            return (plotRGB(stack(file), stretch="lin"))
        } else if (input$seriesChoices == "4") {
            #plot a histogram of the time-series data
            idx <- getTimeSeriesIndex(data, day)
            return (hist(data[[idx]], xlim = c(0, 1), main = day, xlab = "NDVI"))
        }
    })
    
    output$colorSpacePlot <- renderPlot( {
        selectedImage <- selectedImg()
        
        #the details for each "process..." function can be seen in the helper.R file
        selectedImage <- processBrightness(selectedImage, input$brightness)
        selectedImage <- processSaturation(selectedImage, input$saturation)
        
        df <- transformImageToDataFrame(selectedImage)
        
        if (input$clusterColorSpace == TRUE) {
            #The kmeans method is having problems dealing with clusters greater or smaller than 4, 
            #so the segmentation is fixed in this case
            df <- segmentDataFrame(df, 4)
            df <- performPrincipalComponentsAnalysis(df)
            return (plotSegmentedImage(df))
        }
        
        df <- performPrincipalComponentsAnalysis(df)
        return (plotNonSegmentedImage(df))
    })
    
    output$standardImage <- renderImage({ 
        selectedImage <- selectedImg()
        
        #the details for each "process..." function can be seen in the helper.R file
        selectedImage <- processBrightness(selectedImage, input$brightness)
        selectedImage <- processSaturation(selectedImage, input$saturation)
        selectedImage <- processChannelChoices(selectedImage, input$channelChoices)
        selectedImage <- processSegmentation(selectedImage, input$segmentationChoices, 
                                             input$segmentClusters, input$slicmethod,
                                             input$slicGrid)
        
        
        # Generate the PNG
        outfile <- generateImage(img = selectedImage, extension = '.png')
        
        
        # Return a list containing the filename
        img_dim <- dim(selectedImage)
        list(src = outfile,
             contentType = 'image/png',
             width = img_dim[1],
             height = img_dim[2],
             alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    output$myText <- renderText( {
        dim(mandrill)[1]
    })
}

shinyApp(ui = ui, server = server)
