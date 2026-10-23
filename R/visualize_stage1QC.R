#' visualize_stage1QC
#' Visualizing the stage1 results for poor quality polys
#'
#' @param productionPolys string: Full path to the production polys shapefile
#' @param mergedCSV string: Full path to the _mergedCSV CSV file
#' @param s3Bucket string: S3 Bucket name
#' @param s3LAZ_dir string: Full path (without the S3 bucket name) to the clean LAZ (with the tail "/")
#' @param s3stage1_dir string: Full path (without the S3 bucket name) to the stage1 (with the tail "/")
#' @param s3_stage1QC_dir string: Full path (without the S3 bucket name) to the stage1 QC CSVs and JPEG (with the tail "/")
#' @param output string: Full path to the output location in the format R wants to read it in
#' @param UncoveredThreshold numeric: The minimum uncovered percentage threshold to subset the production polys
#'
#' @details
#' This tool generates a shiny app that displays the subset of production polys within the threshold of uncovered stage1 polygons.
#'
#'
#' @examples
#' # Copy the output directory from your File Explorer to your clipboard
#' output <- readClipboard()  # Adds the output in the format R needs
#' # Copy path to the project's production shapefiles SHP
#' readClipboard() # This prints the full path to the console
#' productionShapefiles <- "C:\\Forsite\\LakeBabine\\Lake_Babine_ProductionPolys.shp"
#' # Repeat the same step for the _MergedCSV.csv output from the stage1QCbatch job after running csv_merge
#' #mergedCSV <- "C:\\Forsite\\LakeBabine\\stage1_multiseg\\burnt_areas_SegCal\\Merged_QC\\_MergedCSV.csv"
#'
#' #visualize_stage1QC(productionShapefiles,
#'  # mergedCSV,
#'  # "lake.babine.2016-2",
#'  # "production/clean_las/clean/",
#'  # "production/stage1_multiseg/",
#'  # "production/stage1_multiseg/_QC/",
#'  # output,
#'  # UncoveredThreshold = 40)
#'

visualize_stage1QC <- function(productionPolys,mergedCSV,s3Bucket,s3LAZ_dir,s3stage1_dir,s3_stage1QC_dir,output,UncoveredThreshold = 50){
  checkOutput <- substr(output,nchar(output)-1,nchar(output))
  mergedCSV_df <- read.csv(mergedCSV)
  # Error messages
  if(isTRUE(grepl("/",output))){
    stop("Please copy the output directory from your File Explorer into your clipboard and use the readClipboard() function to print the directory in the format R wants to read it in.")
  }
  if(isFALSE(file.exists(productionPolys))){
    stop(paste0("Can not find file : ",productionPolys,". Please use the readClipboard() function when you copy file path and clean it in the format R wants to read it in."))
  }
  if(isFALSE(file.exists(mergedCSV))){
    stop(paste0("Can not find file : ",mergedCSV,". Please use the readClipboard() function when you copy file path and clean it in the format R wants to read it in."))
  }
  if(isFALSE(!c("PRODGRIDID","UncoveredArea_Percent") %in% colnames(mergedCSV_df))){
    stop("Please make sure that PRODGRIDID and UncoveredArea_Percent are column names in the mergedCSV file")
  }

  # Check that output is formatted correctly
  if(isFALSE(grepl("\\\\",checkOutput))){
    output <- paste0(output,"\\")
  }

  # Function for downloading from the S3
  download_stage1QC <- function(poly){
    stage1_prefix <- paste0("aws s3 cp s3://",s3Bucket,"/",s3stage1_dir)
    stage1_suffix <- paste0("InitialShapes_s1.zip ",output)
    stage1_QC_prefix <- paste0("aws s3 cp s3://",s3Bucket,"/",s3_stage1QC_dir)
    stage1_QC_suffix <- paste0("_CHMcoverage.jpeg ",output)
    laz_prefix <- paste0("aws s3 cp s3://",s3Bucket,"/",s3LAZ_dir)
    laz_suffix <- paste0(".laz ",output)
    system(paste0(stage1_prefix,poly,stage1_suffix))
    system(paste0(stage1_QC_prefix,poly,stage1_QC_suffix))
    system(paste0(laz_prefix,poly,laz_suffix))
    unzip(paste0(output,poly,"InitialShapes_s1.zip"),exdir=output)
    print(mergedCSV_df[mergedCSV_df$PRODGRIDID==poly,])
  }

  # Create production polys subset
  prodPolys <- sf::read_sf(productionPolys)

  mergedCSV_df_subset <- mergedCSV_df[mergedCSV_df$UncoveredArea_Percent > UncoveredThreshold,]
  prodPolys_subset <- prodPolys[prodPolys$PRODGRIDID %in% mergedCSV_df_subset$PRODGRIDID,]
  prodPolys_subset <- sf::st_transform(prodPolys_subset,4326) # Convert to Lat Long
  prodPolys_subset <- sf::st_make_valid(prodPolys_subset) # Ensure geometry is valid

  # Prepare the shiny app
  ui <- shiny::fluidPage(leaflet::leafletOutput("map"))
  server <- function(input, output, session){
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(prodPolys_subset) |>
        #addTiles() %>% # Default Base
        leaflet::addProviderTiles("Esri.WorldImagery")|> # Add ESRI World Imagery
        leaflet::addPolygons(layerId=prodPolys_subset$PRODGRIDID,
                    color="red",
                    weight=2,
                    fillOpacity = 0,
                    label=prodPolys_subset$PRODGRIDID)
    })
    shiny::observeEvent(input$map_shape_click,{
      click <- input$map_shape_click
      if(!is.null(click$id)){download_stage1QC(click$id)}
    })
  }
  shiny::shinyApp(ui,server)
}

# visualize_stage1QC("C:\\Forsite\\Interfor\\INTERFOR_ProductionPolys.shp",
#                    "C:\\Forsite\\Interfor\\stage1_qc\\_MergedCSV.csv",
#                    "interfor.1686-7",
#                    "Production/clean_las_year1/clean/",
#                    "Production/stage1/",
#                    "Production/stage1/SegmentationQC/",
#                    "C:\\Forsite\\Interfor\\stage1_qc\\out",
#                    UncoveredThreshold = 70)
#
# visualize_stage1QC("C:\\Forsite\\LakeBabine\\Lake_Babine_ProductionPolys.shp",
#                    "C:\\Forsite\\LakeBabine\\stage1_multiseg\\burnt_areas_SegCal\\Merged_QC\\_MergedCSV.csv",
#                    "lake.babine.2016-2",
#                    "production/clean_las/clean/",
#                    "production/stage1_multiseg/",
#                    "production/stage1_multiseg/_QC/",
#                    "C:\\Forsite\\LakeBabine\\stage1_multiseg\\burnt_areas_SegCal\\Merged_QC\\out",
#                    UncoveredThreshold = 40)
