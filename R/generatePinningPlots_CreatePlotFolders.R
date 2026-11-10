#' Generate Pinning Plots: Create Plot Folder
#'
#' This is the third step in the process, after `generatePinningPlots_PlacePlots()`
#'
#' @param plotOutlines_shapefile Character. The full path to the plot outlines shapefile outputted from generatePinningPlots_PlacePlots()
#' @param output Character. The full path to the output directory.
#' @param LASFromPoly_dir Character. The full path to the most recent LASFromPoly EXE
#' @param productionPolygons_shapefile Character. The full path to the production polygons shapefile
#' @param aws_LAZ_directory Character. The full S3 path (including bucket) to the directory containing the processed LAZ files
#' @param projectName Character. The project name to be used as prefix for all plot folders and names (without trailing underscore).
#'
#' @returns
#' This script produces a series of plots for pinning
#'
#' @examples
#' #generatePinningPlots_CreatePlotFolders("C:\\Forsite\\Fugro_NorthernIreland\\PlotPlacement\\StrataSample_plotOutlines2.shp",
#' #"C:\\Forsite\\Fugro_NorthernIreland\\PlotPlacement\\output",
#' #"C:\\Users\\MichaelBurnett\\LiDAR Inventory group Dropbox\\Project Data\\OR\\_TSI30Release\\LASFromPoly_6.exe",
#' #"C:\\Forsite\\Fugro_NorthernIreland\\production_shapefiles\\NIE_ProductionPolys.shp",
#' #"fugro.northern.ireland.1547-14/Production/LAZ/05_clean_laz_step2/cleanedLAZ/",
#' #"NIE_phase2")
#'
generatePinningPlots_CreatePlotFolders <- function(plotOutlines_shapefile, output, LASFromPoly_dir, productionPolygons_shapefile, aws_LAZ_directory, projectName){
  message("Reading shapefiles...")
  plotOutline <- sf::read_sf(plotOutlines_shapefile)
  prodPolys <- sf::read_sf(productionPolygons_shapefile)
  #if(is.na(sf::st_crs(plotOutline)) || isFALSE(sf::st_crs(plotOutline) == sf::st_crs(prodPolys))){stop("Plot outline shp has no defined CRS. Identify its original CRS first!")}
  tryCatch(plotOutline <- sf::st_transform(plotOutline,sf::st_crs(prodPolys)),
           error = function(e) {
              sf::st_crs(plotOutline) <- sf::st_crs(prodPolys)
              plotOutline <- sf::st_transform(plotOutline,sf::st_crs(prodPolys))
              plotOutline
           })  # Reproject, force if necessary
  # plotOutline <- sf::st_transform(plotOutline,sf::st_crs(prodPolys))
  plotOutline$NAME <- paste0(projectName,"_",1:nrow(plotOutline))
  plotOutline$BASE_OBJEC <- plotOutline$NAME
  buffered_plotOutline <- sf::st_buffer(plotOutline,3)
  buffered_plotOutline$NAME <- paste0(buffered_plotOutline$NAME,"_buffered")
  buffered_plotOutline$BASE_OBJEC <- buffered_plotOutline$NAME
  buffered_plotOutline_filename <- file.path(dirname(plotOutlines_shapefile),paste0(projectName,"_plotOutlines_buffered.shp"))
  message(paste0("Writing buffered plot outlines to ",buffered_plotOutline_filename))
  sf::st_write(buffered_plotOutline,buffered_plotOutline_filename,delete_dsn=TRUE)
  sf::st_write(plotOutline,plotOutlines_shapefile,delete_dsn = TRUE)

  message("Collecting LAZ")
  tryCatch(joinedData <- sf::st_intersection(buffered_plotOutline,prodPolys),
           error = function(e){
             joinedData <- sf::st_intersection(sf::st_transform(buffered_plotOutline,sf::st_crs(prodPolys)),prodPolys)
             joinedData
           })
  joinedData <- sf::st_intersection(buffered_plotOutline,prodPolys)
  unique_polyNames <- unique(joinedData$PRODGRIDID)
  lazSource_dir <- file.path(output,"SourceLAZ")
  dir.create(lazSource_dir)
  beepr::beep(8)
  readline("Did you run 'aws sso login' already in the past hour? If not, do it now!! [Hit Enter to proceed]")
  for(i in unique(joinedData$PRODGRIDID)){
    system(paste0("aws s3 cp s3://",
                  aws_LAZ_directory,
                  i,
                  ".laz ",
                  lazSource_dir,
                  "//"))
  }
  dir.create(file.path(output,"Plot_LAZ"))
  dir.create(file.path(output,"Buffered_Plot_LAZ"))
  dir.create(file.path(output,"final_plots"))

  message("Clipping Las From Poly...")
  system2(LASFromPoly_dir,c(lazSource_dir,plotOutlines_shapefile,file.path(output,"Plot_LAZ")))
  system2(LASFromPoly_dir,c(lazSource_dir,buffered_plotOutline_filename,file.path(output,"Buffered_Plot_LAZ")))

  message("Generating plot folders...")
  for(pName in plotOutline$NAME){
    temp_shp <- plotOutline[plotOutline$NAME == pName,]
    dir.create(file.path(output,"final_plots",pName))
    sf::write_sf(temp_shp,file.path(output,"final_plots",pName,paste0(pName,"_PlotOutline.shp")))
    file.copy(file.path(output,"Plot_LAZ",paste0(pName,".laz")),file.path(output,"final_plots",pName,paste0(pName,".laz")))
    file.copy(file.path(output,"Buffered_Plot_LAZ",paste0(pName,"_buffered.laz")),file.path(output,"final_plots",pName,paste0(pName,"_buffered.laz")))
  }


}

