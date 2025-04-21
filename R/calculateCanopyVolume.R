#' Calculate Canopy Volume
#'
#' @param lazFiles filenames(list of strings): Set the working directory to where the LAZ files are located and access a list of LAZ files
#' @param shapefile filename(string): Full path to the initial shapes shapefile matching the UNIQUE_ID with the LAZ files, and it must contain ELEVATION and HEIGHT data
#' @param threshold numeric: The height at which to clip all LAZ points below it out.
#' @param outputDir directory(string): Full path to the output location.
#'
#' @returns Clipped LAZ and an updated shapefile containing the CANOPYVOL field
#' @export calculateCanopyVolume
#'
#' @examples
#' lazDir <- readClipboard() # Copy the path to the LAZ files from File Explorer
#' setwd(lazDir)
#' lazFiles <- list.files(pattern="*.laz")
#' shapefile <- "initialShapes.shp"
#' dir.create("output")
#' calculateCanopyVolume(lazFiles,shapefile,threshold=10,outputDir=paste0(getwd(),"//output"))
calculateCanopyVolume <- function(lazFiles, shapefile, threshold, outputDir){
  initialShapes <- sf::read_sf(shapefile)
  initialShapes$CANOPYVOL <- 0
  for(i in lazFiles){
    temp_laz <- lidR::readLAS(i)
    stem_elevation <- initialShapes$ELEVATION[initialShapes$UNIQUE_ID == gsub(".laz","",i)]
    stem_height <- initialShapes$HEIGHT[initialShapes$UNIQUE_ID == gsub(".laz","",i)] - threshold # Calculated Peak_XY height minus the powerline height
    laz_above_threshold <- temp_laz[temp_laz@data$Z > stem_elevation + threshold]
    if(length(laz_above_threshold@data$X) < 4){next}
    chull_poly <- lidR::st_concave_hull(laz_above_threshold)
    mrr <- sf::st_minimum_rotated_rectangle(chull_poly)
    mrr_coords <- sf::st_coordinates(mrr)
    side_lengths <- numeric(4)
    for(SL in c(1:4)){
      side_lengths[SL] <- sf::st_distance(sf::st_point(mrr_coords[SL,1:2]),sf::st_point(mrr_coords[SL+1,1:2]))
    }
    major_axis_length <- max(side_lengths)
    minor_axis_length <- min(side_lengths)

    canopyVol <- (2/3) * pi * stem_height * ((major_axis_length / 2)*(minor_axis_length / 2))
    initialShapes$CANOPYVOL[initialShapes$UNIQUE_ID==gsub(".laz","",i)] <- canopyVol
    lidR::writeLAS(laz_above_threshold,paste0(outputDir,"\\",i))
  }
  sf::write_sf(initialShapes,paste0(outputDir,"\\initialShapes_withCanopyVolume.shp"),layer_options="SHPT=POLYGONZ")
}
