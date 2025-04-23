#' Calculate Canopy Volume
#'
#' @param lazFiles filenames(list of strings): Set the working directory to where the LAZ files are located and access a list of LAZ files
#' @param shapefile filename(string): Full path to the initial shapes shapefile matching the UNIQUE_ID with the LAZ files, and it must contain ELEVATION and HEIGHT data
#' @param threshold numeric: The height at which to clip all LAZ points below it out.
#' @param outputDir directory(string): Full path to the output location.
#' @param OverstrikeColumn column name (string): The name of the column containing the overstrike distance (perhaps it is called FallD2Line in post-intrusion codes production) to add to the threshold to generate a new canopy volume based on threshold + overstrike distance. This number is generally negative.
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
calculateCanopyVolume <- function(lazFiles, shapefile, threshold, outputDir, OverstrikeColumn = "NONE"){
  initialShapes <- sf::read_sf(shapefile) # Read shapefile
  initialShapes$CANOPYVOL <- 0 # Generate empty canopy volume field
  for(i in lazFiles){
    temp_laz <- lidR::readLAS(i) # Read LAZ
    stem_elevation <- initialShapes$ELEVATION[initialShapes$UNIQUE_ID == gsub(".laz","",i)] # Access stem elevation
    stem_height <- initialShapes$HEIGHT[initialShapes$UNIQUE_ID == gsub(".laz","",i)] - threshold # Calculated Peak_XY height minus the powerline height
    if(OverstrikeColumn == "NONE"){
      laz_above_threshold <- temp_laz[temp_laz@data$Z > stem_elevation + threshold] # Remove all LAZ below threshold
    }
    else{
      summary_threshold <- as.numeric((initialShapes[OverstrikeColumn][initialShapes$UNIQUE_ID == gsub(".laz","",i),] * -1)[1] + threshold)
      laz_above_threshold <- temp_laz[temp_laz@data$Z > stem_elevation + summary_threshold] # Remove all LAZ below threshold
    }
    if(length(laz_above_threshold@data$X) < 4){next} # You need at least 4 points for the stem to be relevant
    chull_poly <- lidR::st_concave_hull(laz_above_threshold) # Calculate convex hull
    mrr <- sf::st_minimum_rotated_rectangle(chull_poly) # Get the minimum bounding box, used to generate major and minor axes
    mrr_coords <- sf::st_coordinates(mrr) # Get coordinates of bbox
    side_lengths <- numeric(4) # List of 4
    for(SL in c(1:4)){ # Populate side_lengths with the length of the axes
      side_lengths[SL] <- sf::st_distance(sf::st_point(mrr_coords[SL,1:2]),sf::st_point(mrr_coords[SL+1,1:2]))
    }
    # Access major and minor axes
    major_axis_length <- max(side_lengths)
    minor_axis_length <- min(side_lengths)
    # Calculate canopy volume and add to the shapefile
    canopyVol <- (2/3) * pi * stem_height * ((major_axis_length / 2)*(minor_axis_length / 2))
    initialShapes$CANOPYVOL[initialShapes$UNIQUE_ID==gsub(".laz","",i)] <- canopyVol
    # Export LAZ minus threshold for review
    lidR::writeLAS(laz_above_threshold,paste0(outputDir,"\\",i))
  }
  # Export shapefile
  sf::write_sf(initialShapes,paste0(outputDir,"\\initialShapes_withCanopyVolume.shp"),layer_options="SHPT=POLYGONZ")
}
