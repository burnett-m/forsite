#' Change Z Geometry for a shapefile based on HEIGHT column
#' Uses the numerical values in the HEIGHT column to add to its present elevation (Z) data to present polygons that are Z + HEIGHT in elevation.
#'
#' @param shapefile filename : The input shapefile that you would like to increase the Z geometry for, based on the HEIGHT column. This shapefile must contain a HEIGHT column.
#' @param output filename : The output location for the "fixed" shapefile.
#'
#' @return shapefile
#' @export changeZgeometry_fromHeight
#'
#' @examples
#' shpDir <- readClipboard() # Read the directory path of the folder containing the shapefile you would like to fix
#' setwd(shpDir)
#' files <- list.files(pattern="*.shp")
#' changeZgeometry_fromHeight(files[1]) # Fixes the Z geometry of the first shapefile listed in that directory
changeZgeometry_fromHeight <- function(shapefile,output = gsub(".shp","_fixedZ.shp",shapefile)){
  shp <- sf::read_sf(shapefile)
  if("HEIGHT" %in% colnames(shp)){
    height_additions <- shp$HEIGHT
    for(i in c(1:length(height_additions))){
      temp_subset <- shp[i,]
      temp_coord <- sf::st_coordinates(temp_subset)
      temp_coord[,3] <- temp_coord[,3] + height_additions[i]

      #a polygon from coords
      temp_poly <- sf::st_polygon(x = list(temp_coord[, 1:3])) %>%  # X,Y,Z
        sf::st_sfc() %>%  # from sfg to sfc
        sf::st_sf() # from sfc to sf
      sf::st_geometry(temp_subset) <- sf::st_geometry(temp_poly) # write geometry back into original data
      sf::st_crs(temp_subset) <- sf::st_crs(shp)
      sf::st_geometry(shp[i,]) <- sf::st_geometry(temp_subset)
    }
    sf::write_sf(shp,output,layer_options="SHPT=POLYGONZ")
  }
  else{
    errorCondition("Use a shapefile with a HEIGHT column")
  }
}
