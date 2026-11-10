#' Rasterize Vegetation Inventory Shapefile
#'
#' @param vegInv_shapefile Character. The full path to the vegetation inventory shapefile
#' @param field Character. The shapefile field to rasterize.
#' @param output Character. The full path to the output location. If left blank, it will output in the working directory.
#' @param datatype Character. Use "INT2U" for integers and strings and "FLT2S" for decimals to improve efficiency.
#' @param resolution Integer. The resolution of the raster (default is 10)
#'
#' @returns A rasterized TIFF of the vegetation inventory shapefile. This tiff can be used as an input raster to the `generatePinningPlots_PlacePlots()` script.
#' @export
#'
#' @examples
#' rasterize_vegInv("C:\\Users\\MichaelBurnett\\LiDAR Inventory group Dropbox\\Project Data\\OR\\Sakaw\\EFI_2026\\SFVI\\Sakaw_SFVI_Dissolved.shp","MAIN_SPECI","C:\\Forsite\\Sakaw")
rasterize_vegInv <- function(vegInv_shapefile, field, output=getwd(), datatype="INT2U", resolution=10){
  if(isFALSE(grepl("\\.shp$",vegInv_shapefile,ignore.case=TRUE))){stop(paste0("Invalid input: ",vegInv_shapefile," must be a shapefile (.shp)!"))}
  vegInv_shp <- terra::vect(vegInv_shapefile)
  vegInv_raster <- terra::rast(vegInv_shp,resolution=resolution,crs=terra::crs(vegInv_shp))
  message("Rasterizing data...")
  vegInv_raster <- terra::rasterize(vegInv_shp,vegInv_raster,field=field, filename=file.path(output,gsub(".shp",".tif",basename(vegInv_shapefile))),wopt = list(datatype = datatype,gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2","TILED=YES", "NUM_THREADS=ALL_CPUS")),overwrite=TRUE)
}

