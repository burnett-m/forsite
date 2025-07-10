#' Source ID Viewing for Voxel Segmentation LAS
#'
#' @param mainDir directory : The full directory to the location of the RESULTS from the voxel segmentation (The parent directory of the target segmentation folders)
#' @param targetDir string : The folder name for the segmentation results to be displayed
#' @param plot_prefix string : The prefix to each plot. Please include every character up until the number for the plot (for example: "Tolko_Greensled_Plot_"). The "_DISPLAY.laz" is implied.
#' @param plotNUM numerical : The plot number to be displayed
#'
#' @returns An RGL window displaying the classified point cloud for the plot.
#'
#' @examples
#' mainDir <- "C:\\Forsite\\Tolko\\GreenSled_voxelSeg\\BaseTarget"
#' targetDir <- "target36"
#' plot_prefix <- "Tolko_Greensled_Plot_"
#' plotNUM <- 100
#'
#' sourceIDviewing(mainDir,targetDir,plot_prefix, plotNUM)



sourceIDviewing <- function(mainDir, targetDir,plot_prefix, plotNUM){
  setwd(paste0(mainDir,"//",targetDir))
  temp_laz <- lidR::readLAS(paste0(plot_prefix,as.character(plotNUM),"_DISPLAY.laz"))

  # Prepare the classification
  source_ids <- unique(temp_laz@data$PointSourceID)
  n <- length(source_ids)
  classification_map <- setNames(((seq_along(source_ids) - 1) %% 16) + 1, source_ids) # Create mapping: SourceID → Classification (1 to 16, cycling)
  temp_laz@data$Classification <- classification_map[as.character(temp_laz@data$PointSourceID)] # Apply the mapping to the LAS data
  #print(unique(temp_laz@data$Classification)) # ✅ Optional: check unique classifications assigned
  temp_laz@data$Classification <- as.integer(temp_laz@data$Classification)

  lidR::plot(temp_laz, size = 7, color = "Classification") # Plot using RGB color mode
}
