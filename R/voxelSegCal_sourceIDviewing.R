#' Source ID Viewing for Voxel Segmentation LAS
#'
#' @param resultsDirectory directory : The full directory to the location of the RESULTS from the voxel segmentation (The parent directory of the target segmentation folders)
#' @param targetDir string : The folder name for the segmentation results to be displayed
#' @param plot_prefix string : The prefix to each plot. Please include every character up until the number for the plot (for example: "Tolko_Greensled_Plot_"). The "_DISPLAY.laz" is implied.
#' @param plotNUM numerical : The plot number to be displayed
#'
#' @returns An RGL window displaying the classified point cloud for the plot.
#'
#' @examples
#' resultsDirectory <- "C:\\Forsite\\Tolko\\GreenSled_voxelSeg\\BaseTarget"
#' targetDir <- "target36"
#' plot_prefix <- "Tolko_Greensled_Plot_"
#' plotNUM <- 100
#'
#' voxelSegCal_sourceIDviewing(resultsDirectory,targetDir,plot_prefix, plotNUM)



voxelSegCal_sourceIDviewing <- function(resultsDirectory, targetDir,plot_prefix, plotNUM){
  setwd(paste0(resultsDirectory,"//",targetDir))
  temp_laz <- lidR::readLAS(paste0(plot_prefix,as.character(plotNUM),"_DISPLAY.laz"))

  # Prepare the classification
  source_ids <- unique(temp_laz@data$PointSourceID)
  n <- length(source_ids)
  classification_map <- setNames(((seq_along(source_ids) - 1) %% 30) + 1, source_ids) # Create mapping: SourceID → Classification (1 to 16, cycling)
  temp_laz@data$Classification <- classification_map[as.character(temp_laz@data$PointSourceID)] # Apply the mapping to the LAS data
  #print(unique(temp_laz@data$Classification)) # ✅ Optional: check unique classifications assigned
  temp_laz@data$Classification <- as.integer(temp_laz@data$Classification)

  custom_palette <- c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
    "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
    "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
    "#E5C494", "#B3B3B3", "#1B9E77", "#D95F02", "#7570B3",
    "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666",
    "#A1D99B", "#9ECAE1", "#FDB462", "#BC80BD", "#CCEBC5"
  )
  lidR::plot(temp_laz, size = 7, color = "Classification", pal = custom_palette) # Plot using RGB color mode
}
