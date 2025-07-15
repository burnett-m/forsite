#' Display Configuration Logs - Voxel Segmentation Calibration
#'
#' @param resultsDirectory directory(string) : Directory containing all of the folders for the results
#'
#' @returns segParameters data frame containing all of the logs
#' @export
#'
#' @examples
#' #voxelSegCal_DisplayConfigLog("C:\\Tolko\\Greensled\\BaseTarget")
voxelSegCal_DisplayConfigLog <- function(resultsDirectory){
  setwd(resultsDirectory)
  folds <- list.dirs(recursive = FALSE)
  folds <- folds[order(as.numeric(gsub("\\D","",folds)))] # Makes the order of the folders proper
  for(i in folds){
    setwd(i)
    segmentation_cfg <- ini::read.ini(list.files(pattern="*.cfg"))
    if(i == folds[1]){
      segParameters <- as.data.frame(segmentation_cfg$`Seg Params`)
    }
    else{
      segParameters <- rbind(segParameters,as.data.frame(segmentation_cfg$`Seg Params`))
    }
    rownames(segParameters)[which(folds == i)] <- gsub("./","",i)
    setwd("..")
  }
  return(segParameters)
}
