#' New Segmentation - Voxel Segmentation Calibration
#'
#' @param CFG_filename filename(string) : Full path to the input CFG file to run the segmentation from
#' @param resultsDirectory directory(string) : Directory containing all of the folders for the results
#' @param voxelSegmentationEXE_directory filename(string) : Full path to the VoxelSegmentationCPU.exe with all helper files in the same directory
#' @param Dropbox_directory directory(string) : Directory on Dropbox where to copy the new target folder to
#' @param allPinTestSummary_directory filename(string) :  Full path to the CSV containing the statistical results based on POIs
#'
#' @returns New voxel segmentation with statistical results from the main columns
#'
#' @examples
#' #cfgFilename <- "C:\\Forsite\\Tolko\\GreenSled_voxelSeg\\_TolkoGS_NewFormat_Voxel_SegCal.cfg"
#' #resultsDirectory <- "C:\\Forsite\\Tolko\\GreenSled_voxelSeg\\BaseTarget"
#' #voxelSegEXE_dir <- "C:\\Forsite\\voxelSeg_training\\VoxelSegEXE\\VoxelSegmentationCPU.exe"
#' #dropbox_dir <- "C:\\Users\\MichaelBurnett\\LiDAR Inventory group Dropbox\\Project Data\\OR\\Tolko\\Tolko_Sask\\_GreenSled\\SegCal\\_Results"
#' #allPinTestSummary_dir <- "C:\\Forsite\\Tolko\\GreenSled_voxelSeg\\BaseTarget\\all_Pin_All_Test_Summary_No_Edge.csv"
#' #voxelSegCal_NewSegmentation(cfgFilename,resultsDirectory,voxelSegEXE_dir,allPinTestSummary_dir)
#'
voxelSegCal_NewSegmentation <- function(CFG_filename, resultsDirectory, voxelSegmentationEXE_directory, Dropbox_directory, allPinTestSummary_directory){
  # Function for updating the CFG target directory name
  create_new_target_dir <- function(target_path) {
    target_basename <- basename(target)
    target_dirname <- dirname(target)

    # Filter only those matching "target" followed by a number
    target_dirs <- grep("^target\\d+$", target_basename, value = TRUE)

    # Extract numbers
    target_nums <- as.numeric(gsub("target", "", target_dirs))

    # Get next number
    next_num <- if (length(target_nums) == 0) 1 else max(target_nums) + 1

    # Create new directory name
    new_dir_name <- paste0("target", next_num)
    new_dir_path <- file.path(target_dirname, new_dir_name)
    new_dir_path <- paste0(gsub("/","\\\\",new_dir_path),"\\")

    return(new_dir_path)
  }

  cfg <- ini::read.ini(CFG_filename) # Read CFG file
  segParams <- colnames(as.data.frame(cfg$`Seg Params`))
  target <- cfg$`File/Directory Config`$TARGETDIR
  newTargetDir <- create_new_target_dir(target)
  cfg$`File/Directory Config`$TARGETDIR <- newTargetDir

  base_target <- readline("Which target number should we base this new segmentation on?")
  setwd(paste0(resultsDirectory,"\\target",base_target))
  base_target_CFG <- ini::read.ini(list.files(pattern="*.cfg"))
  cfg$`Seg Params` <- base_target_CFG$`Seg Params`

  segParams <- colnames(as.data.frame(cfg$`Seg Params`))
  print(segParams)
  parameter_to_tweak <- readline("Which parameter NUMBER should we adjust?")
  print(cfg$`Seg Params`[segParams[as.numeric(parameter_to_tweak)]])
  parameter_value <- readline("What should we change it to?")

  cfg$`Seg Params`[segParams[as.numeric(parameter_to_tweak)]] <- parameter_value
  ini::write.ini(cfg,cfgFilename)

  setwd(dirname(voxelSegmentationEXE_directory))
  system(paste(basename(voxelSegmentationEXE_directory),"-OP_TYPE=1",cfgFilename,sep=" "))

  setwd(resultsDirectory)
  newTargetDir_split <- strsplit(newTargetDir,"\\\\")[[1]]
  newTargetDir_newTarget <- newTargetDir_split[length(newTargetDir_split)]
  fs::dir_copy(newTargetDir_newTarget, Dropbox_directory)

  allPinTestSummary <- read.csv(allPinTestSummary_directory)
  knitr::kable(allPinTestSummary[c(1:15)])

}

