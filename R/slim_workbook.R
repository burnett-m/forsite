#' Slim Workbook
#'
#' Reads a large Excel file that may be too difficult to open in Excel and removes sheets from it based on a provided range.
#'
#' @param filename string: The filename for the Excel file including the file extension
#'
#' @returns A slimmed down Excel file with the selected sheets removed
#' @export slim_workbook
#'
#' @examples
#' #main_dir <- "C:/Forsite"
#' #setwd(main_dir)
#' #filename <- "WF_AreaD_AllSpecies_Scorecard_Nargess.xlsm"
#' #slim_workbook(filename)
slim_workbook <- function(filename){
  file_extension <- paste0(".",sub("^[^.]*\\.", "", filename)) # Get file extension
  wb <- openxlsx2::wb_load(filename) # Read workbook

  # Get workbook sheet names
  wb_names <- wb$get_sheet_names()
  for(i in seq_along(wb_names)){
    cat(i,":",wb_names[i],"\n")
  }

  # Get list of sheets to delete
  cat("\n","Provide the range for workbook sheets to delete")
  start_sheet <- as.numeric(readline("Enter first sheet index :"))
  end_sheet <- as.numeric(readline("Enter last sheet index :"))
  range <- seq(start_sheet,end_sheet)

  # Delete sheets
  wb$workbook$definedNames <- list() # Give defined names a list() rather than NULL
  for(sheet in wb_names[range]){
    wb <- openxlsx2::wb_remove_worksheet(wb,sheet)
  }

  openxlsx2::wb_save(wb,gsub(file_extension,paste0("_filtered",file_extension),filename),overwrite = TRUE)
}
