#' Ground Truth Quality Control
#'
#' Used from your respective _inProgress folder to attach your name to the boxes and display all items simultaneously up until 120 RGL windows (and then stop since RGL is prone to crashing if too many windows are open)
#'
#' @param parentDirectory string. The full '_inProgress' directory after your user name. This process will rename every box in this folder with "_"+username.
#'
#' @return
#' @export
#'
#' @examples
gtQC <- function(parentDirectory){
  setwd(parentDirectory)
  folds <- list.files()

}
