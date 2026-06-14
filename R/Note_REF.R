
# Generate roxygen2 note: Run next line
# library(devtools)
# devtools::document()

# If want everything inside the package, put next line at NAMESPACE
# exportPattern(".")



#' Update this MODBrowser package
#'
#' @description
#' Update this MODBrowser package, Automatic detaching and attaching current MODB.
#' 
#' If you have your own dataset, use: I_have_dataset=TRUE
#' It will keep your datasets and Datasets infomation.xlsx
#' 
#' @param I_have_dataset Logical. Whether to preserve and restore the current
#'   datasets while updating the package.
#' @param forceInstall Logical. Passed to `remotes::install_github()` to force a
#'   reinstall from GitHub.
#'
#' @export
modb.update <- function(I_have_dataset=TRUE,forceInstall=FALSE) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    stop("Package 'remotes' is required to update MODBrowser.", call. = FALSE)
  }
  
  if(I_have_dataset){
    modb.clean.file("TEMP_MODupdate")
    modb.nowDataset(ToName="TEMP_MODupdate")
  }
  
  detach("package:MODBrowser", unload = TRUE)
  
  cat("\n\n------Updating from GitHub------\n\n")
  
  if(forceInstall){
    remotes::install_github("ScottCalcalas/XZDBfunction",force = TRUE,upgrade = "never")
  }else{remotes::install_github("ScottCalcalas/XZDBfunction")}
  
  cat("\n\n------Github files configure Completed------\n\n")
  
  library(MODBrowser)
  cat("\n\n------Testing Completed------\n\n")
  
  if(I_have_dataset){
    modb.sync.to.shinyapp(DatasetfolderName="TEMP_MODupdate",xlsx.index.location = "TEMP_MODupdate/Datasets infomation.xlsx")
    modb.clean.file("TEMP_MODupdate")
    cat("Please delete folder 'TEMP_MODupdate' ")
  }
  
  cat("\n\n------Update Completed------\n\n")
  
}
