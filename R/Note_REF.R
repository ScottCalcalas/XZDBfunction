
# Generate roxygen2 note: Run next line
# library(devtools)
# devtools::document()

# If want everything inside the package, put next line at NAMESPACE
# exportPattern(".")



#' Update this XZDBfunction package
#'
#' @description
#' Update this XZDBfunction package, Automatic detaching and attaching current XZDB.
#' 
#' If you have your own dataset, use: I_have_dataset=TRUE
#' 
#' @export
XZ.update <- function(I_have_dataset=FALSE) {
  library("remotes")
  detach("package:XZDBfunction", unload = TRUE)
  
  if(I_have_dataset){
    xiaopei.clean.file("TEMP_XZupdate")
    xzdb.nowDataset(ToName="TEMP_XZupdate")
  }
  
  remotes::install_github("scottcalcalas/XZDBfunction")
  
  if(I_have_dataset){
    xiaopei.sync.to.shinyapp(DatasetfolderName="TEMP_XZupdate")
    xiaopei.clean.file("TEMP_XZupdate")
    cat("Please delete folder 'TEMP_XZupdate' ")
  }
  
  cat("\n\n------Update Completed------\n\n")
  library(XZDBfunction)
  
}
