
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
#' It will keep your datasets and Datasets infomation.xlsx
#' 
#' @export
xzdb.update <- function(I_have_dataset=TRUE,forceInstall=FALSE) {
  library("remotes")
  
  if(I_have_dataset){
    xzdb.clean.file("TEMP_XZupdate")
    xzdb.nowDataset(ToName="TEMP_XZupdate")
  }
  
  detach("package:XZDBfunction", unload = TRUE)
  
  cat("\n\n------Updating from GitHub------\n\n")
  
  if(forceInstall){
    remotes::install_github("scottcalcalas/XZDBfunction",force = TRUE,upgrade = "never")
  }else{remotes::install_github("scottcalcalas/XZDBfunction")}
  
  cat("\n\n------Github files configure Completed------\n\n")
  
  library(XZDBfunction)
  cat("\n\n------Testing Completed------\n\n")
  
  if(I_have_dataset){
    xzdb.sync.to.shinyapp(DatasetfolderName="TEMP_XZupdate",xlsx.index.location = "TEMP_XZupdate/Datasets infomation.xlsx")
    xzdb.clean.file("TEMP_XZupdate")
    cat("Please delete folder 'TEMP_XZupdate' ")
  }
  
  cat("\n\n------Update Completed------\n\n")
  
}
