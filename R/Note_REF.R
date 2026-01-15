
# Generate roxygen2 note: Run next line
# library(devtools)
# devtools::document()

# If want everything inside the package, put next line at NAMESPACE
# exportPattern(".")



#' Update this XZDBfunction package
#'
#' @description
#' Update this XZDBfunction package
#' 
#' @export
XZ.update <- function() {
  library("remotes")
  detach("package:XZDBfunction", unload = TRUE)
  remotes::install_github("scottcalcalas/XZDBfunction")
  
}
