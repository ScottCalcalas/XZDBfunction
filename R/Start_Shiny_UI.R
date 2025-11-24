# start.R — one-click bootstrapper for XZ DB Browser
# Project layout:
#   ProjectRoot/
#     App_Info/           (app.R, XZ_DB_functions.r, datasets/, IndexedData/, searching_*.csv)
#     Output/             (exports)
#     Start.R             (this file)



#' Run UI: Launch the Genomic DB Browser Shiny Application
#'
#' @description
#' Starts the Shiny-based **Genomic DB Browser** included within the
#' `XZDBfunction` package.  
#' This function automatically:
#' - Locates the packaged Shiny application directory,
#' - Ensures required R packages are installed,
#' - Selects an available random port,
#' - Launches the UI in the user's default browser.
#'
#' @details
#' The Shiny app is bundled inside the package under the `shinyapp/`
#' directory. If the directory cannot be found, the function will stop
#' with an informative error.  
#' Missing dependencies will be installed automatically from CRAN.
#'
#' @return
#' This function is called for launching a Shiny
#' application and does not return a value.
#'
#' @export
XZDB.Run <- function() {
  # Find app directory inside installed package
  appDir <- system.file("shinyapp", package = "XZDBfunction")
  if (appDir == "") {
    stop("Could not find Shiny app directory in the package.", call. = FALSE)
  }
  
  # Packages
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  needed <- c("shiny", "bslib", "DT", "readxl", "dplyr", "purrr",
              "rlang", "stringr", "tibble", "ggpubr", "patchwork",
              "httpuv", "tidyverse")
  installed <- rownames(installed.packages())
  missing <- setdiff(needed, installed)
  if (length(missing)) {
    message("Installing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, Ncpus = max(1, parallel::detectCores() - 1))
  }
  
  # Random port
  port <- tryCatch(httpuv::randomPort(), error = function(e) 8888)
  url <- sprintf("http://127.0.0.1:%d/", port)
  message(sprintf("Starting app on %s", url))
  
  # Run app
  shiny::runApp(
    appDir = appDir,
    host = "127.0.0.1",
    port = port,
    launch.browser = TRUE
  )
}
