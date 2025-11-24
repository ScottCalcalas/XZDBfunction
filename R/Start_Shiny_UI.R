# start.R — one-click bootstrapper for XZ DB Browser
# Project layout:
#   ProjectRoot/
#     App_Info/           (app.R, XZ_DB_functions.r, datasets/, IndexedData/, searching_*.csv)
#     Output/             (exports)
#     Start.R             (this file)

#' Run Shiny UI for the Genomic DB Browser
#'
#' @description
#' Launches the Shiny-based Genomic DB Browser.
#'
#' - Default: Runs the packaged UI located inside the installed
#'   XZDBfunction package.
#'
#' - Development mode (`use_current = TRUE`): Copies only two files into the
#'   **current working directory**:
#'     * app.R  
#'     * XZ_DB_functions.r  
#'   Then runs the Shiny app from the working directory.
#'
#' This allows users to modify UI/server code locally without altering the
#' package installation and without creating any additional folders.
#'
#' @param use_current Logical.  
#' - `FALSE` (default): run packaged app  
#' - `TRUE`: copy only app.R and XZ_DB_functions.r into current directory,
#'   then run app locally
#'
#' @return Invisibly returns the directory used for launching the app.
#'
#' @export
XZDB.Run <- function(use_current = FALSE) {
  
  pkg_app <- system.file("shinyapp", package = "XZDBfunction")
  if (!nzchar(pkg_app)) {
    stop("Could not find 'shinyapp' directory inside the package.", call. = FALSE)
  }
  
  # --------------------------
  # Development mode: run locally
  # --------------------------
  if (isTRUE(use_current)) {
    
    message("[XZDB.Run] use_current = TRUE → Running Shiny app from working directory.")
    
    # required files inside package shinyapp
    pkg_app_R <- file.path(pkg_app, "app.R")
    pkg_fun_R <- file.path(pkg_app, "XZ_DB_functions.r")
    
    if (!file.exists(pkg_app_R))
      stop("Missing app.R inside package shinyapp folder.")
    if (!file.exists(pkg_fun_R))
      stop("Missing XZ_DB_functions.r inside package shinyapp folder.")
    
    # copy ONLY app.R and XZ_DB_functions.r into current working directory
    file.copy(pkg_app_R, getwd(), overwrite = TRUE)
    file.copy(pkg_fun_R, getwd(), overwrite = TRUE)
    
    message("[XZDB.Run] Copied app.R and XZ_DB_functions.r into: ", normalizePath(getwd()))
    message("[XZDB.Run] Launching app from working directory...")
    
    # launch app from working directory
    return(shiny::runApp(getwd(), launch.browser = TRUE))
  }
  
  # --------------------------
  # Standard mode: run from package
  # --------------------------
  message("[XZDB.Run] Running packaged app from: ", pkg_app)
  
  shiny::runApp(
    appDir = pkg_app,
    host = "127.0.0.1",
    port = tryCatch(httpuv::randomPort(), error = function(e) 8888),
    launch.browser = TRUE
  )
}
