# start.R — one-click bootstrapper for XZ DB Browser
# Project layout:
#   ProjectRoot/
#     App_Info/           (app.R, XZ_DB_functions.r, datasets/, IndexedData/, searching_*.csv)
#     Output/             (exports)
#     Start.R             (this file)


#' Run Shiny UI for the Genomic DB Browser
#'
#' @description
#' Launches the Shiny-based **Genomic DB Browser** bundled inside the
#' `XZDBfunction` package.  
#'
#' By default, the UI runs directly from the installed package directory
#' (`system.file("shinyapp")`).  
#'
#' Developers or advanced users may instead choose to run the app from the
#' **current working directory** by setting `use_current = TRUE`.  
#' In this mode, all Shiny UI files (e.g., `app.R`, `XZ_DB_functions.r`,
#' CSS/JS assets, etc.) are automatically copied from the packaged
#' `shinyapp/` directory into the user's current folder before launching.
#'
#' @details
#' This function also:
#' - Ensures required Shiny packages are installed,
#' - Selects a random available port,
#' - Starts the app in the default web browser.
#'
#' The `use_current = TRUE` mode is recommended for:
#' - Development,
#' - Debugging,
#' - Editing UI/server code,
#' - Running an external Shiny deployment.
#'
#' No files inside the installed package are modified.
#'
#' @param use_current
#' Logical.  
#' - `FALSE` (default): run the packaged Shiny app (safe, static, stable).  
#' - `TRUE`: copy the packaged Shiny app into the current directory and
#' run the app from there (development mode).
#'
#' @return
#' Invisibly returns the app directory being used.  
#' (The function is normally called for its side-effects.)
#'
#' @examples
#' \dontrun{
#' # Standard mode — run packaged app
#' XZDB.Run()
#'
#' # Development mode — copy app files to working directory and run locally
#' XZDB.Run(use_current = TRUE)
#' }
#'
#' @export
XZDB.Run <- function(use_current = FALSE) {
  
  # ---------- Locate packaged shinyapp ----------
  pkg_app <- system.file("shinyapp", package = "XZDBfunction")
  if (!nzchar(pkg_app)) {
    stop("Could not find 'shinyapp' directory inside the package.", call. = FALSE)
  }
  
  # ---------- Determine where to run the UI ----------
  if (isTRUE(use_current)) {
    # Development mode: copy to current working directory
    run_dir <- file.path(getwd(), "shinyapp_local")
    
    # clean old folder
    if (dir.exists(run_dir))
      unlink(run_dir, recursive = TRUE, force = TRUE)
    
    # copy all package shinyapp files
    dir.create(run_dir, showWarnings = FALSE, recursive = TRUE)
    file.copy(pkg_app, getwd(), recursive = TRUE, overwrite = TRUE)
    run_dir <- file.path(getwd(), "shinyapp")
    
    message("[XZDB.Run] Running app from current directory: ", normalizePath(run_dir))
  } else {
    # Standard mode: run inside package
    run_dir <- pkg_app
    message("[XZDB.Run] Running packaged app from: ", run_dir)
  }
  
  # ---------- Ensure dependencies ----------
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  needed <- c("shiny", "bslib", "DT", "readxl", "dplyr", "purrr",
              "rlang", "stringr", "tibble", "ggpubr", "patchwork",
              "httpuv", "tidyverse")
  
  missing <- setdiff(needed, rownames(installed.packages()))
  if (length(missing)) {
    message("Installing required packages: ", paste(missing, collapse = ", "))
    install.packages(missing, Ncpus = max(1, parallel::detectCores() - 1))
  }
  
  # ---------- Select port ----------
  port <- tryCatch(httpuv::randomPort(), error = function(e) 8888)
  url  <- sprintf("http://127.0.0.1:%d/", port)
  message("Launching app at: ", url)
  
  # ---------- Launch Shiny ----------
  shiny::runApp(
    appDir = run_dir,
    host = "127.0.0.1",
    port = port,
    launch.browser = TRUE
  )
  
  invisible(run_dir)
}
