

#' Prepare Local Working Folder With Example Files
#'
#' @description
#' Copies the example files bundled inside the package's internal
#' `shinyapp/` directory into the user's current working directory.
#'  
#' This helper function is designed for **new users** who want to quickly
#' review the protocol and example dataset metadata before running the
#' Genomic DB Browser manually.
#'
#' It copies:
#' - The **`datasets/`** folder (including all example datasets),
#' - **`Datasets infomation.xlsx`** (example dataset information index),
#' - **`Use protocol.docx`** (step-by-step usage guide).
#'
#' These files help new users understand:
#' - How dataset metadata should be formatted,
#' - How to fill in `Datasets infomation.xlsx` for their own datasets,
#' - The recommended workflow described in the protocol.
#'
#' @details
#' After running this function, your working directory will contain:
#' - A local copy of the `datasets/` folder,
#' - The example dataset information Excel file,
#' - A protocol document explaining how to structure and use the database.
#'
#' You can modify **`Datasets infomation.xlsx`** to include your own datasets:
#' 1. Open the file in Excel.
#' 2. Add a new row for each dataset you want to index.
#' 3. Set `DatasetName` to the exact filename (e.g., `"MyData.xlsx"`).
#' 4. Set the sheet number/name in the `Sheet` column.
#' 5. Save the file in the same folder.
#'
#' When you run the Shiny application, the program will automatically
#' read this Excel file to index and process your datasets.
#'
#' @return
#' Invisibly returns the paths of the copied files.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Prepare example data and protocol files in the current folder
#'   xzdb.help()
#'
#'   # After this, check your working directory:
#'   #   datasets/
#'   #   Datasets infomation.xlsx
#'   #   Use protocol.docx
#' }
xzdb.help <- function() {
  # 1. Locate shinyapp folder inside the installed package
  pkg_app <- system.file("example", package = "XZDBfunction")
  if (pkg_app == "")
    stop("Cannot find shinyapp folder inside XZDBfunction package.")
  
  # 2. Paths to items we want to copy
  datasets_src <- file.path(pkg_app, "datasets")
  info_xlsx_src <- file.path(pkg_app, "Datasets infomation.xlsx")
  protocol_docx_src <- file.path(pkg_app, "Use protocol.docx")
  
  # 3. Check existence
  if (!dir.exists(datasets_src))
    stop("Missing 'datasets' folder inside package/XZDBfunction/example.")
  
  if (!file.exists(info_xlsx_src))
    stop("Missing 'Datasets infomation.xlsx' inside shinyapp.")
  
  if (!file.exists(protocol_docx_src))
    stop("Missing 'Use protocol.docx' inside shinyapp.")
  
  # 4. Copy into current working directory
  cwd <- getwd()
  
  message("Copying 'datasets' folder...")
  dir.create(file.path(cwd, "datasets"), showWarnings = FALSE)
  file.copy(from = list.files(datasets_src, full.names = TRUE),
            to   = file.path(cwd, "datasets"),
            overwrite = FALSE, recursive = TRUE)
  
  message("Copying 'Datasets infomation.xlsx'...")
  file.copy(info_xlsx_src, cwd, overwrite = FALSE)
  
  message("Copying 'Use protocol.docx'...")
  file.copy(protocol_docx_src, cwd, overwrite = FALSE)
  
  message("Check the protocol and example dataset information. See Files at: ", cwd)
}












