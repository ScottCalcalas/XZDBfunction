# Generate note: Run next line
# library(devtools)
# devtools::document()

# If want everything inside the package, put next line at NAMESPACE
# exportPattern(".")



# ---------- Build per-dataset index CSVs ----------

#' Build an index file for one dataset
#'
#' @description
#' Reads a single dataset file located in the local `datasets/` folder,
#' extracts gene identifier and gene symbol columns, normalizes and resolves
#' missing values, removes rows with no usable identifiers, and produces an
#' index file.  
#'
#' The resulting index CSV is written to both locations:
#'
#' 1. Local: `./IndexedData/`  
#' 2. Package: `system.file("shinyapp", package = "XZDBfunction")/IndexedData/`  
#'
#' If writing to the installed package directory is not permitted,
#' the function writes locally without error and emits a warning.
#'
#' @param file.Name
#' Character. Dataset filename (must exist under `datasets/`).
#'
#' @param Sheet
#' Integer or character. Sheet number or sheet name when reading XLSX files.
#' Default: `1`.
#'
#' @param xlsx.index.location
#' Character. Path to the Dataset Information Excel file
#' (typically `"Datasets infomation.xlsx"`).  
#' Must exist in the **current working directory**, not inside the package.
#'
#' @param write_to_shinyapp
#' Logical. Whether to write the index output into the package `shinyapp/`
#' directory in addition to the local `IndexedData/` directory.  
#' Default: `TRUE`.
#'
#' @return
#' Invisibly returns the `tibble` containing the processed index table.  
#' Primarily called for its side-effects (CSV file creation).
#'
#' @details
#' The function uses the metadata in the index Excel file to:
#' - Determine the expected file type (CSV/TXT/XLSX)
#' - Locate gene identifier and gene symbol columns  
#' - Normalize, trim, and resolve missing identifier pairs  
#' - Construct stable row keys of the form `"DatasetName~Sheet.index"`  
#' - Drop rows where both ID and Symbol cannot be recovered  
#'
#' @examples
#' \dontrun{
#' xiaopei.input("DatasetA.xlsx", Sheet = 1)
#' }
#'
#' @export
xiaopei.input <- function(file.Name,
                          Sheet = 1,
                          xlsx.index.location = "Datasets infomation.xlsx",
                          write_to_shinyapp = TRUE) {
  
  # ---- Read index file from current working directory ----
  indexfile <- readxl::read_xlsx(xlsx.index.location)
  indexfile <- trim_df(indexfile)
  
  # ---- Match index row ----
  i <- which(as.character(indexfile$DatasetName) == as.character(file.Name) &
               as.character(indexfile$Sheet) == as.character(Sheet))
  
  if (!length(i)) {
    warning(sprintf("[xiaopei.input] No index for %s (Sheet=%s)", file.Name, Sheet))
    out <- tibble::tibble(Row = character(0), GeneID = character(0), geneSymbol = character(0))
    .write_index_dual(out, file.Name, Sheet, write_to_shinyapp)
    return(invisible(NULL))
  }
  i <- i[1]
  
  # ---- Determine File Type ----
  File_Type <- toupper(scalarize_chr(indexfile$File_Type[i]))
  if (!nzchar(File_Type) || is.na(File_Type))
    File_Type <- infer_type(file.Name)
  
  # ---- Read dataset ----
  path <- file.path("datasets", file.Name)
  nowdata <- tryCatch({
    if (File_Type == "CSV") utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    else if (File_Type == "TXT") utils::read.table(path, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    else if (File_Type == "XLSX") as.data.frame(readxl::read_xlsx(path, sheet = Sheet), check.names = FALSE)
    else stop(sprintf("Unknown File_Type: %s", File_Type))
  }, error = function(e) {
    warning(sprintf("[xiaopei.input] Cannot read %s (Sheet %s): %s", file.Name, Sheet, e$message))
    NULL
  })
  
  # ---- If fail or empty ----
  if (is.null(nowdata) || !NROW(nowdata)) {
    out <- tibble::tibble(Row = character(0), GeneID = character(0), geneSymbol = character(0))
    .write_index_dual(out, file.Name, Sheet, write_to_shinyapp)
    cat("run done (empty): ", file.Name, "\n")
    return(invisible(NULL))
  }
  
  # ---- Mapping gene ID + symbol ----
  id_col  <- scalarize_chr(indexfile$VarN_GeneID[i])
  sym_col <- scalarize_chr(indexfile$VarN_GeneName[i])
  id_in   <- ci_match(id_col, names(nowdata))
  sym_in  <- ci_match(sym_col, names(nowdata))
  
  n <- nrow(nowdata)
  gene_id_vec  <- if (!is.na(id_in))  as.character(nowdata[[id_in]])  else rep(NA_character_, n)
  gene_sym_vec <- if (!is.na(sym_in)) as.character(nowdata[[sym_in]]) else rep(NA_character_, n)
  
  gene_id_vec  <- cleanse_tokens(gene_id_vec)
  gene_sym_vec <- cleanse_tokens(gene_sym_vec)
  filled <- resolve_gene_pairs(gene_id_vec, gene_sym_vec)
  keep   <- filled$keep
  
  if (!any(keep)) {
    out <- tibble::tibble(Row = character(0), GeneID = character(0), geneSymbol = character(0))
    .write_index_dual(out, file.Name, Sheet, write_to_shinyapp)
    cat("run done (all dropped): ", file.Name, "\n")
    return(invisible(NULL))
  }
  
  # ---- Build Dataindex ----
  Row_all <- paste0(indexfile$DatasetName[i], "~", Sheet, ".", seq_len(n))
  Row     <- Row_all[keep]
  
  Dataindex <- tibble::tibble(
    Row        = Row,
    GeneID     = filled$GeneID,
    geneSymbol = filled$GeneSymbol
  )
  
  .write_index_dual(Dataindex, file.Name, Sheet, write_to_shinyapp)
  
  cat("run done :", file.Name, "\n")
  return(invisible(Dataindex))
}







#' Internal helper: write index CSV to local and package locations
#'
#' @description
#' Writes an index `data.frame`/`tibble` produced by `xiaopei.input()` to:
#'
#' - The local directory `./IndexedData/`  
#' - The package directory `system.file("shinyapp", package = "XZDBfunction")/IndexedData/`
#'
#' Fails safely when package directory write access is unavailable.
#'
#' @param df A data.frame or tibble containing index data.
#' @param file.Name Character. Dataset filename (base name).
#' @param Sheet Sheet number/name associated with the index.
#' @param write_to_shinyapp Logical. Write to package location in addition to local.
#'
#' @return Invisibly `NULL`.
#'
#' @keywords internal
.write_index_dual <- function(df, file.Name, Sheet, write_to_shinyapp = TRUE) {
  
  # ensure local
  if (!dir.exists("IndexedData"))
    dir.create("IndexedData", recursive = TRUE)
  
  out_local <- file.path("IndexedData", sprintf("%s~%s.csv", file.Name, Sheet))
  utils::write.csv(df, out_local, row.names = FALSE)
  
  # shinyapp location
  if (isTRUE(write_to_shinyapp)) {
    appDir <- system.file("shinyapp", package = "XZDBfunction")
    if (nzchar(appDir)) {
      
      idxDir <- file.path(appDir, "IndexedData")
      if (!dir.exists(idxDir)) {
        # test write permission
        ok <- dir.create(idxDir, recursive = TRUE, showWarnings = FALSE)
        if (!ok) {
          warning("[xiaopei.input] no permission to write into shinyapp/IndexedData")
          return(invisible(NULL))
        }
      }
      
      out_pkg <- file.path(idxDir, sprintf("%s~%s.csv", file.Name, Sheet))
      try(utils::write.csv(df, out_pkg, row.names = FALSE), silent = TRUE)
    }
  }
  invisible(NULL)
}



#' Clean all files and folders inside a directory
#'
#' @description Create the directory if it doesn't exist. And make sure it being cleaned
#' 
#' 
#' @param idx_dir Character. Directory to clean.
#'
#' @return Invisibly TRUE.
#'
#' @examples
#' xiaopei.clean.file("IndexedData")
#' xiaopei.clean.file("Output")
#'
#' @export
xiaopei.clean.file <- function(idx_dir) {
  
  # Ensure directory exists
  if (!dir.exists(idx_dir)) {
    message(sprintf("[clean.file] Directory '%s' does not exist — creating it.", idx_dir))
    dir.create(idx_dir, showWarnings = FALSE)
    return(invisible(TRUE))
  }
  
  # Get all items inside (files + folders)
  items <- list.files(
    idx_dir,
    full.names  = TRUE,
    all.files   = TRUE,
    no..        = TRUE
  )
  
  # Nothing to delete
  if (!length(items)) {
    message(sprintf("[clean.file] '%s' is already empty.", idx_dir))
    return(invisible(TRUE))
  }
  
  # Delete everything inside the directory
  unlink(items, recursive = TRUE, force = TRUE)
  
  message(sprintf(
    "[clean.file] Cleaned '%s': removed %d item(s).",
    idx_dir, length(items)
  ))
  
  invisible(TRUE)
}




#' Synchronize all datasets and index files into the package shinyapp directory
#'
#' @description
#' Copies the following **from the current working directory** into the installed
#' package’s `shinyapp/` directory:
#'
#' - `datasets/`  
#' - `IndexedData/`  
#' - the dataset-information Excel file (`xlsx.index.location`)
#'
#' Before copying, the function clears any existing `datasets/` or `IndexedData/`
#' folders inside the package directory to ensure a clean update.
#'
#' The function tests write permission on the package directory.  
#' If the directory is locked (common on system-wide installations), a user-friendly
#' warning is shown instructing the user to perform a manual copy.
#'
#' @param xlsx.index.location
#' Character. Path to the Dataset Information Excel file in the current working
#' directory. Default: `"Datasets infomation.xlsx"`.
#'
#' @return
#' Logical (invisibly):  
#' - `TRUE` if sync succeeded  
#' - `FALSE` if package directory is read-only or not found  
#'
#' @examples
#' \dontrun{
#' xiaopei.sync.to.shinyapp()
#' }
#'
#' @export
xiaopei.sync.to.shinyapp <- function(xlsx.index.location = "Datasets infomation.xlsx",DatasetfolderName="datasets") {
  
  appDir <- system.file("shinyapp", package = "XZDBfunction")
  
  if (!nzchar(appDir)) {
    warning("[sync] Cannot find shinyapp directory in package.")
    return(invisible(FALSE))
  }
  
  # ---- test write permission ----
  testfile <- file.path(appDir, ".__test_write__")
  can_write <- tryCatch({
    writeLines("test", testfile)
    file.remove(testfile)
    TRUE
  }, error = function(e) FALSE)
  
  if (!can_write) {
    warning(
      "[sync] shinyapp folder is READ-ONLY.\n",
      "Please manually copy:\n",
      "- datasets/\n",
      "- IndexedData/\n",
      "- ", xlsx.index.location, "\n",
      "into: ", appDir
    )
    return(invisible(FALSE))
  }
  
  message("[sync] Updating ", appDir)
  
  # ---- Remove old copies ----
  unlink(file.path(appDir, "datasets"),     recursive = TRUE, force = TRUE)
  #unlink(file.path(appDir, "IndexedData"),  recursive = TRUE, force = TRUE)
  
  # ---- Copy new datasets/ ----
  if (dir.exists(DatasetfolderName)) {
    file.copy(DatasetfolderName, appDir, recursive = TRUE, overwrite = TRUE)
  }
  
  # ---- Copy new IndexedData/ ----
  if (dir.exists("IndexedData")) {
    file.copy("IndexedData", appDir, recursive = TRUE, overwrite = TRUE)
  }
  
  # ---- Copy index Excel ----
  if (file.exists(xlsx.index.location)) {
    file.copy(xlsx.index.location,
              file.path(appDir, basename(xlsx.index.location)),
              overwrite = TRUE)
  }
  
  message("[sync] Done.")
  
  invisible(TRUE)
}


#' Build index files for all datasets listed in the index Excel
#'
#' @description
#' Reads the dataset-information Excel file (e.g., `"Datasets infomation.xlsx"`),
#' loops over all listed dataset names and sheet specifications, and calls
#' \code{\link{xiaopei.input}} for each entry.  
#'
#' After successfully building all local index files, the function optionally
#' synchronizes the entire `datasets/`, `IndexedData/`, and Excel index file
#' into the installed package's `shinyapp/` directory using
#' \code{\link{xiaopei.sync.to.shinyapp}}.
#'
#' @param xlsx.index.location
#' Character. File path to the dataset-information Excel file in the current
#' working directory. Default: `"Datasets infomation.xlsx"`.
#'
#' @param sync
#' Logical. If `TRUE` (default), calls \code{xiaopei.sync.to.shinyapp()} after all
#' indexes are built. Set to `FALSE` to disable automatic syncing.
#'
#' @return
#' Invisibly `NULL`. Creates/updates local `IndexedData/` directory and
#' (optionally) updates the package `shinyapp/` folder.
#'
#' @examples
#' \dontrun{
#' # Process all datasets and sync to shinyapp
#' xiaopei.input.all()
#'
#' # Process only, do NOT sync
#' xiaopei.input.all(sync = FALSE)
#' }
#'
#' @export
xiaopei.input.all <- function(xlsx.index.location = "Datasets infomation.xlsx") {
  xiaopei.clean.file(idx_dir="IndexedData")
  
  
  indexfile <- readxl::read_xlsx(xlsx.index.location)
  indexfile <- trim_df(indexfile)
  if (!NROW(indexfile)) {
    warning("[input.all] Index has 0 rows.")
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(indexfile))) {
    fn <- scalarize_chr(indexfile$DatasetName[i])
    sh <- indexfile$Sheet[i]
    tryCatch(
      xiaopei.input(fn, Sheet = sh, xlsx.index.location = xlsx.index.location),
      error = function(e) warning(sprintf("[input.all] Failed on row %d: %s (Sheet=%s): %s", i, fn, as.character(sh), conditionMessage(e)))
    )
  }
  xiaopei.sync.to.shinyapp()
}





##********************************************************##
# PREPARE search tables (robust to missing columns)

# ---- PREPARE: GeneID ----------------------------------------------
xiaopei.prepare.GeneID <- function(index.location = "IndexedData",
                                   file_regex = "~[0-9]+\\.csv$") {
  message("[prepare.GeneID] selecting {GeneID, Row}; coercing Row -> character")
  files <- list.files(index.location, pattern = file_regex, full.names = TRUE)
  if (!length(files)) {
    utils::write.csv(data.frame(GeneID = character()), "searching_GeneID.csv", row.names = FALSE)
    message("No index CSVs found; wrote empty searching_GeneID.csv")
    return(invisible(NULL))
  }
  
  read_one <- function(path) {
    nm <- tools::file_path_sans_ext(basename(path))
    df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    
    if (!"Row" %in% names(df))   df$Row <- paste0(basename(path), "~1.", seq_len(nrow(df)))
    if (!"GeneID" %in% names(df)) df$GeneID <- NA_character_
    
    df <- df %>% 
      mutate(GeneID = cleanse_tokens(GeneID)) %>% 
      filter(!is.na(GeneID)) %>% 
      group_by(GeneID) %>% 
      summarise(!!nm := paste(unique(Row), collapse = ";"), .groups = "drop")
    
    df
  }
  
  
  pieces <- purrr::map(files, read_one)
  combined <- purrr::reduce(pieces, dplyr::full_join, by = "GeneID")
  utils::write.csv(combined, file = "searching_GeneID.csv", row.names = FALSE)
  message(sprintf("Done: %d files → %d rows, %d columns", length(files), nrow(combined), ncol(combined)))
  invisible(NULL)
}

# ---- PREPARE: geneSymbol ------------------------------------------
xiaopei.prepare.geneSymbol <- function(index.location = "IndexedData",
                                       file_regex = "~[0-9]+\\.csv$") {
  message("[prepare.geneSymbol] selecting {geneSymbol, Row}; coercing Row -> character")
  files <- list.files(index.location, pattern = file_regex, full.names = TRUE)
  if (!length(files)) {
    utils::write.csv(data.frame(geneSymbol = character()), "searching_geneSymbol.csv", row.names = FALSE)
    message("No index CSVs found; wrote empty searching_geneSymbol.csv")
    return(invisible(NULL))
  }
  
  read_one <- function(path) {
    nm <- tools::file_path_sans_ext(basename(path))
    df <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    if (!"Row" %in% names(df)) df$Row <- paste0(basename(path), "~1.", seq_len(nrow(df)))
    if (!"geneSymbol" %in% names(df) && "GeneSymbol" %in% names(df)) {
      names(df)[names(df) == "GeneSymbol"] <- "geneSymbol"
    }
    if (!"geneSymbol" %in% names(df)) df$geneSymbol <- NA_character_
    df$geneSymbol <- cleanse_tokens(df$geneSymbol)
    df <- df %>% 
      mutate(geneSymbol = cleanse_tokens(geneSymbol)) %>% 
      filter(!is.na(geneSymbol)) %>% 
      group_by(geneSymbol) %>% 
      summarise(!!nm := paste(unique(Row), collapse = ";"), .groups = "drop")
    }
  
  pieces <- purrr::map(files, read_one)
  combined <- purrr::reduce(pieces, dplyr::full_join, by = "geneSymbol")
  utils::write.csv(combined, file = "searching_geneSymbol.csv", row.names = FALSE)
  message(sprintf("Done: %d files → %d rows, %d columns", length(files), nrow(combined), ncol(combined)))
  invisible(NULL)
}

# Convenience wrapper
xiaopei.prepare.all <- function(index.location = "IndexedData") {
  xiaopei.prepare.GeneID(index.location)
  xiaopei.prepare.geneSymbol(index.location)
}

##********************************************************##
# search (console)
xiaopei.search.geneSymbol <- function(geneSymbol) {
  index <- utils::read.csv("searching_geneSymbol.csv", check.names = FALSE)
  result <- index[which(index$geneSymbol == geneSymbol), , drop = FALSE]
  cat("Find: "); print(result)
}

xiaopei.search.GeneID <- function(GeneID) {
  index <- utils::read.csv("searching_GeneID.csv", check.names = FALSE)
  result <- index[which(index$GeneID == GeneID), , drop = FALSE]
  cat("Find: "); print(result)
}

##********************************************************##
# print core (with metadata block first)

print_and_save_selected_rows <- function(
    result,
    output_file = "Output/TEMP searching results.csv",
    xlsx.index.location = "Datasets infomation.xlsx",
    warn_on_malformed = FALSE,
    return_rows = FALSE
) {
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(output_file)) file.remove(output_file)
  
  indexfile <- readxl::read_excel(xlsx.index.location)
  indexfile <- trim_df(indexfile)
  
  vals_raw <- as.vector(unlist(result, use.names = FALSE))
  vals     <- unlist(strsplit(vals_raw, ";", fixed = TRUE))
  
  tokens <- grep("^[^~]+~.+\\.[0-9]+$", vals, value = TRUE)
  
  if (!length(tokens)) return(if (return_rows) tibble() else invisible(NULL))
  
  parts    <- strsplit(tokens, "~", fixed = TRUE)
  files    <- trimws(vapply(parts, `[[`, "", 1L))
  sheetrow <- trimws(vapply(parts, `[[`, "", 2L))
  
  lastdot <- regexpr("\\.(?=[^\\.]*$)", sheetrow, perl = TRUE)
  sheets <- ifelse(lastdot > 0, substr(sheetrow, 1L, lastdot - 1L), NA_character_)
  rows_s <- ifelse(lastdot > 0, substr(sheetrow, lastdot + 1L, nchar(sheetrow)), NA_character_)
  rows   <- suppressWarnings(as.integer(trimws(rows_s)))
  
  keep  <- nzchar(files) & nzchar(sheets) & is.finite(rows) & rows >= 1L
  files <- files[keep]; sheets <- sheets[keep]; rows <- rows[keep]
  if (!length(files)) return(if (return_rows) tibble() else invisible(NULL))
  
  SEP <- "\u001F"
  keys <- paste0(files, SEP, sheets)
  keys_order <- unique(keys)
  rows_by_key <- lapply(keys_order, function(k) unique(rows[keys == k]))
  names(rows_by_key) <- keys_order
  
  # Exclude mapping columns and also hide File_Type & Sheet from metadata
  drop_meta <- c("VarN_GeneID", "VarN_GeneName", "File_Type", "Sheet")
  
  collected <- list()
  
  for (key in keys_order) {
    split_key <- strsplit(key, SEP, fixed = TRUE)[[1]]
    file_name  <- split_key[1]
    sheet_name <- split_key[2]
    wanted     <- rows_by_key[[key]]
    path <- file.path("datasets", file_name)
    
    # index row for metadata + mapping
    i <- which(as.character(indexfile$DatasetName) == as.character(file_name) &
                 as.character(indexfile$Sheet)       == as.character(sheet_name))
    File_Type <- if (length(i)) toupper(scalarize_chr(indexfile$File_Type[i[1]])) else NA_character_
    if (!nzchar(File_Type) || is.na(File_Type)) File_Type <- infer_type(file_name)
    if (!nzchar(File_Type) || is.na(File_Type)) next
    
    # read the data table
    df <- tryCatch({
      if (File_Type == "CSV") {
        utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
      } else if (File_Type == "TXT") {
        utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
      } else if (File_Type == "XLSX") {
        sheet_arg <- suppressWarnings(as.integer(sheet_name))
        if (!is.finite(sheet_arg)) sheet_arg <- sheet_name
        as.data.frame(readxl::read_excel(path, sheet = sheet_arg), check.names = FALSE, stringsAsFactors = FALSE)
      } else NULL
    }, error = function(e) NULL)
    if (is.null(df)) next
    
    valid <- wanted[wanted <= nrow(df)]
    if (!length(valid)) next
    
    # console + file output
    cat("=== File:", file_name, " | Sheet:", sheet_name, " | Type:", File_Type, "===\n")
    cat("Rows:", paste(valid, collapse = ", "), "\n")
    for (r in valid) { cat(sprintf("[Row %d]\n", r)); print(df[r, , drop = FALSE]) }
    cat("\n")
    cat(sprintf("# File: %s\n", file_name), file = output_file, append = TRUE)
    cat(sprintf("# Sheet: %s\n", sheet_name), file = output_file, append = TRUE)
    suppressWarnings(
      utils::write.table(head(df, 0), file = output_file, sep = ",",
                         col.names = TRUE, row.names = FALSE, append = TRUE, qmethod = "double")
    )
    utils::write.table(df[valid, , drop = FALSE], file = output_file, sep = ",",
                       col.names = FALSE, row.names = FALSE, append = TRUE, qmethod = "double")
    cat("\n", file = output_file, append = TRUE)
    
    if (return_rows) {
      #1) Metadata block (drop mapping vars + File_Type + Sheet) -------
      meta_row <- if (length(i)) indexfile[i[1], , drop = FALSE] else tibble()
      if (nrow(meta_row) == 0) meta_row <- tibble(DatasetName = file_name)
      meta_cols <- setdiff(names(meta_row), drop_meta)
      meta_rep  <- meta_row[rep(1, length(valid)), meta_cols, drop = FALSE]
      meta_rep  <- dplyr::mutate(meta_rep, dplyr::across(everything(), ~as.character(.x %||% NA_character_)))
      
      #2) Data block, with standardized gene columns -------------------
      block_data <- as_tibble(df[valid, , drop = FALSE])
      
      id_col  <- if (length(i)) scalarize_chr(indexfile$VarN_GeneID[i[1]])  else NA_character_
      sym_col <- if (length(i)) scalarize_chr(indexfile$VarN_GeneName[i[1]]) else NA_character_
      
      present_id  <- ci_match(id_col,  names(block_data))
      present_sym <- ci_match(sym_col, names(block_data))
      
      # === Rename for USER-FACING table ===
      if (!is.na(present_id) && !"ID" %in% names(block_data)) {
        names(block_data)[names(block_data) == present_id] <- "ID"
      }
      if ("GeneID" %in% names(block_data) && !"ID" %in% names(block_data)) {
        names(block_data)[names(block_data) == "GeneID" ] <- "ID"
      }
      if (!is.na(present_sym) && !"GeneSymbol" %in% names(block_data)) {
        names(block_data)[names(block_data) == present_sym] <- "Symbol"
      }
      
      block_data <- dplyr::mutate(block_data, dplyr::across(everything(), as.character))
      
      #3) Assemble: [metadata] + [data] -------
      block <- dplyr::bind_cols(meta_rep, block_data)
      
      
      block_data <- dplyr::relocate(
        block_data,
        dplyr::any_of(c("Symbol", "ID", "GeneID")),
        .before = 1
      )
      
      
      collected[[length(collected) + 1L]] <- block
    }
  }
  
  message(sprintf("Done. Saved to '%s'.", output_file))
  if (return_rows) {
    if (length(collected)) return(dplyr::bind_rows(collected))
    return(tibble())
  }
  invisible(NULL)
}












##********************************************************##
# print helpers (pass index path through)

xiaopei.print.geneSymbol <- function(geneSymbol, output_name = NULL,
                                     fulldata.location = "datasets",
                                     add_timestamp = TRUE,
                                     xlsx.index.location = "Datasets infomation.xlsx") {
  index <- utils::read.csv("searching_geneSymbol.csv", check.names = FALSE)
  terms <- unique_preserve(as.character(geneSymbol))
  base  <- sanitize_filename(output_name %||% "")
  combined <- tibble()
  
  for (t in terms) {
    result <- index[which(toupper(index$geneSymbol) == toupper(t)), , drop = FALSE]
    cat("Find: "); print(result); cat("\n\n")
    if (!nrow(result)) next
    
    suffix <- sanitize_filename(t)
    stem   <- if (nzchar(base)) paste0(base, "_", suffix) else suffix
    mode_tag <- "exact"
    fname <- if (add_timestamp) paste0(stem, "__", mode_tag, "_", timestamp_str()) else paste0(stem, "__", mode_tag)
    
    ofile  <- file.path("Output", paste0(fname, ".csv"))
    df <- print_and_save_selected_rows(result, output_file = ofile,
                                       xlsx.index.location = xlsx.index.location,
                                       return_rows = TRUE)
    if (nrow(df)) combined <- dplyr::bind_rows(combined, dplyr::mutate(df, `__Query` = t))
  }
  return(combined)
}

xiaopei.print.GeneID <- function(GeneID, output_name = NULL,
                                 fulldata.location = "datasets",
                                 add_timestamp = TRUE,
                                 xlsx.index.location = "Datasets infomation.xlsx") {
  index <- utils::read.csv("searching_GeneID.csv", check.names = FALSE)
  terms <- unique_preserve(as.character(GeneID))
  base  <- sanitize_filename(output_name %||% "")
  combined <- tibble()
  
  for (t in terms) {
    result <- index[which(as.character(index$GeneID) == as.character(t)), , drop = FALSE]
    cat("Find: "); print(result); cat("\n\n")
    if (!nrow(result)) next
    
    suffix <- sanitize_filename(t)
    stem   <- if (nzchar(base)) paste0(base, "_", suffix) else suffix
    mode_tag <- "exact"
    fname <- if (add_timestamp) paste0(stem, "__", mode_tag, "_", timestamp_str()) else paste0(stem, "__", mode_tag)
    
    ofile  <- file.path("Output", paste0(fname, ".csv"))
    df <- print_and_save_selected_rows(result, output_file = ofile,
                                       xlsx.index.location = xlsx.index.location,
                                       return_rows = TRUE)
    if (nrow(df)) combined <- dplyr::bind_rows(combined, dplyr::mutate(df, `__Query` = t))
  }
  return(combined)
}

xiaopei.print.geneSymbol.family <- function(geneSymbol, output_name = NULL,
                                            fulldata.location = "datasets",
                                            add_timestamp = TRUE,
                                            xlsx.index.location = "Datasets infomation.xlsx") {
  index <- utils::read.csv("searching_geneSymbol.csv", check.names = FALSE)
  terms <- unique_preserve(as.character(geneSymbol))
  base  <- sanitize_filename(output_name %||% "")
  combined <- tibble()
  
  for (t in terms) {
    family_key <- gsub("[^A-Za-z]", "", t)
    if (!nzchar(family_key)) next
    result <- index[grepl(family_key, index$geneSymbol, ignore.case = TRUE), , drop = FALSE]
    
    cat("Find (family:", family_key, ")\n"); print(result); cat("\n\n")
    if (!nrow(result)) next
    
    suffix <- sanitize_filename(family_key)
    stem   <- if (nzchar(base)) paste0(base, "_", suffix) else suffix
    mode_tag <- "family"
    fname <- if (add_timestamp) paste0(stem, "__", mode_tag, "_", timestamp_str()) else paste0(stem, "__", mode_tag)
    
    ofile  <- file.path("Output", paste0(fname, ".csv"))
    df <- print_and_save_selected_rows(result, output_file = ofile,
                                       xlsx.index.location = xlsx.index.location,
                                       return_rows = TRUE)
    if (nrow(df)) {
      df <- dplyr::mutate(df, `__Query` = t, `__Family` = family_key)
      df <- dplyr::relocate(df, dplyr::starts_with("__"), .after = dplyr::last_col())
      combined <- dplyr::bind_rows(combined, df)
    }
  }
  return(combined)
}

xiaopei.print.geneSymbol.fuzzy <- function(geneSymbol, output_name = NULL,
                                           fulldata.location = "datasets",
                                           add_timestamp = TRUE,
                                           xlsx.index.location = "Datasets infomation.xlsx") {
  index <- utils::read.csv("searching_geneSymbol.csv", check.names = FALSE)
  terms <- unique_preserve(as.character(geneSymbol))
  base  <- sanitize_filename(output_name %||% "")
  combined <- tibble()
  
  for (t in terms) {
    hits_logical <- agrepl(t, index$geneSymbol, ignore.case = TRUE, max.distance = 0.1, useBytes = FALSE)
    result <- index[hits_logical, , drop = FALSE]
    
    cat("Find (fuzzy:", t, ")\n"); print(result); cat("\n\n")
    if (!nrow(result)) next
    
    suffix <- sanitize_filename(t)
    stem   <- if (nzchar(base)) paste0(base, "_", suffix) else suffix
    mode_tag <- "fuzzy-0.1"
    fname <- if (add_timestamp) paste0(stem, "__", mode_tag, "_", timestamp_str()) else paste0(stem, "__", mode_tag)
    
    ofile  <- file.path("Output", paste0(fname, ".csv"))
    df <- print_and_save_selected_rows(result, output_file = ofile,
                                       xlsx.index.location = xlsx.index.location,
                                       return_rows = TRUE)
    if (nrow(df)) {
      df <- dplyr::mutate(df, `__Query` = t, `__Fuzzy` = "0.1")
      df <- dplyr::relocate(df, dplyr::starts_with("__"), .after = dplyr::last_col())
      combined <- dplyr::bind_rows(combined, df)
    }
  }
  return(combined)
}
