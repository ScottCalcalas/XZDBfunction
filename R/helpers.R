#' Module helpers
#' @keywords internal

# ---- helpers ----
`%||%` <- function(a, b) if (!is.null(a)) a else b

sanitize_filename <- function(x) {
  x <- enc2utf8(trimws(x %||% ""))
  if (!nzchar(x)) x <- "PrintResults"
  gsub("[<>:\"/\\\\|?*]", "_", x)
}

timestamp_str <- function() format(Sys.time(), "%Y%m%d-%H%M%S")

# unique while preserving first-seen order
unique_preserve <- function(x) {
  seen <- new.env(parent = emptyenv()); out <- vector("list", length(x)); k <- 0L
  for (v in x) { key <- as.character(v); if (!exists(key, envir = seen, inherits = FALSE)) {
    assign(key, TRUE, envir = seen); k <- k + 1L; out[[k]] <- v } }
  unlist(out[seq_len(k)], use.names = FALSE)
}

# normalize all character cols: trim + convert "" to NA
trim_df <- function(df) {
  if (!ncol(df)) return(df)
  for (nm in names(df)) {
    if (is.character(df[[nm]]) || is.factor(df[[nm]])) {
      v <- as.character(df[[nm]])
      v <- trimws(v)
      v[!nzchar(v)] <- NA_character_
      df[[nm]] <- v
    }
  }
  df
}

# scalar guards (no length-0 scalars)
scalarize_chr <- function(x, na = NA_character_) {
  if (is.null(x) || length(x) == 0) return(na)
  as.character(x)[1]
}
scalarize_num <- function(x, na = NA_real_) {
  if (is.null(x) || length(x) == 0) return(na)
  suppressWarnings(as.numeric(x)[1])
}

infer_type <- function(fname) {
  if (grepl("\\.csv$",  fname, TRUE)) return("CSV")
  if (grepl("\\.txt$",  fname, TRUE)) return("TXT")
  if (grepl("\\.xlsx$", fname, TRUE)) return("XLSX")
  return(NA_character_)
}

# smarter match: exact (case-insens.), else base before "...n"
ci_match <- function(want, cols) {
  want <- scalarize_chr(want)
  if (!nzchar(want) || is.na(want)) return(NA_character_)
  m <- which(tolower(cols) == tolower(want))
  if (length(m) == 1) return(cols[m])
  basecols <- gsub("\\.\\.\\d+$", "", cols)
  m2 <- which(tolower(basecols) == tolower(want))
  if (length(m2) >= 1) return(cols[m2[1]])
  NA_character_
}

# ----- blank-like handling -----
.blank_like <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  tolower(x) %in% c("", "na", "n/a", "null", "nil", "none", ".", "nan", "missing")
}
cleanse_tokens <- function(x) {
  x <- as.character(x)
  x[.blank_like(x)] <- NA_character_
  x
}

