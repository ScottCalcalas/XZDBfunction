
# ---------- Lightweight gene mappers with caching ----------
.hcache <- new.env(parent = emptyenv())
.pkg_ok <- function(pkg) isTRUE(requireNamespace(pkg, quietly = TRUE))

# Normalizers & detectors
norm_symbol <- function(x) {
  x <- scalarize_chr(x, na = NA_character_)
  if (is.na(x)) return(NA_character_)
  toupper(gsub("\\s+", "", x))
}
norm_ensembl <- function(x) {
  x <- scalarize_chr(x, na = NA_character_)
  if (is.na(x)) return(NA_character_)
  sub("\\.\\d+$", "", toupper(x))  # strip version
}
is_ensembl <- function(x) grepl("^ENSG\\d{6,}", toupper(x %||% ""))
is_entrez  <- function(x) grepl("^\\d+$", x %||% "")

map_symbol_to_id <- function(sym) {sym}

map_id_to_symbol <- function(id) {id}

# Vectorized fill with rules:
# - If both missing -> drop
# - If one missing -> try to infer; if still missing -> synthesize "NA_<other>"
# - Ensure no blanks in outputs
resolve_gene_pairs <- function(id_vec, sym_vec) {
  n <- length(id_vec)
  id  <- cleanse_tokens(id_vec)
  sym <- cleanse_tokens(sym_vec)
  
  # attempt back-fill
  for (i in seq_len(n)) {
    if (is.na(id[i]) && !is.na(sym[i])) {
      guess <- map_symbol_to_id(sym[i])
      if (!is.na(guess) && nzchar(guess)) id[i] <- guess
    }
    if (is.na(sym[i]) && !is.na(id[i])) {
      guess <- map_id_to_symbol(id[i])
      if (!is.na(guess) && nzchar(guess)) sym[i] <- guess
    }
  }
  # synthesize if still one side missing
  for (i in seq_len(n)) {
    if (is.na(id[i]) && !is.na(sym[i]))  id[i]  <- paste0("NA_", norm_symbol(sym[i]))
    if (!is.na(id[i]) && is.na(sym[i])) sym[i] <- paste0("NA_", norm_ensembl(id[i]))
  }
  
  keep <- !(is.na(id) & is.na(sym))
  list(GeneID = id[keep], GeneSymbol = sym[keep], keep = keep)
}

