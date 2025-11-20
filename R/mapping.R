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

map_symbol_to_id <- function(sym) {
  sym <- norm_symbol(sym)
  if (!nzchar(sym) || is.na(sym)) return(NA_character_)
  key <- paste0("S2I:", sym)
  if (exists(key, envir = .hcache, inherits = FALSE)) return(get(key, .hcache))
  out <- NA_character_
  if (.pkg_ok("AnnotationDbi") && .pkg_ok("org.Hs.eg.db")) {
    try({
      eg <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                  keys = sym, keytype = "SYMBOL",
                                  column = "ENSEMBL", multiVals = "first")
      out <- scalarize_chr(unname(eg[[1]]), NA_character_)
      if (nzchar(out)) out <- norm_ensembl(out)
    }, silent = TRUE)
  }
  if (is.na(out) && .pkg_ok("ensembldb") && .pkg_ok("EnsDb.Hsapiens.v86")) {
    try({
      edb <- EnsDb.Hsapiens.v86::EnsDb.Hsapiens.v86
      res <- ensembldb::select(edb, keys = sym, keytype = "SYMBOL", columns = "GENEID")
      if (NROW(res)) out <- norm_ensembl(res$GENEID[1])
    }, silent = TRUE)
  }
  assign(key, out, envir = .hcache); out
}

map_id_to_symbol <- function(id) {
  id <- scalarize_chr(id, na = NA_character_); if (is.na(id)) return(NA_character_)
  id_norm <- if (is_ensembl(id)) norm_ensembl(id) else id
  key <- paste0("I2S:", id_norm)
  if (exists(key, envir = .hcache, inherits = FALSE)) return(get(key, .hcache))
  out <- NA_character_
  if (.pkg_ok("AnnotationDbi") && .pkg_ok("org.Hs.eg.db")) {
    try({
      if (is_ensembl(id_norm)) {
        sy <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                    keys = id_norm, keytype = "ENSEMBL",
                                    column = "SYMBOL", multiVals = "first")
      } else if (is_entrez(id_norm)) {
        sy <- AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db,
                                    keys = id_norm, keytype = "ENTREZID",
                                    column = "SYMBOL", multiVals = "first")
      } else sy <- NA_character_
      out <- scalarize_chr(unname(sy[[1]]), NA_character_)
      if (nzchar(out)) out <- norm_symbol(out)
    }, silent = TRUE)
  }
  if (is.na(out) && is_ensembl(id_norm) && .pkg_ok("ensembldb") && .pkg_ok("EnsDb.Hsapiens.v86")) {
    try({
      edb <- EnsDb.Hsapiens.v86::EnsDb.Hsapiens.v86
      res <- ensembldb::select(edb, keys = id_norm, keytype = "GENEID", columns = "SYMBOL")
      if (NROW(res)) out <- norm_symbol(res$SYMBOL[1])
    }, silent = TRUE)
  }
  assign(key, out, envir = .hcache); out
}

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

