test_that("filename sanitizer replaces unsafe path characters", {
  expect_equal(XZDBfunction:::sanitize_filename("gene:a/b*c"), "gene_a_b_c")
  expect_equal(XZDBfunction:::sanitize_filename(""), "PrintResults")
})

test_that("token cleansing treats blank-like values as missing", {
  cleaned <- XZDBfunction:::cleanse_tokens(c("", "NA", "n/a", "GeneA", " missing "))
  expect_equal(is.na(cleaned), c(TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(cleaned[4], "GeneA")
})

test_that("gene-pair resolution keeps rows with usable identifiers", {
  resolved <- XZDBfunction:::resolve_gene_pairs(
    id_vec = c("ENSG000001", NA, NA),
    sym_vec = c(NA, "CDK13", NA)
  )

  expect_equal(resolved$keep, c(TRUE, TRUE, FALSE))
  expect_equal(resolved$GeneID, c("ENSG000001", "CDK13"))
  expect_equal(resolved$GeneSymbol, c("ENSG000001", "CDK13"))
})

test_that("clean file helper creates and empties directories", {
  tmp <- file.path(tempdir(), paste0("xzdb-clean-", Sys.getpid()))
  dir.create(tmp, showWarnings = FALSE)
  file.create(file.path(tmp, "old.csv"))

  expect_true(XZDBfunction::xzdb.clean.file(tmp))
  expect_true(dir.exists(tmp))
  expect_length(list.files(tmp, all.files = TRUE, no.. = TRUE), 0)
})
