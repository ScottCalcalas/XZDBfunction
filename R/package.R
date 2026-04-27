#' XZDBfunction package
#'
#' @keywords internal
#' @importFrom dplyr %>% across arrange bind_cols bind_rows filter group_by last_col mutate n relocate starts_with summarise
#' @importFrom rlang :=
#' @importFrom stats anova as.formula ave binomial complete.cases confint.default glm
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect any_of everything
#' @importFrom utils head install.packages installed.packages write.csv
"_PACKAGE"

utils::globalVariables(c("GeneID", "Row", "geneSymbol", "row_id"))
