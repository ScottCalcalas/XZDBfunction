

#' Transformation for data inputs when using PRISM to draw figures
#'
#' @description
#' It's for quantificated western blot data. See the data input requirement below. 
#' If you like to see an example, run: Clean.measurement.for.prism.help()
#' 
#' Require input in tables is like:
#' Col names: Protein names (Accept multiple same exist)
#' Row names: GroupNames
#' 
#' Output tables is like:
#' Col names: Orgnaized GroupNames by orders
#' Row names: Protein names (Transfer to only one row for a same Protein)
#' 
#' @export
#'
Clean.measurement.for.prism<-function(df,outname="Cleaned.Prism.input"){

  library(dplyr)
  library(tidyr)
  library(readxl)
  out <- df %>%
    mutate(row_id = ave(seq_len(n()), Row, FUN = seq_along)) %>% 
    pivot_wider(
      names_from = row_id,
      values_from = -Row,
      names_sep = "_"
    ) %>%
    arrange(Row)
  
  clean_names <- function(v) {
    sapply(v, function(x) {
      if (grepl("_1$", x)) {
        sub("_1$", "", x)             # keep but remove _1
      } else if (grepl("_[0-9]+$", x)) {
        ""                          # replace _2, _3, _4... with NA
      } else {
        x                              # keep original if no suffix
      }
    })
  }
  
  
  base_row <-clean_names(colnames(out))
  
  out<-rbind(base_row,out)
  #print(out)
  write.csv(out,file = paste0(outname,".csv"),row.names = F,na="")
  print("saved file:")
  print(paste0(outname,".csv"))
}


