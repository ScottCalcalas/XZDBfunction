

#' Build environment for Bioinformation analysis
#' 
#'
#' @export
#'

xiaopei.build.bioinfo.envi<-function(){

  utils::install.packages("BiocManager")
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    stop("Package 'BiocManager' is required for Bioconductor installation.", call. = FALSE)
  }
  
  
  BiocManager::install(c("DESeq2", "EnhancedVolcano", "edgeR", "sva"))
  
  BiocManager::install("GeneOverlap")
  
  
  
  
  utils::install.packages(c("ggplot2", "magrittr", "readr", "pheatmap", "ggpubr"))

  utils::install.packages("biomaRt")
  
  
  BiocManager::install("biomaRt")
  
  
  
  
  utils::install.packages("PKI")

  utils::install.packages("devtools")

  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' is required to install pinned package versions.", call. = FALSE)
  }
  
  devtools::install_version("dbplyr", version = "2.3.4")
  
  # Install Bioconductor packages
  BiocManager::install("clusterProfiler")
  BiocManager::install("org.Hs.eg.db")
  BiocManager::install("AnnotationDbi")
  BiocManager::install("DOSE")


}


