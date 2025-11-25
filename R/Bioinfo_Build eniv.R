

#' Build environment for Bioinformation analysis
#' 
#'
#' @export
#'

xiaopei.build.bioinfo.envi<-function(){

  install.packages("BiocManager")
  
  
  BiocManager::install(c("DESeq2", "EnhancedVolcano", "edgeR", "sva"))
  
  BiocManager::install("GeneOverlap")
  
  
  
  
  install.packages(c("ggplot2", "magrittr", "readr", "pheatmap", "ggpubr"))
  
  install.packages("biomaRt")
  
  
  BiocManager::install("biomaRt")
  
  
  
  
  install.packages("PKI")
  
  install.packages("devtools")
  
  library("devtools")
  
  devtools::install_version("dbplyr", version = "2.3.4")
  
  # Install Bioconductor packages
  BiocManager::install("clusterProfiler")
  BiocManager::install("org.Hs.eg.db")
  BiocManager::install("AnnotationDbi")
  BiocManager::install("DOSE")


}


