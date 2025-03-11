# Install BiocManager for BioConductor Libraries

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.19")

# Install BiocGenerics

BiocManager::install("BiocGenerics")

BiocManager::install("S4Vectors")
