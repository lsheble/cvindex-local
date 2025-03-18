# Install BiocManager for BioConductor Libraries

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
  BiocManager::install(version = "3.19")

# Install BiocGenerics

if (!require("BiocGenerics")) {BiocManager::install("BiocGenerics"); require("BiocGenerics")}
if (!require("S4Vectors")) {BiocManager::install("S4Vectors"); require("S4Vectors")}

