# Install BiocManager for BioConductor Libraries
## Note: R >= 4.4 needed for BiocManager v. 3.19, 3.20 ; R >= 4.5 needed for later versions

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
  BiocManager::install(version = "3.19")

# Install BiocGenerics and S4Vectors

if (!require("BiocGenerics")) {BiocManager::install("BiocGenerics"); require("BiocGenerics")}
if (!require("S4Vectors")) {BiocManager::install("S4Vectors"); require("S4Vectors")}

