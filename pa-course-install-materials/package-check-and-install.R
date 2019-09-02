#code for predictive analytics course

#specify the packages of interest
packages = c("tidyverse","glmnet","smooth","matrixStats", "TSA", "TTR", "metrics", "forecast", "shiny", "maps", "mapsproj", "longCATEDA", "grid", "devtools")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded

problematic.packages = c()
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    loaded.correctly = FALSE
    try({
      loaded.correctly = require(x,  character.only = TRUE)
    })
    if (loaded.correctly == FALSE)
    {
      problematic.packages = c(problematic.packages, x)
    }
  }
})

#print out those packages which require more work
cat("------------------------------------------------------------")
cat("The following packages could not be installed. Remeber this does not include tensorflow or kears. We use a seperate script to install them.")
cat(problematic.packages)