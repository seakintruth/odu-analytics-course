#code for predictive analytics course

#specify the packages of interest
packages = c("tidyverse","glmnet","smooth","matrixStats", "TSA", "TTR", "Metrics", "forecast", "shiny", "maps", "mapproj", "longCatEDA", "grid", "devtools", "caret", "yardstick")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded

problematic.packages <<- ""
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    try({
      install.packages(x, dependencies = TRUE)
      loaded.correctly = require(x,  character.only = TRUE)
    })
    if (!require(x, character.only = TRUE))
    {
      problematic.packages <<- paste(x, problematic.packages, sep=", ")
    }
  }
})

#print out those packages which require more work
cat("------------------------------------------------------------\n")
cat(paste("These packages could not be installed:",
          problematic.packages,
          "Remember this does not include tensorflow or kears.",
          "We use a separate script to install them.", sep="\n"))