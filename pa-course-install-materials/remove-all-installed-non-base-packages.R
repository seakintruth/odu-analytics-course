readInput <- function(question)
{ 
  n <- readline(prompt=paste(question, " \n"))
  return(n)
}

proceed.answer = readInput(question="This script should only be run if you are having issues with installed on a version of R < 3.6 being updated/installed on your machine. Is this your issue?")
if (proceed.answer != "yes")
{
  cat(paste("You have not answered 'yes' to the question are you having issues with installed on a version of R < 3.6 being updated/installed on your machine.",
            "As a result the script will now stop.",sep="\n"))
  stop()
}

cat(paste("We will now install every non-base or non-recommended package for an R installation from your machine.",
          " If this is not your intention press the stop sign in the next 10 seconds.",sep="\n"))

Sys.sleep(10)

# create a list of all installed packages
ip <- as.data.frame(installed.packages())
head(ip)
# if you use MRO, make sure that no packages in this library will be removed
ip <- subset(ip, !grepl("MRO", ip$LibPath))
# we don't want to remove base or recommended packages either\
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# determine the library where the packages are installed
path.lib <- unique(ip$LibPath)
# create a vector with all the names of the packages you want to remove
pkgs.to.remove <- ip[,1]
head(pkgs.to.remove)
# remove the packages
sapply(pkgs.to.remove, remove.packages, lib = path.lib)