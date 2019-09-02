readInput <- function(question)
{ 
  n <- readline(prompt=paste(question, " \n"))
  return(n)
}


has.r.tools = FALSE
try({
  has.r.tools = devtools::find_rtools()
})
if (has.r.tools == FALSE)
{
  cat(paste("You do not have R tools installed.",
            "If you are working on a windows machine it is highly recommended that you go back and try and install or reinstall Rtools35.exe at: https://cran.r-project.org/bin/windows/Rtools/",
            "It is also located on your USB drive.\n.",
            "When you install it be sure to check the box that says to include Rtools in your PATH.",
            "",
            "If you are working on a mac At the very least on your mac you should be able to open up a terminal and type 'gcc --version'",
            "If this command produces an error then you need to type into your terminal 'xcode-select --install' and press 'Install' when the menu is presented.'",
            "You may also need to do one or two more additional steps. Please see our notes on the usb drive about building packages on a mac.",
            "",
            sep="\n"))
  Sys.sleep(5)
  proceed.answer = readInput(question="Despite the fact that you have a windows machine where we cannot detect Rtools or you are working on a mac would you like to proceed with installation?")
  if (proceed.answer != "yes")
  {
    cat(paste("You have not answered 'yes' to the question would you like to proceed despite not having an installation of Rtools we can detect.",
              "Please go back and try and install or reinstall Rtools35.exe at: https://cran.r-project.org/bin/windows/Rtools/ if you are working on windows",
              "It is also located on your USB drive.\n.",
              "When you install it be sure to check the box that says to include Rtools in your PATH.",
              "If you are working on a mac please see the notes for on the usb drive about building packages on a mac",sep="\n"))
    stop()
  }
}

# the student has rtools installed. next we ask them if they have conda installed.
answer = readInput(question="Do you have have Anaconda3-5.2 installed on your machine?")
if (answer != "yes")
{
  cat(paste("You have not answered 'yes' to the question do you have Anaconda3-5.2 installed on your machine.",
            "Please go and install the software on your usb drive or at https://repo.anaconda.com/archive/", sep="\n"))
  stop()
} else
{
  
  remove.packages.answer = readInput(question="To be sure you get a clean installation we are going to remove any prior installation of the the following packages from your machine: 'tensorflow', 'keras' and 'reticulate'. Is this okay?")
  if (answer != "yes")
  {
    my.tensorflow.config = FALSE
    my.python.config = FALSE
    try({
      # check that it installed without issue
      my.tensorflow.config = tensorflow:::tf_config()
      tensor.flow.is.avaiable = my.tensorflow.config$available
      configured.on.system.without.issue = TRUE
      
      # check for numpy
      my.python.config = reticulate::py_config()
      found.numpy.without.issue = !(is.null(my.python.config$numpy))
    })
    
    cat(paste("You have not answered 'yes' to the question is it okay to remove any prior installations of 'tensorflow', 'keras', and 'reticulate'.",
              "To avoid this step you must already have an installation of tensorflow and keras up and running.",
              "Show an instructor that this is the case or rerun the script and answer 'yes' to this question.", sep="\n"))
    
    if (my.tensorflow.config)
    {
      cat(paste("The student has a valid tensorflow configured.", sep="\n"))
    }
    if(my.python.config)
    {
      cat(paste("The student has a valid python environment configured with numpy.", sep="\n"))
    }
    
    stop()
  }
  cat(paste("Trying to remove any prior installations of tensorflow, keras and reticulate.", sep="\n"))
  remove.packages(c("tensorflow", "keras", "reticulate"))
  
  Sys.sleep(5)
  cat(paste("Trying to install tensorflow and keras using Rtools from github.",
            "You may be asked to install or update other packages",
            "Always select '1: All' to this question.",
            "You also may be asked to if it is okay to restart R at times",
            "You should say 'yes' to this question.", sep = "\n"))
  Sys.sleep(5)
  
  try({
    devtools::install_github("rstudio/tensorflow")
    devtools::install_github("rstudio/keras")
  })
  
  
  
  if (require(tensorflow))
  {
    
    installed.onto.system.without.issue = FALSE
    configured.on.system.without.issue = FALSE
    found.numpy.without.issue = FALSE
    loaded.keras.without.issue = FALSE
    try({
      # try and install tensorflow using the anaconda installation the student indcated s/he had.
      tensorflow::install_tensorflow(method="conda")
      installed.onto.system.without.issue = TRUE
      
      # check that it installed without issue
      my.tensorflow.config = tensorflow:::tf_config()
      tensor.flow.is.avaiable = my.tensorflow.config$available
      configured.on.system.without.issue = TRUE
      
      # check for numpy
      my.python.config = reticulate::py_config()
      found.numpy.without.issue = !(is.null(my.python.config$numpy))
      
      loaded.keras.without.issue = require(keras) 
    })
    # print out these diagnostics
    if (installed.onto.system.without.issue == FALSE)
    {
      cat(paste("Could not build tensorflow properly on your system.",
                "Be sure you are running as administration.",
                "Also you can try uninstalling RTools and R and then reinstalling the version of R version 3.6.1 (2019-07-05) -- 'Action of the Toes' and Rtools35.exe provided on the usb drive.",
                "Then rerun this script.",
                "If that still does not try the following: uninstall and reinstall Anaconda3-5.2. Once reinstalled goto a command prompt and type 'conda update --all'.",
                "Note this may take a while to complete. Once that is finished. Try to rerun this script.", sep="\n"))
    }
    else if (configured.on.system.without.issue == FALSE)
    {
      cat(paste("Could not configure tensorflow properly on your system.",
                "Try the following: uninstall and reinstall Anaconda3-5.2. Once reinstalled goto a command prompt and type 'conda update --all'.",
                "Note this may take a while to complete. Once that is finished. Try to rerun this script.", sep="\n"))
    }
    else if (found.numpy.without.issue == FALSE)
    {
      cat(paste("Could not find a numpy installation on your system.",
                "Try the following: uninstall and reinstall Anaconda3-5.2. Once reinstalled goto a command prompt and type 'conda update --all'.",
                "Note this may take a while to complete. Once that is finished. Try to rerun this script.", sep="\n"))
    }
    else if (loaded.keras.without.issue == FALSE)
    {
      cat(paste("Could not laod keras.",
                "Try and install keras via cran by typing:  install.packages(\"tensorflow\", repos='http://cran.us.r-project.org') in the console below.",
                "Note to instructor: I have not seen this issue arise yet but it should be minor.",
                "Getting a valid tensorflow configuration is the big hurdle keras just uses that configuration.", sep="\n"))
    }
    else
    {
      belt.and.suspenders.test = FALSE
      try({
        tensorflow::use_session_with_seed(42)
        belt.and.suspenders.test = TRUE
      })
      if (belt.and.suspenders.test)
      {
        cat(paste("Congratulations! Everything looks like it is installed and configured properly."))
      }
      else
      {
        cat(paste("Everything seemed fine but failed to run a simple set session seed test.",
                  "Output of tf_config is",
                  tensor.flow.is.avaiable, 
                  "I have never seen this before. Please show this to an instructor.",
                  sep="\n"))
      }
    }
  }
  else
  {
    cat(paste("Could not install tensorflow via github.",
              "We are now trying to install tensorflow and keras from CRAN", sep="\n"))
    remove.packages(c("tensorflow, keras", "reticulate"))
    packages = c("tensorflow", "keras")
    problematic.packages = c()
    package.check <- lapply(packages, FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        loaded.correctly = FALSE
      }
    })
    if (require(tensorflow))
    {
      
      installed.onto.system.without.issue = FALSE
      configured.on.system.without.issue = FALSE
      found.numpy.without.issue = FALSE
      loaded.keras.without.issue = FALSE
      
      try({
        # try and install tensorflow using the anaconda installation the student indcated s/he had.
        tensorflow::install_tensorflow(method="conda")
        installed.onto.system.without.issue = TRUE
        
        # check that it installed without issue
        my.tensorflow.config = tensorflow:::tf_config()
        tensor.flow.is.avaiable = my.tensorflow.config$available
        configured.on.system.without.issue = TRUE
        
        # check for numpy
        my.python.config = reticulate::py_config()
        found.numpy.without.issue = !(is.null(my.python.config$numpy))
        
        loaded.keras.without.issue = require(keras) 
      })
      # print out these diagnostics
      if (installed.onto.system.without.issue == FALSE)
      {
        cat(paste("Could not build tensorflow properly on your system even using CRAN.",
                  "This is a larger problem with installing packages in general as Rtools35.exe is not even required for this.",
                  "Please try to see if you can install any package from CRAN on your system.",
                  "Be sure you are running as administration.",
                  "Also you can try uninstalling RTools and R and then reinstalling the version of R version 3.6.1 (2019-07-05) -- 'Action of the Toes' and Rtools35.exe provided on the usb drive.",
                  "Then rerun this script.",
                  "If that still does not try the following: uninstall and reinstall Anaconda3-5.2. Once reinstalled goto a command prompt and type 'conda update --all'.",
                  "Note this may take a while to complete. Once that is finished. Try to rerun this script.", sep="\n"))
      }
      else if (configured.on.system.without.issue == FALSE)
      {
        cat(paste("Could not configure the tensorflow installation from CRAN properly on your system.",
                  "Be sure you are running as administration.",
                  "Also you can try uninstalling RTools and R and then reinstalling the version of R version 3.6.1 (2019-07-05) -- 'Action of the Toes' and Rtools35.exe provided on the usb drive.",
                  "Then rerun this script.",
                  "In addition you can try the following: uninstall and reinstall Anaconda3-5.2. Once reinstalled goto a command prompt and type 'conda update --all'.",
                  "Note this may take a while to complete. Once that is finished. Try to rerun this script.", sep="\n"))
      }
      else if (found.numpy.without.issue == FALSE)
      {
        cat(paste("Could not find a numpy installation on your system.",
                  "Try the following: uninstall and reinstall Anaconda3-5.2. Once reinstalled goto a command prompt and type 'conda update --all'.",
                  "Note this may take a while to complete. Once that is finished. Try to rerun this script.", sep="\n"))
      }
      else if (loaded.keras.without.issue == FALSE)
      {
        cat(paste("Could not laod keras but d",
                  "Try and install keras via cran by typing:  install.packages(\"tensorflow\", repos='http://cran.us.r-project.org') in the console below.",
                  "Note to instructor: I have not seen this issue arise yet but it should be minor.",
                  "Getting a valid tensorflow configuration is the big hurdle keras just uses that configuration.", sep="\n"))
      }
      else
      {
        belt.and.suspenders.test = FALSE
        try({
          tensorflow::use_session_with_seed(42)
          belt.and.suspenders.test = TRUE
        })
        if (belt.and.suspenders.test)
        {
          cat(paste("Congratulations! Everything looks like it is installed and configured properly."))
        }
        else
        {
          cat(paste("Everything seemed fine but failed to run a simple set session seed test.",
                    "Output of tf_config is",
                    tensor.flow.is.avaiable, 
                    "I have never seen this before. Please show this to an instructor.",
                    sep="\n"))
        }
      }
    }
  }
}