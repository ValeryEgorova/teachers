#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Install packages needed for the analysis
# Author: Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------


# List of required packages

required_packages <- c("tidyverse",
                       "haven",
                       "readxl",
                       "psych",
                       "foreign",
                       "factoextra",
                       "corrr",
                       "ggcorrplot",
                       "writexl",
                       "car",
                       "ggfortify",
                       "mgcv",
                       "erer",
                       "caret",
                       "e1071",
                       "Hmisc",
                       "naniar",
                       "ggpubr",
                       "DescTools",
                       "erer",
                       "ggridges",
                       "factoextra",
                       "cluster")


# Function to check and install packages
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      install.packages(package)
    }
    library(package, character.only = TRUE)
  }
}


# Call the function with the list of required packages
check_and_install_packages(required_packages)
