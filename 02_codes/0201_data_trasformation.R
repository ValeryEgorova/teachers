#-------------------------------------------------------------------
# Project: 
# Organization: 
# Objective: Prepare a dataset for the analysis
# Author:  Valeria Egorova
# Date: 03 July 2024
#-------------------------------------------------------------------

# Read the script to uploaded necessary libraries
source(file.path(rcodes, "0200_load_packages.R"))


# Read files and delete differing variables
data_general <- read_excel(file.path(inputData,"data_general.xlsx")) %>%
 
  write_dta(file.path(outData,"scales.dta"))

