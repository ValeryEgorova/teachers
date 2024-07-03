#-------------------------------------------------------------------
# Project: Teachers
# Organization: 
# Author: 
# Date: 03 July 2024
#-------------------------------------------------------------------

# for Windows
USERNAME    <- Sys.getenv("USERNAME")
# for Mac
USER        <- Sys.getenv("USER")

if (USERNAME == "Админ") {
  projectFolder  <- getwd()
} 

if (USERNAME == "Valery") {
  projectFolder  <- getwd()
} 

if (USER == "Админ") {
  projectFolder  <- getwd()
} 

# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))

# set up key folders
documentation <-  file.path(projectFolder, "00_documentation")
inputData     <-  file.path(projectFolder, "01_input_data")
rcodes        <-  file.path(projectFolder, "02_codes")
outData       <-  file.path(projectFolder, "03_outputs/0301_data")
outTables     <-  file.path(projectFolder, "03_outputs/0302_tables")  
outFigures    <-  file.path(projectFolder, "03_outputs/0303_figures")


stopifnot(dir.exists(documentation))
stopifnot(dir.exists(inputData))
stopifnot(dir.exists(rcodes))
stopifnot(dir.exists(outData))
stopifnot(dir.exists(outTables))
stopifnot(dir.exists(outFigures))

