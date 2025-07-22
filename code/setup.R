# setup.R
# Setup project to set working directory and load  libraries
# Project: NWAfrica-Bird-Vessel-Interactions-Main 

#-----------------------------------------------
# set computer
#-----------------------------------------------

cpu <- "laptop"  

# Set main data paths
if(cpu == "laptop") main_dir <- "C:/Users/lnh88/Dropbox/GitHub_LNH/NWAfrica-Bird-Vessel-Interactions-Main"

#-----------------------------------------------
# Create data paths
#-----------------------------------------------

input_dir <- paste(main_dir, "data/input", sep="/")

output_dir <- paste(main_dir, "data/output", sep="/")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

#-----------------------------------------------
# Load required packages
#-----------------------------------------------

#install.packages("devtools") # in case "devtools" has not already been installed
#remotes::install_github("JBjouffray/ggBRT") # will take several minutes to install
#devtools::install_github("JBjouffray/ggBRT") # will take several minutes to install
#remotes::install_github("davidsjoberg/ggsankey")

pacman::p_load("tidyr", "dplyr", "purrr","stringr", "DescTools", "splitstackshape",  # tidy
               "data.table", "readr", # reading
               "hms", "lubridate", "suncalc", # time operations
               "ggplot2", "ggalluvial", "ggpubr", "Hmisc", "visdat",
               "fmsb", "gridExtra", "rworldxtra", 
               "rnaturalearth", "egg", "metR", "ggsankey","scales",# plots
               "flextable", "officer", #tables
               "doParallel", "parallel", # parallelizing 
               "sf", "raster", "sp", "terra", # spatial data
               "groupdata2", "gbm", "ggBRT", "dismo") #BRT modelling

