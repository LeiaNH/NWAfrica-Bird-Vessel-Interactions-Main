# mainBRT.R
# Project: NWAfrica-Bird-Vessel-Interactions-Main
# 
# This script runs the complete Boosted Regression Trees (BRT) modeling workflow
# for seabird-vessel interaction data from the study:"Seabird-vessel interactions in industrial fisheries of Northwest Africa:implications for international bycatch management" (Navarro-Herrero et al., 2025). 
# It includes data preprocessing and exploratory analysis, hyperparameter tuning, fitting of the final full models per species, and generation of bootstrap replicates to assess model stability and uncertainty.

# -----------------------------------------------------------------
# Workflow
# Step 1. Exploratory data analysis and preprocessing
# Step 2. BRT hyperparameter tuning
# Step 3. Fit full BRT model with best parameters
# Step 4. Fit BRT full model with bootstrap replicates
# 
# Note: Parallelization uses 3 CPU cores by default; adjust as needed.
# -----------------------------------------------------------------

#---------------------------------------------------------------
# Working directory and requirements
#---------------------------------------------------------------

source("code/setup.R")  # load paths, packages, and global settings

#---------------------------------------------------------------
# Processing workflow
#---------------------------------------------------------------

#---------------------------------------------------------------
# Step 1. Exploratory data analysis and preprocessing
#---------------------------------------------------------------

# This step performs variable renaming, filtering, and correlation-based
# variable selection using Spearman coefficients. Variables exceeding
# a correlation threshold are removed based on priority rules. It also
# creates visual summaries of variables and response distributions.

threshold <- 0.7  # Spearman correlation threshold

source("code/BRT/eda_correlations.R")

#---------------------------------------------------------------
# Step 2. BRT hyperparameter tuning
#---------------------------------------------------------------

# This step runs BRT models across a grid of hyperparameters in parallel.
# Each species is processed separately. The best-performing hyperparameter
# combination is saved based on performance metrics (e.g., deviance, AUC).

mod_code <- "brt"  # model label for output files
cores <- 3         # number of CPU cores to use in parallel runs

source("code/BRT/BRTfit.R")

#---------------------------------------------------------------
# Step 3. Fit full BRT model with best parameters
#---------------------------------------------------------------

# Using the best hyperparameters from tuning, a full model is trained
# for each species. The script outputs the final fitted model and performance metrics.

mod_code <- "brt"  # model label for output files
cores <- 3         # number of CPU cores to use in parallel runs

source("code/BRT/BRTfitfullmodel.R")

#---------------------------------------------------------------
# Step 4. Fit BRT full model with bootstrap replicates
#---------------------------------------------------------------

# This step runs multiple bootstrap replicates (e.g., 50) per species using
# the best hyperparameters. It saves each model to allow downstream analyses
# of variability, stability, and confidence intervals.

mod_code <- "brt"  # model label for output files
cores <- 3         # number of CPU cores to use in parallel runs

source("code/BRT/BRTfitfullmodelBootstrap.R")


