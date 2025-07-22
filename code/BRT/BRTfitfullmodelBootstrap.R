# BRTfitfullmodelBootstrap.R
#---------------------------------------------------------------
# Fit Boosted Regression Tree (BRT) full models with bootstrap
# resampling across multiple species using selected 
# hyperparameters. Generates multiple bootstrap replicates 
# per species to assess model variability and robustness.
#---------------------------------------------------------------

#---------------------------------------------------------------
# Workflow
# Step 1. List species folders and assign model IDs
# Step 2. Loop through species to load data and tuning results
# Step 3. Perform bootstrap resampling and model fitting in parallel
#---------------------------------------------------------------

#--------------------------------------------------------
# Step 1. List species folders and assign model IDs
#--------------------------------------------------------

# List all subdirectories within the output directory
folders <- list.dirs(output_dir, recursive = FALSE)

# Extract species codes from folder names
sp <- basename(folders)                            

#--------------------------------------------------------
# Step 2. Loop through species to load data and tuning results
#--------------------------------------------------------

for (i in seq_along(sp)) {
  
  print(sp[i])  # Track progress
  
  # Assign model ID based on species (predefined IDs)
  if (sp[i] == "CALBOR") model_id <- 9
  if (sp[i] == "CALEDW") model_id <- 23
  if (sp[i] == "LARAUD") model_id <- 21
  
  #--------------------------------------------------------
  # Step 3. Load data and prior hyperparameter tuning results
  #--------------------------------------------------------
  
  outdir <- paste0(output_dir, "/", sp[i])         # Species output directory
  data <- readRDS(paste0(outdir, "/data.rds"))    # Load preprocessed data
  
  vars <- names(data)                              # All variables in dataset
  
  mod_out <- read.csv(paste0(outdir, "/optim_params.csv"))  # Hyperparameter results
  predict_list <- readRDS(paste0(outdir, "/predlist.rds")) # Selected predictors
  
  # Use the predefined model_id to select hyperparameters and predictors
  select_model_id <- model_id
  
  # Extract hyperparameters for chosen model
  tc <- mod_out$tc[select_model_id]               # Tree complexity
  lr <- mod_out$lr[select_model_id]               # Learning rate
  bf <- mod_out$bf[select_model_id]               # Bag fraction
  ntrees <- mod_out$n.trees[select_model_id]      # Number of trees
  
  # Subset predictors based on optimization
  pred_list <- vars[vars %in% predict_list[[select_model_id]]]
  
  #--------------------------------------------------------
  # Step 4. Bootstrap resampling and model fitting
  #--------------------------------------------------------
  
  outdir_bootstrap <- paste0(outdir, "/bootstrap/")
  if (!dir.exists(outdir_bootstrap)) dir.create(outdir_bootstrap, recursive = TRUE)
  
  # Number of bootstrap model fits to perform
  n.boot <- 50
  
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  foreach(t = 1:n.boot, .packages = c("dismo", "gbm", "dplyr", "splitstackshape", "stringr")) %dopar% {
    
    # Stratified bootstrap sample: sample 50% of data with replacement, stratified by Response and organismID
    idata <- splitstackshape::stratified(data, c("Response", "organismID"), 0.5, replace = TRUE)
    
    # Fit BRT full model using fixed number of trees and hyperparameters
    mod_boot <- dismo::gbm.fixed(
      data = idata,
      gbm.x = pred_list,
      gbm.y = "Response",
      family = "bernoulli",
      tree.complexity = tc,
      learning.rate = lr,
      bag.fraction = bf,
      n.trees = ntrees
    )
    
    # Save the fitted bootstrap model
    outfile <- paste0(outdir_bootstrap, "/", stringr::str_pad(t, 2, pad = "0"), "_", sp[i], "_", mod_code, "_boot.rds")
    saveRDS(mod_boot, file = outfile)
  }
  
  stopCluster(cl)
}

#
#
#
#
#
# Clean up
tokeep <- c("output_dir", "input_dir", "cpu")
rm(list = setdiff(ls(), tokeep))

