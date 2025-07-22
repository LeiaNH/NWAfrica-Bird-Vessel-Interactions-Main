# BRTfitfullmodel.R
#---------------------------------------------------------------
# Fit final Boosted Regression Tree (BRT) models using selected 
# hyperparameters across multiple species. Model selection is 
# based on AUC and learning rate, followed by model fitting and 
# visualizations of variable importance and response curves.
#---------------------------------------------------------------

#---------------------------------------------------------------
# Workflow
# Step 1. Select optimal hyperparameter sets
# Step 2. Fit full BRT model for each species
# Step 3. Visualizations
#---------------------------------------------------------------


#-----------------------------------------------
# Step 1. Select optimal hyperparameter sets
#-----------------------------------------------

# List all subdirectories within the output directory
folders <- list.dirs(output_dir, recursive = FALSE)

# Extract species codes from folder names
sp <- basename(folders)

bestMod <- list()  # container for top models per species

for (i in seq_along(sp)) {
  #i=3
  if (file.exists(paste0(output_dir, "/", sp[i], "/optim_params.csv"))) {
    
    mod_out <- read.csv(paste0(output_dir, "/", sp[i], "/optim_params.csv"))
    
    # Filter: models with >1000 trees
    mod_out <- mod_out %>% dplyr::filter(n.trees > 1000)
    
    # Keep top 5 AUC-scoring models
    mod_out <- mod_out %>% slice_max(AUC, n = 5)
    
    # From those, keep models with the lowest learning rates
    mod_out <- mod_out %>% slice_min(lr)
    
    ID <- mod_out %>% pull(id)  # extract model IDs
    
    if (length(ID) > 0) {
      t <- mod_out %>% dplyr::filter(id %in% ID)
      t$CodeName <- sp[i]
      bestMod[i] <- list(t)
    }
  }
}

# Combine top models across all species
Modes <- rbindlist(bestMod)

# Print selection summary
Modes %>% dplyr::select(id, CodeName, AUC, cv.AUC, lr)

#-----------------------------------------------
# Step 2. Fit full BRT model for each species
#-----------------------------------------------

# List all subdirectories within the input directory
folders <- list.dirs(output_dir, recursive = FALSE)

# Extract species codes from folder names
sp <- basename(folders)

for (i in seq_along(sp)) {
  
  print(sp[i])
  
  #----------------------------------------------------
  # Set model ID manually (based on curve inspection)
  #----------------------------------------------------
  if (sp[i] == "CALBOR") model_id <- 9
  if (sp[i] == "CALEDW") model_id <- 23
  if (sp[i] == "LARAUD") model_id <- 21
  
  #----------------------------------------------------
  # Load data and selected model parameters
  #----------------------------------------------------
  outdir <- paste0(output_dir, "/", sp[i])
  
  data <- readRDS(paste0(outdir, "/data.rds"))
  vars <- names(data)
  
  mod_out <- read.csv(paste0(outdir, "/optim_params.csv"))
  predict_list <- readRDS(paste0(outdir, "/predlist.rds"))
  
  # Save chosen hyperparameters to disk
  mod_out %>%
    dplyr::slice(model_id) %>%
    write.csv(file = paste0(outdir, "/hyperpfullmodel.csv"), row.names = FALSE)
  
  # Extract chosen parameters
  tc <- mod_out$tc[model_id]
  lr <- mod_out$lr[model_id]
  bf <- mod_out$bf[model_id]
  ntrees <- mod_out$n.trees[model_id]
  pred_list <- vars[vars %in% predict_list[[model_id]]]
  
  # Save list of variables used
  original <- as.data.frame(names(data))
  final <- as.data.frame(pred_list)
  final$fit <- "fullmodel"
  vars <- merge(original, final, all.x = TRUE)
  write.csv(vars, paste0(outdir, "/variables.csv"), row.names = FALSE)
  
  #----------------------------------------------------
  # Fit final model (if option A is selected)
  #----------------------------------------------------
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  

    set.seed(131)
    mod_full <- dismo::gbm.fixed(
      data = data,
      gbm.x = pred_list,
      gbm.y = "Response",
      family = "bernoulli",
      tree.complexity = tc,
      learning.rate = lr,
      bag.fraction = bf,
      n.trees = ntrees
    )
    saveRDS(mod_full, file = paste0(outdir, "/", mod_code, ".rds"))
  
  
  # Load fitted model from disk
  mod_full <- readRDS(paste0(outdir, "/", mod_code, ".rds"))
  
  #----------------------------------------------------
  # Step 3. Visualizations: variable importance & response
  #----------------------------------------------------
  
  ## Variable importance values
  var_imp <- summary(mod_full)$rel.inf
  ImpData <- as.data.frame(matrix(c(var_imp), ncol = length(var_imp)))
  colnames(ImpData) <- summary(mod_full)$var
  
  ## Radar plot
  ImpData <- rbind(rep(max(var_imp), ncol(ImpData)), rep(0, ncol(ImpData)), ImpData)
  png(paste0(outdir, "/", mod_code, "_var_radar.png"), width = 1000, height = 1000, res = 150)
  fmsb::radarchart(ImpData)
  dev.off()
  
  ## Bar plot of variable influence
  png(paste0(outdir, "/", mod_code, "_var_influence.png"), width = 1000, height = 1000, res = 150)
  ggBRT::ggInfluence(mod_full, show.signif = FALSE, col.bar = "skyblue3")
  dev.off()
  
  ## Partial dependence plots
  png(paste0(outdir, "/", mod_code, "_response.png"), width = 1500, height = 1500, res = 200)
  names(mod_full$gbm.call)[1] <- "dataframe"
  ggBRT::ggPD(mod_full, n.plots = ncol(ImpData), smooth = FALSE, rug = FALSE, ncol = 3, col.line = "skyblue3")
  dev.off()
}

stopCluster(cl)  # stop parallel cluster

#
#
#
#
#
# Clean up
tokeep <- c("output_dir", "input_dir", "cpu")
rm(list = setdiff(ls(), tokeep))
