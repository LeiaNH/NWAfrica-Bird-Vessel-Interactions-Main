# BRTfit.R
#---------------------------------------------------------------
# Boosted Regression Tree (BRT) model training and 
# hyperparameter optimization using cross-validation for 
# species-specific datasets.
#
# Code adapted from:
# https://zenodo.org/record/5701286#.ZGI6hHbP2Uk
# Originally developed for March et al. (2021), *Scientific Reports*
# "Winter distribution of juvenile and sub-adult male Antarctic 
# fur seals (Arctocephalus gazella)..."
#
# Hyperparameter grid search is performed over:
#   - learning rate (lr)
#   - tree complexity (tc)
#   - bag fraction (bf)
#
# Simpler models with higher tree counts are preferred, following
# Elith et al. (2008). Only models with >1,000 trees are retained.
#---------------------------------------------------------------

#---------------------------------------------------------------
# Workflow
# Step 1. Read observations
# Step 2. Set output directory
# Step 3. Prepare data
# Step 4. Define cross-validation folds
# Step 5. BRT hyperparameter optimization
# Step 6. Combine model outputs
# Step 7. Export model diagnostics
#---------------------------------------------------------------

#-----------------------------------------------
# Step 1. Read observations
#-----------------------------------------------

# List all subdirectories within the output directory
folders <- list.dirs(output_dir, recursive = FALSE)

# Extract species codes from folder names
sp <- basename(folders)

# Loop through each species
for (i in seq_along(sp)) {
  
  print(sp[i])
  
  # Load species-specific dataset
  data <- readRDS(paste0(output_dir, "/", sp[i], "/", sp[i], "_inputBRT_L2.rds"))
  
  #-----------------------------------------------
  # Step 2. Set output directory for results
  #-----------------------------------------------
  
  outdir <- paste0(output_dir, "/", sp[i])
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  #-----------------------------------------------
  # Step 3. Prepare data for BRT
  #-----------------------------------------------
  
  # Log-transform skewed variables
  data$EKE     <- log1p(data$EKE)
  data$CHL     <- log1p(data$CHL)
  data$VESSELS <- log1p(data$VESSELS)
  
  #-----------------------------------------------
  # Step 4. Define cross-validation folds
  #-----------------------------------------------
  
  # Use Leave-One-Out CV for LARAUD species; else use 10-fold CV
  if(sp[i] == "LARAUD") {
    k_value <- length(unique(data$organismID))
  } else {
    k_value <- 10
  }
  
  # Set reproducibility seed and generate folds by individual ID
  set.seed(123)
  foldset <- groupdata2::fold(data, k = k_value, id_col = 'organismID') 
  
  # Format and append fold assignments
  data <- foldset %>%
    rename(folds = .folds) %>%
    dplyr::mutate(folds = as.numeric(folds)) %>%
    as.data.frame()
  
  # Check fold structure and export summary
  sz <- data %>%
    group_by(folds) %>%
    dplyr::summarize(nObs = n(), nIndividuals = unique(length(organismID)))
  
  write.csv(sz, paste0(outdir, "/structure.csv"), row.names = FALSE)
  
  #-----------------------------------------------
  # Step 5. BRT: Hyperparameter optimization
  #-----------------------------------------------
  
  # Convert folds and response to appropriate formats
  data$folds <- as.factor(data$folds)
  data$Response <- as.numeric(as.character(data$Response))
  
  # Save prepped data
  saveRDS(data, paste0(outdir, "/data.rds"))
  
  # Identify predictor variables
  drop <- c("organismID", "Response", "folds")
  vars <- setdiff(names(data), drop)
  
  # Define tree iterations
  ini.nt <- 50
  max.nt <- 10000
  step.nt <- 50
  tree.list <- seq(ini.nt, max.nt, by = step.nt)
  
  # Define hyperparameter combinations to evaluate
  comb <- expand.grid(
    lr = c(0.001, 0.005, 0.01, 0.05),
    tc = c(1, 3, 5),
    bf = c(0.5, 0.6, 0.7)
  )
  
  # Initialize parallel backend
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Run BRT models for each parameter combination
  all_list <- foreach(j = 1:nrow(comb), .packages = c("dismo", "gbm", "dplyr")) %dopar% {
    set.seed(131)
    
    mod <- tryCatch(
      dismo::gbm.step(
        data = data,
        gbm.x = vars,
        gbm.y = "Response",
        family = "bernoulli",
        tree.complexity = comb$tc[j],
        learning.rate = comb$lr[j],
        bag.fraction = comb$bf[j],
        fold.vector = data$folds,
        n.folds = length(unique(data$folds)),
        n.trees = ini.nt,
        step.size = step.nt,
        max.trees = max.nt
      ),
      error = function(e) return(NULL)
    )
    
    if (!is.null(mod)) {
      
      saveRDS(mod, file = paste0(outdir, "/", mod_code, "gbmStep.rds"))
      
      mod_out <- data.frame(
        tree.complexity = mod$interaction.depth,
        learning.rate = mod$shrinkage,
        bag.fraction = mod$bag.fraction,
        n.trees = mod$n.trees,
        AUC = mod$self.statistics$discrimination,
        cv.AUC = mod$cv.statistics$discrimination.mean,
        deviance = mod$self.statistics$mean.resid,
        cv.deviance = mod$cv.statistics$deviance.mean,
        PER = (1 - mod$self.statistics$mean.resid / mod$self.statistics$mean.null) * 100,
        cv.PER = (1 - mod$cv.statistics$deviance.mean / mod$self.statistics$mean.null) * 100
      )
      
      # Extract cross-validated deviance values for each tree
      cv_deviance <- mod$cv.values
      cv_deviance <- c(cv_deviance, rep(NA, length(tree.list) - length(cv_deviance)))
      
      # Identify most important predictors
      pred_order <- summary(mod)$var
      rn_position <- which(pred_order == "randNum")
      pred_list <- as.character(pred_order[1:(rn_position - 1)])
      
      list(mod_out = mod_out, cv_deviance = cv_deviance, pred_list = pred_list)
    }
  }
  
  #-----------------------------------------------
  # Step 6. Combine model outputs across combinations
  #-----------------------------------------------
  
  # Collate metrics
  mod_list <- foreach(i = 1:nrow(comb)) %dopar% all_list[[i]]$mod_out
  mod_list[!lengths(mod_list)] <- list(data.frame(tree.complexity = NA))
  mod_out <- rbindlist(mod_list, fill = TRUE)
  
  # Append parameter combinations to model output
  mod_out <- bind_cols(comb, dplyr::select(mod_out, -c(tree.complexity, learning.rate, bag.fraction))) %>%
    dplyr::mutate(id = 1:n())
  
  # Collate deviance profiles
  deviance_list <- list()
  for (i in 1:nrow(mod_out)) {
    dev <- all_list[[i]]$cv_deviance
    if (is.null(dev)) dev <- rep(NA, length(tree.list))
    deviance_list[[i]] <- data.frame(
      id = mod_out$id[i],
      lr = mod_out$lr[i],
      tc = mod_out$tc[i],
      bf = mod_out$bf[i],
      ntrees = tree.list,
      cv_deviance = dev
    )
  }
  cv_deviance <- rbindlist(deviance_list)
  
  # Collect selected predictors
  predict_list <- foreach(i = 1:nrow(comb)) %dopar% all_list[[i]]$pred_list
  
  # Stop cluster after processing
  stopCluster(cl)
  
  #-----------------------------------------------
  # Step 7. Export model diagnostics 
  #-----------------------------------------------
  
  # Plot deviance profiles
  p <- ggplot(data = cv_deviance) +
    geom_line(data = dplyr::rename(cv_deviance, comb = id), 
              aes(x = ntrees, y = cv_deviance, group = comb), color = "grey80") +
    geom_line(aes(x = ntrees, y = cv_deviance, group = id), color = "firebrick3") +
    scale_x_continuous(limits = c(0, max(cv_deviance$ntrees[!is.na(cv_deviance$cv_deviance)]))) +
    facet_wrap(id ~ .) +
    theme_bw()
  
  ggsave(paste0(outdir, "/optim_params.png"), p, width = 25, height = 14, units = "cm", dpi = 300)
  
  # Export model outputs
  write.csv(mod_out, paste0(outdir, "/optim_params.csv"), row.names = FALSE)
  write.csv(cv_deviance, paste0(outdir, "/cv_deviance.csv"), row.names = FALSE)
  saveRDS(predict_list, paste0(outdir, "/predlist.rds"))
}


#
#
#
#
#
# Clean up
tokeep <- c("output_dir", "input_dir", "cpu")
rm(list = setdiff(ls(), tokeep))
