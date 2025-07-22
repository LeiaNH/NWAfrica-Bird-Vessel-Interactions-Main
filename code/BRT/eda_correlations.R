# eda_correlations.R
#---------------------------------------------------------------
# Exploratory Data Analysis (EDA) of predictor variables for Boosted Regression Tree (BRT) modeling:
# includes missing data visualization, correlation clustering, variable filtering based on correlation and priority,
# distribution plots of numerical and categorical predictors, and preparation of cleaned data for subsequent modeling.
#---------------------------------------------------------------

#---------------------------------------------------------------
# Workflow 
# 1. Load species-specific input datasets (level 1)
# 2. Visualize missing data patterns
# 3. Compute and plot Spearman correlation clustering of numeric predictors
# 4. Identify and remove highly correlated variables based on a correlation threshold and variable priority
# 5. Visualize distributions of numerical and categorical variables by response class
# 6. Save cleaned datasets for BRT modeling (level 2)
#---------------------------------------------------------------

#-----------------------------------------------
# Step 1. Load species-specific input datasets (level 1) 
#-----------------------------------------------

# List all subdirectories within the input directory
folders <- list.files(input_dir, recursive = FALSE)

# Extract species codes from folder names
sp <- sub("_.*", "", folders)

# Loop through each species
for (i in seq_along(sp)) {
  
  # Load preprocessed input data for the species
  data <- readRDS(paste0(input_dir, "/", sp[i], "_inputBRT_L1.rds"))
  
  # Rename selected variables for clarity
  data <- data %>%
    rename(
      LENGTH = vessellength,
      WINDS = WSPEED,
      VESSELS = NfishVess,
      NAVSTAT = NavStat,
      GEAR = geartype,
      LIGHT = lightTime
    )
  
  # Variables to exclude from analysis (non-predictors)
  drop <- c("codeName", "organismID", "randNum", "Response")
  
  # Identify numeric and non-numeric (categorical) variables to include in EDA
  num_vars <- names(data)[sapply(data, is.numeric)]
  num_vars <- num_vars[!(num_vars %in% drop)]
  
  non_num_vars <- names(data)[!sapply(data, is.numeric)]
  non_num_vars <- non_num_vars[!(non_num_vars %in% drop)]
  
  #-----------------------------------------------
  # Step 2. Visualize missing data patterns
  #-----------------------------------------------
  outdir <- paste0(output_dir, "/", sp[i], "/predictors/")
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # Plot missing data overview
  pngfile <- paste0(outdir, "/", sp[i], "_eda_missing.png")
  png(pngfile, width=1500, height=1000, res=200)
  p <- visdat::vis_miss(data, warn_large_data = FALSE)
  print(p)
  dev.off()
  
  #-----------------------------------------------
  # Step 3. Compute and plot Spearman correlation clustering of numeric predictors
  #-----------------------------------------------
  
  pngfile <- paste0(outdir, "/clusterEDA.png")
  png(pngfile, width=2500, height=2000, res=300)
  op <- par(mar=c(0.5,5,0.5,0.5))
  v <- as.formula(paste("~", num_vars, collapse = "+"))
  plot(Hmisc::varclus(v, similarity = "spearman", data = data), cex = 0.8)
  abline(a = 1 - threshold, 0, col = "grey70", lty = 1, lwd = 5)
  par(op)
  dev.off()
  
  #-----------------------------------------------
  # Step 4. Identify and remove highly correlated variables based on correlation threshold and priority
  #-----------------------------------------------
  
  # Extract correlation matrix from clustering object
  similarity_matrix <- Hmisc::varclus(v, similarity = "spearman", data = data)[[2]]
  diag(similarity_matrix) <- 0  # Remove self-correlations
  
  # Convert correlation matrix to long format
  corr_df <- as.data.frame(similarity_matrix)
  corr_df$var1 <- rownames(corr_df)
  corr_df_long <- tidyr::pivot_longer(corr_df, -var1, names_to = "var2", values_to = "correlation")
  
  # Filter for variable pairs with high correlation (above threshold, below 1)
  cor_pairs <- corr_df_long %>%
    filter(correlation > threshold & correlation < 1) %>%
    mutate(var1 = factor(var1), var2 = factor(var2)) %>%
    arrange(desc(correlation))
  
  # Read variable priority info to resolve which variable to keep

  covar <- c("fishingHours", "nvessels", "length", "BAT", "SLP", "CHL", "NPP", "SMTD",
             "COASTD", "CAND", "SST", "SSTg", "SAL", "SALg", "SSH", "EKE", "MLD",
             "WSPEED", "FSLE", "EDDY")
  
  priority <- 1:20
  
  priorityCovar <- data.frame(covar, priority)
  
  #print(priorityCovar)
  
  priorityCovar <- priorityCovar %>%
    mutate(var1 = covar, var2 = covar, priority1 = priority, priority2 = priority) %>%
    select(-covar, -priority)
  
  # Merge priority info into correlation pairs
  cor_pairs <- cor_pairs %>%
    left_join(priorityCovar %>% select(var1, priority1), by = "var1") %>%
    left_join(priorityCovar %>% select(var2, priority2), by = "var2")
  
  # Select lower-priority variable for removal
  drop <- cor_pairs %>%
    mutate(sel = if_else(priority1 > priority2, var1, var2)) %>%
    select(sel) %>%
    distinct() %>%
    pull()
  
  # Update final set of numeric variables
  num_vars <- num_vars[!(num_vars %in% drop)]
  
  #-----------------------------------------------
  # Step 5. Visualize distributions of numerical and categorical variables by response class
  #-----------------------------------------------
  
  # ---------
  # Numerical
  # ---------
  
  # Melt data to long format for density plots
  data_long <- reshape2::melt(data, id.vars = "Response", measure.vars = num_vars)
  
  # Set colors for each response class
  col1 <- hsv(0.85, 0.6, 0.8, 0.5)
  col2 <- hsv(0.55, 0.6, 0.8, 0.5)
  data_long$type <- factor(data_long$Response, levels = c("0", "1"))
  
  # Plot density of each numerical variable by response
  p <- ggplot(data_long, aes(x = value, fill = type)) +
    geom_density(color = "#e9ecef", alpha = 0.5) +
    scale_fill_manual(values = c(col1, col2)) +
    facet_wrap(~variable, scales = "free", ncol = 4) +
    labs(x = "", y = "Density") +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())
  
  pngfile <- paste0(outdir, "/numerical.png")
  ggsave(pngfile, p, width = 25, height = 20, units = "cm", dpi = 300)
  
  # ---------
  # Categorical
  # ---------
  
  # Ensure Response is a factor
  data$Response <- as.factor(data$Response)
  
  # Helper function to generate plots for categorical variables
  plot_cat <- function(data, response_var, cat_vars) {
    plots <- lapply(cat_vars, function(var) {
      ggplot(data, aes_string(x = var, fill = response_var)) +
        geom_histogram(position = "stack", stat = "count") +
        scale_fill_manual(values = c(col1, col2)) +
        labs(title = var, x = NULL, y = "Count") +
        egg::theme_article() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    })
    gridExtra::grid.arrange(grobs = plots, ncol = 3)
  }
  
  # Generate and save plot
  p <- plot_cat(data, "Response", non_num_vars)
  pngfile <- paste0(outdir, "/categorical.png")
  ggsave(pngfile, p, width = 25, height = 20, units = "cm", dpi = 300)
  
  #-----------------------------------------------
  # Step 6. Save cleaned datasets for BRT modeling (level 2)
  #-----------------------------------------------
  
  # Variables to retain in final dataset
  keep <- c("Response", "randNum", "organismID")
  vars <- c(keep, num_vars, non_num_vars)
  
  # Select and save final variables
  data <- data %>%
    select(all_of(vars))
  
  saveRDS(data, paste0(output_dir, "/", sp[i], "/", sp[i],"_inputBRT_L2.rds"))
}

#
#
#
#
#
# Clean up
tokeep <- c("output_dir", "input_dir", "cpu")
rm(list = setdiff(ls(), tokeep))
