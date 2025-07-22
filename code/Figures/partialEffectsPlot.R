# partialEffectsPlotBoot.R
# ---------------------------------------------------------------
# Figure 5 from:
# "Seabird-vessel interactions in industrial fisheries of Northwest Africa:
# implications for international bycatch management"
# Navarro-Herrero et al. (2025)
# ---------------------------------------------------------------

bootstrap <- T             # Enable bootstrapping
n_boot <- 50              # Number of bootstrap replicates to use

# -----------------------------
# 1. Identify species folders
# -----------------------------

folders <- list.dirs(paste0(output_dir), recursive = FALSE)   # List species directories in output_dir
sp <- basename(folders)  # Extract species codes (e.g., "CALBOR", "CALEDW")

plot_list <- list()      # Initialize list to store plots (top 4 predictors)

# -----------------------------
# 2. Loop over species and load models
# -----------------------------

for (i in seq_along(sp)){
  
  print(sp[i])
  # Load full BRT model for the species
  outdir <- paste0(output_dir, "/", sp[i])
  mod <- readRDS(paste0(outdir, "/brt.rds"))
  
  # Locate bootstrap model files for the species
  outdir_bootstrap <- paste0(output_dir, "/", sp[i], "/bootstrap/")
  boots_files <- list.files(outdir_bootstrap, pattern = "\\.rds$", full.names = TRUE)
  
  # Load all bootstrap models into a list
  set.seed(856)
  models <- lapply(boots_files, readRDS)
  n_models <- length(models)
  
  # -----------------------------
  # 3. Prepare prediction values and storage
  # -----------------------------
  
  # Prepare list of predictor values to evaluate partial effects (100 points each)
  n_res <- 100
  gbm_list <- ggBRT::plot.gbm.4list(models[[1]], continuous.resolution = n_res)
  
  # Get predictor variable names
  pred.names <- models[[1]]$var.names
  n_var <- length(pred.names)
  
  # Initialize array to store bootstrap predictions (dimensions: points x variables x bootstraps)
  boot_mat <- array(NA, dim=c(n_res, n_var, n_boot))
  
  # -----------------------------
  # 4. Generate bootstrap predictions for all models
  # -----------------------------
  
  for(t in 1:length(models)){
    mi <- models[[t]]
    
    # Predict partial effects for each predictor at the predefined values
    ipred_boot <- ggBRT::plot.gbm.boot(mi, list.4.preds = gbm_list, continuous.resolution = n_res)
    
    # Store predictions in the matrix
    boot_mat[,,t] <- ipred_boot
  } 
  
  # -----------------------------
  # 5. Calculate medians and confidence intervals across bootstrap replicates
  # -----------------------------
  
  boot_med <- apply(boot_mat, c(1,2), median, na.rm=T)
  boot_cil <- apply(boot_mat, c(1,2), quantile, prob = 0.025, na.rm=T)
  boot_ciu <- apply(boot_mat, c(1,2), quantile, prob = 0.975, na.rm=T)
  
  # -----------------------------
  # 6. Prepare data frame of partial effect summaries per variable
  # -----------------------------
  
  data_list <- list()
  for(s in 1:n_var){
    print(s)  # progress output
    
    var = pred.names[s]
    xval = gbm_list[[s]]$X1
    
    med = boot_med[,s]
    med =  med[!is.na(med)]
    
    cil = boot_cil[,s]
    cil =  cil[!is.na(cil)]
    
    ciu = boot_ciu[,s]
    ciu =  ciu[!is.na(ciu)]
    
    # Data frame with variable name, predictor values, median prediction, and CI bounds
    idf <- data.frame(
      var = var,
      xval = xval,
      med = med,
      cil = cil,
      ciu = ciu)
    
    # Append to list
    data_list[[s]] <- idf
  }
  
  # Combine all variable data frames into one
  data <- rbindlist(data_list)
  
  # -----------------------------
  # 7. Prepare variable importance labels and filter variables to plot
  # -----------------------------
  
  data$var <- factor(data$var, levels = mod$contributions$var)
  
  relinf <- round(mod$contributions$rel.inf, 1)
  labels <- paste0(mod$contributions$var, " (", relinf, "%)")
  names(labels) <- mod$contributions$var
  
  terms <- mod[["gbm.call"]][["gbm.x"]]
  n_plots <- length(terms)
  data2 <- filter(data, var %in% mod$contributions$var[1:n_plots])
  
  # -----------------------------
  # 8. Set plot colors and species names based on species code
  # -----------------------------
  
  if(sp[i] == "CALBOR"){
    colLine <- rgb(0.9,0.4,0,0.9)
    colFill <- rgb(0.9,0.4,0,0.5)
    commonName <- "Cory's Shearwater"
    maxperc <- 0.15
  }
  
  if(sp[i] == "CALEDW"){
    colLine <- rgb(0.4,0.7,0.5,0.9)
    colFill <- rgb(0.4,0.7,0.5,0.5)
    commonName <- "Cape Verde Shearwater"
    maxperc <- 0.25
  }
  
  if(sp[i] == "LARAUD"){
    colLine <- rgb(0.4,0.4,1,0.9)
    colFill <- rgb(0.4,0.4,1,0.5)
    commonName <- "Audouin's gull"
    maxperc <- 0.15
  }
  
  # -----------------------------
  # 9. Plot partial effects with confidence intervals (all predictors)
  # -----------------------------
  
  p <- ggplot(data2, aes(x = xval)) +
    geom_ribbon(aes(ymin = cil, ymax = ciu), fill=colFill, alpha=.2, linetype=0) +
    geom_line(aes(y = med), color=colLine) +
    ylab("Fitted function") + xlab("") +
    facet_wrap(var~., scales = "free_x", ncol = 3, strip.position = "bottom", labeller=labeller(var=labels)) +
    theme_article(base_size = 13) +
    theme(
      strip.placement = "outside",
      plot.margin = unit(c(10,10,10,10), "points"),
      axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))
  
  outfile <- paste0(outdir_bootstrap, "/PartialEffPlotBoot.png")
  ggsave(outfile, p, width=30, height=30, units="cm", dpi=300)
  
  print("partial eff plot printed")
  
  # -----------------------------
  # 10. Prepare plots for top 4 predictors only
  # -----------------------------
  
  top4 <- names(labels)[1:4]
  data3 <- data2 %>% dplyr::filter(var %in% top4)
  
  p4 <- ggplot(data3, aes(x = xval)) +
    geom_ribbon(aes(ymin = cil, ymax = ciu), fill=colFill, alpha=.2, linetype=0) +
    geom_line(aes(y = med), color=colLine) +
    ylab("Fitted function") + xlab("") +
    facet_wrap(var~., scales = "free_x", ncol = 4, strip.position = "bottom", labeller=labeller(var=labels)) +
    theme_article(base_size = 13) +
    theme(strip.placement = "outside", axis.title.y = element_text(size = 11))
  

  # Store these plots
  plot_list[[i]] <- list(p4) 

  # -----------------------------
  # 11. Special handling for categorical variables "LIGHT" and "GEAR"
  # -----------------------------
  
  catVars <- c("LIGHT", "GEAR")
  
  if(sum(catVars %in% unique(data2$var)) == 2){
    
    # Filter data for categorical vars and remove continuous predictor values
    data2 <- data2 %>%
      dplyr::filter(var %in% catVars) %>%
      dplyr::select(-xval)
    
    # Define the categorical factor levels explicitly
    xval <- tibble::tibble(xval=c("Dawn", "Day", "Dusk", "Night",
                                  "DL", "F", "PL", "S", "T", "Unsp."))
    
    data2 <- bind_cols(data2, xval)
    
    # Plot categorical predictors with points and error bars
    p <- ggplot(data2, aes(x = xval, y = med)) +
      geom_point(colour=colFill)+
      geom_errorbar(aes(ymin=med-cil, ymax=med+ciu), width=.2,
                    position=position_dodge(0.05), colour=colFill) +
      ylab("Fitted function") + xlab("") +
      facet_wrap(var~., scales = "free_x", strip.position = "bottom", labeller=labeller(var=labels)) +
      theme_article(base_size = 13) +
      theme(
        strip.placement = "outside",
        plot.margin = unit(c(10,10,10,10), "points"),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))
      )
    
    outfile <- paste0(outdir_bootstrap, "/PartialEffPlotBoot_cat.png")
    ggsave(outfile, p, width=30, height=30, units="cm", dpi=300)
    
    print("partial eff plot printed")
    
  }
}

# -----------------------------
# 12. Combine and save top 4 partial effect plots for all species
# -----------------------------

plotlist_f <- flatten(plot_list)

margin = theme(plot.margin = unit(c(0,0,0,0), "cm"))

ggsave(
  filename = paste0(output_dir, "/partialEffPlotTop4.png"), 
  plot = gridExtra::marrangeGrob(grobs = plotlist_f, ncol = 1, nrow= 3,
                                 margin), 
  width = 8, height = 9)


#
#
#
#
#
# Clean up
tokeep <- c("output_dir", "input_dir", "cpu")
rm(list = setdiff(ls(), tokeep))
