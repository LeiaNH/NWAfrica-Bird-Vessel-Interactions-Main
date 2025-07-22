# radarPlot.R
# ---------------------------------------------------------------
# Figure 4 from:
# "Seabird-vessel interactions in industrial fisheries of Northwest Africa:
# implications for international bycatch management"
# Navarro-Herrero et al. (2025)
# ---------------------------------------------------------------

# -----------------------------
# 1. Identify species folders
# -----------------------------

folders <- list.dirs(paste0(output_dir), recursive = FALSE)
sp <- basename(folders)  # Species codes (e.g., "CALBOR", "CALEDW")

# -----------------------------
# 2. Collect all model variables
# -----------------------------

# Extract all variable names across models to ensure a complete variable set
rad_list <- list()

for (i in seq_along(sp)) {
  outdir <- paste0(output_dir, "/", sp[i])
  mod_full <- readRDS(paste0(outdir, "/brt.rds"))
  
  var_imp <- summary(mod_full)$rel.inf
  ImpData <- as.data.frame(t(var_imp))
  colnames(ImpData) <- summary(mod_full)$var
  rad_list[[i]] <- ImpData
}

# Merge all variable importance dataframes (filling missing columns with NA)
rad <- do.call(plyr::rbind.fill, rad_list)

# -----------------------------
# 3. Define radar chart scale
# -----------------------------

# Determine maximum value across all variables for chart axis scaling
max_value <- ceiling(max(unlist(rad), na.rm = TRUE)) + 2

# -----------------------------
# 4. Loop over species and plot
# -----------------------------

for (i in seq_along(sp)) {
  cat("Processing:", sp[i], "\n")
  
  # Load model for current species
  outdir <- paste0(output_dir, "/", sp[i])
  mod_full <- readRDS(paste0(outdir, "/brt.rds"))
  
  var_imp <- summary(mod_full)$rel.inf
  ImpData <- as.data.frame(t(var_imp))
  colnames(ImpData) <- summary(mod_full)$var
  
  # Add missing variables with zero importance
  missing_vars <- setdiff(names(rad), names(ImpData))
  for (v in missing_vars) {
    ImpData[[v]] <- 0
  }
  
  # Reorder and select consistent variable set for plotting
  ImpData <- ImpData %>%
    dplyr::select(
      LIGHT, GEAR, VESSELS, LENGTH, COASTD, CAND,
      BAT, SLP, SMTD, SST, WINDS, SSH, SAL, FSLE,
      EKE, MLD, CHL
    )
  
  # Prepare data for radar chart
  radar_data <- rbind(
    rep(max_value, ncol(ImpData)),  # Max values (for chart scale)
    rep(0, ncol(ImpData)),          # Min values
    ImpData[1, , drop = FALSE]      # Actual values
  )
  
  # -----------------------------
  # 5. Define species-specific styles
  # -----------------------------
  
  if (sp[i] == "CALBOR") {
    colLine <- rgb(0.9, 0.4, 0, 0.9)
    colFill <- rgb(0.9, 0.4, 0, 0.5)
    commonName <- "Cory's Shearwater"
    
  } else if (sp[i] == "CALEDW") {
    colLine <- rgb(0.4, 0.7, 0.5, 0.9)
    colFill <- rgb(0.4, 0.7, 0.5, 0.5)
    commonName <- "Cape Verde Shearwater"
    
  } else if (sp[i] == "LARAUD") {
    colLine <- rgb(0.4, 0.4, 1, 0.9)
    colFill <- rgb(0.4, 0.4, 1, 0.5)
    commonName <- "Audouin's Gull"
  }
  
  # -----------------------------
  # 6. Generate and save radar plot
  # -----------------------------
  
  pngfile <- paste0(output_dir, "/", sp[i], "/radarPlot.png")
  png(pngfile, width = 1500, height = 1500, res = 200)
  
  fmsb::radarchart(
    radar_data,
    axistype     = 1,
    pcol         = colLine,
    pfcol        = colFill,
    plwd         = 1,
    cglcol       = "grey",
    cglty        = 1,
    cglwd        = 0.8,
    axislabcol   = "grey40",
    caxislabels  = seq(0, max_value, 8),
    calcex       = 1.2,
    vlcex        = 1.5,
    cex.main     = 2,
    title        = substitute(italic(x), list(x = commonName))
  )
  
  dev.off()
}

#
#
#
#
#
# Clean up
tokeep <- c("output_dir", "input_dir", "cpu")
rm(list = setdiff(ls(), tokeep))
