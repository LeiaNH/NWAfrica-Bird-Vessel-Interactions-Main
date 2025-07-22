# mainFigures.R
# Project: NWAfrica-Bird-Vessel-Interactions-Main
# 
# This script runs those scripts to generate main figures (3-6) from the study:
# "Seabird-vessel interactions in industrial fisheries of Northwest Africa:
# implications for international bycatch management" (Navarro-Herrero et al., 2025)

#---------------------------------------------------------------
# Working directory and requirements
#---------------------------------------------------------------

source("code/setup.R")  # load paths, packages, and global settings

#-----------------------------------------------
# Figure 3 Bar plot
#-----------------------------------------------

source("Figures/barPlotInteractions.R")

#-----------------------------------------------
# Figure 4 Radar plot
#-----------------------------------------------

source("Figures/radarPlot.R")

#-----------------------------------------------
# Figure 5 Partial dependence plot
#-----------------------------------------------

source("Figures/partialEffectsPlotBoot.R")

#-----------------------------------------------
# Figure 6 Sankey plot
#-----------------------------------------------

source("Figures/sankeyAttends.R")

