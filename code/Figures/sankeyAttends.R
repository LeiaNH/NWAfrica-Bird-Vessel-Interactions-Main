# sankeyAttends.R
# ---------------------------------------------------------------
# Figure 6 from:
# "Seabird-vessel interactions in industrial fisheries of Northwest Africa:
# implications for international bycatch management"
# Navarro-Herrero et al. (2025)
# ---------------------------------------------------------------

# --------------------------
# 1. Load interaction data
# --------------------------

data <- readRDS(paste0(input_dir, "/events.rds")) %>% 
  dplyr::filter(type == "Attendance",
                Vessel == "Fishing",
                commonName %in% c("Cory's shearwater", "Cape Verde shearwater", "Audouin's gull")) %>%
  dplyr::select(commonName, EEZ, geartype, vesselflag)

# Clean up strings: replace underscores with spaces in gear type,
# and shorten "Western Sahara" to "W Sahara" for consistency.
data$geartype <- gsub("_", " ", data$geartype)
data$EEZ <- gsub("Western Sahara", "W Sahara", data$EEZ)

# Define flag groups by continent for easier regional classification
european <- c("PRT", "ESP", "GRC", "RUS", "LTU", "GEO", "NOR", "FRA", "DEU", "NLD", "LVA", "TUR", "POL")
asian <- c("CHN", "IDN", "CUW")
african <- c("MAR", "SEN", "SOM", "MRT", "GNB", "CMR")
american <- c("BLZ")

# Add a new column 'Region flag' classifying vessel flags by continent
# Rename columns for clarity: Gear = geartype, Country = vesselflag
data <- data %>%
  mutate('Region flag' = case_when(
    vesselflag %in% european ~ "Europe",
    vesselflag %in% asian ~ "Asia",
    vesselflag %in% african ~ "Africa",
    vesselflag %in% american ~ "C America")) %>%
  rename(Gear = geartype,
         Country = vesselflag)

# Convert vessel country codes (ISO3) to full country names
data$Flag =  countrycode::countrycode(data$Country, origin = 'iso3c', 
                                      destination = 'country.name')

# ---------------------------
# 2. Prepare and plot per species
# ---------------------------

# Get unique species names and reorder them for plotting
sp <- unique(data$commonName)
sp <- c(unique(data$commonName)[2],
        unique(data$commonName)[1],
        unique(data$commonName)[3])

plot_list <- list()

# Loop through each species to create a Sankey plot
for(i in seq_along(sp)){
  # Filter data for the current species, reshape data into long format
  # suitable for Sankey plotting, then arrange by region flag.
  df <- data %>%
    dplyr::filter(commonName == sp[i]) %>%
    make_long(Gear, EEZ, Flag, 'Region flag') %>%
    arrange('Region flag')
  
  # Calculate the count of attendances for each Gear node
  gear_counts <- df %>%
    filter(x == "Gear") %>%
    dplyr::count(.data$node) %>%
    rename(gear = node, count = n)
  
  # Join counts back to data and update Gear node labels with sample size
  df <- df %>%
    left_join(gear_counts, by = c("node" = "gear")) %>%
    mutate(node = if_else(x == "Gear", 
                          paste0(node, " (N = ", count, ")"), 
                          node)) %>%
    select(-count)
  
  # Define output filename (not used inside loop here, but could be)
  pngfile <- paste0(output_dir, "/sankeyPlot.png")
  
  # Create Sankey plot using ggplot2 and ggsankey package
  p <- ggplot(df, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node, 
                      fill = factor(node), 
                      label = node)) +
    geom_sankey(flow.alpha = .6, show.legend = F, color = "gray80") +
    scale_fill_viridis_d(option="viridis") +
    geom_sankey_text(size = 4.5, color = "black", hjust = 0, 
                     position = position_nudge(x = 0.1)) +
    labs(x = NULL, y = NULL) +    
    theme_sankey(base_size = 16) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = .5),
          panel.background = element_blank(), 
          title = NULL)
  
  # Store the plot as a list element
  plot_list[[i]] <- list(p)
}

# Flatten list of lists into a single list of plots
plotlist_f <- flatten(plot_list)

# Output filename for combined Sankey plot
pngfile <- paste0(output_dir, "/sankey.png")

# Save combined plots arranged vertically as a single PNG image
ggsave(
  filename = pngfile, 
  plot = gridExtra::marrangeGrob(grobs = plotlist_f, nrow = 3, ncol = 1), 
  width = 10, height = 15
)


#
#
#
#
#
# Clean up
tokeep <- c("output_dir", "input_dir", "cpu")
rm(list = setdiff(ls(), tokeep))
