# barPlotInteractions.R
# ---------------------------------------------------------------
# Figure 3 from:
# "Seabird-vessel interactions in industrial fisheries of Northwest Africa:
# implications for international bycatch management"
# Navarro-Herrero et al. (2025)
# ---------------------------------------------------------------

# --------------------------
# 1. Load interaction data
# --------------------------

data <- readRDS(paste0(input_dir, "/events.rds"))

# --------------------------
# 2. Summarize sample sizes
# --------------------------

# Count number of interactions by species, type, and vessel category
d <- data %>%
  group_by(commonName, type, Vessel) %>%
  summarise(n = n(), .groups = "drop")

# Prepare sample size labels for plot annotations
temp <- d %>%
  group_by(commonName, type) %>%
  summarise(
    n = sum(n),
    n = paste0("N = ", scales::comma(n)),
    y = 0.01,           # vertical position for annotation text
    .groups = "drop"
  )

# --------------------------
# 3. Prepare plotting data
# --------------------------

# Set seabird species order for plotting (alphabetically reversed for readability)
d$commonName <- factor(d$commonName, levels = rev(sort(unique(d$commonName))))

# Ensure correct stacking order: Nonfishing first, then Fishing
d$Vessel <- factor(d$Vessel, levels = c("Nonfishing", "Fishing"))

# Define color palette
vessel_colors <- c(
  "Nonfishing" = "#336699",  # Dark blue
  "Fishing"    = "#66cc66"   # Soft green
)

# --------------------------
# 4. Generate and save plot
# --------------------------

pngfile <- paste0(output_dir, "/BarPlot.png")
png(pngfile, width = 1300, height = 600, res = 200)

ggplot(d, aes(x = commonName)) +
  geom_bar(
    aes(y = n, fill = Vessel, colour = Vessel),
    position = "fill", stat = "identity"
  ) +
  scale_fill_manual(values = vessel_colors) +
  scale_color_manual(values = vessel_colors) +
  coord_flip() +
  facet_wrap(~factor(type, levels = c("Encounter", "Attendance"))) +
  geom_text(
    data = temp,
    aes(x = commonName, y = y, label = n),
    hjust = 0,
    colour = "black",
    size = 3
  ) +
  xlab("") +
  ylab("Proportion of seabird-vessel interaction types") +
  scale_y_continuous(labels = scales::percent_format()) +
  guides(
    fill = guide_legend(nrow = 1),
    colour = "none"
  ) +
  egg::theme_article() +
  theme(
    axis.title.y    = element_text(size = 16),
    axis.text.y     = element_text(size = 9, face = "italic"),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    legend.title    = element_blank(),
    strip.text      = element_text(size = 10)
  )

dev.off()

#
#
#
#
#
# Clean up
tokeep <- c("output_dir", "input_dir", "cpu")
rm(list = setdiff(ls(), tokeep))
