# ==============================================================================
# Script 03: Missing Value Analysis Plot
# Purpose: Create heatmap for Data chapter section 2.2
# Author: Sowmya Yerraguntla
# Date: 2024-11-30
# ==============================================================================

library(tidyverse)
library(here)

# Load missing value data
missing_data <- read_csv(here("data/clean/missing_value_analysis.csv"))

# Create heatmap
p_missing_heatmap <- missing_data |>
  ggplot(aes(x = ref_year, y = reporter_desc, fill = pct_qty_present)) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_wrap(~data_type, ncol = 1) +
  scale_fill_gradient(
    low = "#d73027",
    high = "#4575b4",
    limits = c(0, 100),
    name = "% Records with\nQuantity Data"
  ) +
  scale_x_continuous(breaks = seq(2005, 2024, 2)) +
  labs(
    title = "Quantity Data Availability by Reporter and Year",
    subtitle = "Percentage of trade records with quantity information",
    x = "Year",
    y = "Reporter"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

# Save plot
ggsave(
  here("figures/missing_value_heatmap.png"),
  plot = p_missing_heatmap,
  width = 12,
  height = 8,
  dpi = 300
)

cat("Missing value heatmap saved to figures/missing_value_heatmap.png\n")

# Display
print(p_missing_heatmap)