# ==============================================================================
# Script 04: Create ALL Visualizations (IMPROVED - Best Practices)
# Purpose: Generate all plots for Results chapter
# Author: Sowmya Yerraguntla
# Date: 2024-11-30
# ==============================================================================

library(tidyverse)
library(here)
library(scales)
library(zoo)

cat("\n========================================\n")
cat("CREATING ALL VISUALIZATIONS\n")
cat("With proper ordering and best practices\n")
cat("========================================\n\n")

# Load cleaned data and force numeric types
producer_clean <- read_csv(here("data/clean/producer_exports_clean.csv")) |>
  mutate(
    value_billions = as.numeric(value_billions),
    value_usd = as.numeric(value_usd),
    qty = as.numeric(qty)
  )

product_mix <- read_csv(here("data/clean/product_mix.csv")) |>
  mutate(knit_share = as.numeric(knit_share))

growth_rates <- read_csv(here("data/clean/growth_rates.csv")) |>
  mutate(
    value_billions = as.numeric(value_billions),
    yoy_growth = as.numeric(yoy_growth)
  )

market_clean <- read_csv(here("data/clean/market_imports_clean.csv")) |>
  mutate(
    value_billions = as.numeric(value_billions),
    value_usd = as.numeric(value_usd)
  )

partner_shares <- read_csv(here("data/clean/partner_shares.csv")) |>
  mutate(
    value_usd = as.numeric(value_usd),
    share = as.numeric(share)
  )

hhi_data <- read_csv(here("data/clean/hhi_concentration.csv")) |>
  mutate(
    hhi = as.numeric(hhi),
    n_partners = as.numeric(n_partners)
  )

pipeline_data <- read_csv(here("data/clean/pipeline_analysis.csv")) |>
  mutate(
    textile_imports_billions = as.numeric(textile_imports_billions),
    apparel_exports_billions = as.numeric(apparel_exports_billions),
    dependency_ratio = as.numeric(dependency_ratio)
  )

textile_imports <- read_csv(here("data/raw/textile_imports_all.csv")) |>
  mutate(primary_value = as.numeric(primary_value))


# ==============================================================================
# PLOT 1: Producer Export Trends 
# ==============================================================================

cat("Creating Plot 1: Producer export trends...\n")

# Calculate total exports for ordering
# Calculate total exports for ordering (use most recent available year per country)
country_order <- producer_clean |>
  group_by(reporter) |>
  filter(year == max(year)) |>
  summarise(total = sum(value_billions, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(reporter)

# Ensure all 4 countries are in the order
if(length(country_order) < 4) {
  country_order <- c("China", "Bangladesh", "Viet Nam", "India")
}

p1_exports <- producer_clean |>
  mutate(reporter = factor(reporter, levels = country_order)) |>
  group_by(reporter, year, hs_category) |>
  summarise(value_billions = sum(value_billions, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = year, y = value_billions, color = hs_category)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~reporter, ncol = 2, scales = "free_y") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  scale_color_manual(
    values = c("Knit" = "#2166ac", "Woven" = "#b2182b"),
    name = "Product Type"
  ) +
  labs(
    title = "Apparel Exports by Producer Country, 2005-2024",
    subtitle = "Knit (HS 61) vs Woven (HS 62) apparel, ordered by 2024 export value",
    x = "Year",
    y = "Export Value (USD)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14)
  )

ggsave(here("figures/plot1_producer_exports.png"), p1_exports, width = 12, height = 8, dpi = 300)


# ==============================================================================
# PLOT 2: Product Mix
# ==============================================================================

cat("Creating Plot 2: Product mix trends...\n")

# Calculate 2024 knit share for ordering
# Calculate most recent knit share for ordering
share_order <- product_mix |>
  group_by(reporter) |>
  filter(year == max(year)) |>
  ungroup() |>
  arrange(desc(knit_share)) |>
  pull(reporter)

# Ensure all countries present
if(length(share_order) < 4) {
  share_order <- c("Viet Nam", "China", "Bangladesh", "India")
}

p2_product_mix <- product_mix |>
  mutate(reporter = factor(reporter, levels = share_order)) |>
  ggplot(aes(x = year, y = knit_share * 100, color = reporter)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  scale_color_brewer(palette = "Set1", name = "Country") +
  labs(
    title = "Knit Apparel as Share of Total Apparel Exports",
    subtitle = "HS 61 / (HS 61 + HS 62), by producer country",
    x = "Year",
    y = "Knit Share (%)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

ggsave(here("figures/plot2_product_mix.png"), p2_product_mix, width = 10, height = 7, dpi = 300)


# ==============================================================================
# PLOT 3: Growth Rates 
# ==============================================================================

cat("Creating Plot 3: Growth rate trends...\n")

p3_growth <- growth_rates |>
  filter(!is.na(yoy_growth)) |>
  mutate(
    hs_code = as.character(hs_code),
    reporter = factor(reporter, levels = country_order)
  ) |>
  group_by(reporter, hs_code) |>
  arrange(year) |>
  mutate(growth_smooth = rollmean(yoy_growth, k = 3, fill = NA, align = "right")) |>
  ungroup() |>
  filter(!is.na(growth_smooth)) |>
  ggplot(aes(x = year, y = growth_smooth, color = hs_code)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~reporter, ncol = 2) +
  scale_x_continuous(breaks = seq(2010, 2025, 5)) +
  scale_color_manual(
    values = c("61" = "#2166ac", "62" = "#b2182b"),
    labels = c("61" = "Knit (HS 61)", "62" = "Woven (HS 62)"),
    name = "Product Type"
  ) +
  labs(
    title = "Export Growth Rates by Producer Country",
    subtitle = "3-year rolling average of year-over-year growth",
    x = "Year",
    y = "Growth Rate (%)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14)
  )

ggsave(here("figures/plot3_growth_rates.png"), p3_growth, width = 12, height = 8, dpi = 300)


# ==============================================================================
# PLOT 4: Pipeline Scatter 
# ==============================================================================

cat("Creating Plot 4: Pipeline scatter plot...\n")

p4_pipeline <- pipeline_data |>
  filter(!is.na(textile_imports_billions), !is.na(apparel_exports_billions)) |>
  mutate(reporter = factor(reporter, levels = country_order)) |>
  ggplot(aes(x = textile_imports_billions, y = apparel_exports_billions, color = reporter)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~period, ncol = 2) +
  scale_x_log10(labels = label_number(suffix = "B")) +
  scale_y_log10(labels = label_number(suffix = "B")) +
  scale_color_brewer(palette = "Set1", name = "Country") +
  labs(
    title = "Textile Inputs vs Apparel Outputs Pipeline",
    subtitle = "Log-log scale, by period",
    x = "Textile Imports (USD, billions)",
    y = "Apparel Exports (USD, billions)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14)
  )

ggsave(here("figures/plot4_pipeline_scatter.png"), p4_pipeline, width = 12, height = 7, dpi = 300)


# ==============================================================================
# PLOT 5: Import Dependency Ratio 
# ==============================================================================

cat("Creating Plot 5: Import dependency ratio...\n")

p5_dependency <- pipeline_data |>
  filter(!is.na(dependency_ratio), dependency_ratio < 5) |>
  mutate(reporter = factor(reporter, levels = country_order)) |>
  ggplot(aes(x = year, y = dependency_ratio, color = reporter)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  scale_color_brewer(palette = "Set1", name = "Country") +
  labs(
    title = "Import Dependency Ratio Over Time",
    subtitle = "Textile imports / Apparel exports (higher = more assembly-focused)",
    x = "Year",
    y = "Dependency Ratio"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

ggsave(here("figures/plot5_dependency_ratio.png"), p5_dependency, width = 10, height = 7, dpi = 300)


# ==============================================================================
# PLOT 6: Vietnam vs Bangladesh Head-to-Head 
# ==============================================================================

cat("Creating Plot 6: Vietnam vs Bangladesh comparison...\n")

vn_bd_comparison <- producer_clean |>
  filter(reporter %in% c("Viet Nam", "Bangladesh")) |>
  group_by(reporter, year) |>
  summarise(total_exports = sum(value_billions, na.rm = TRUE), .groups = "drop")

p6_vn_bd <- vn_bd_comparison |>
  ggplot(aes(x = year, y = total_exports, color = reporter, fill = reporter)) +
  geom_area(alpha = 0.3, position = "identity") +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    values = c("Bangladesh" = "#e41a1c", "Viet Nam" = "#4daf4a"),
    name = "Country"
  ) +
  scale_fill_manual(
    values = c("Bangladesh" = "#e41a1c", "Viet Nam" = "#4daf4a"),
    name = "Country"
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "Vietnam vs Bangladesh: Total Apparel Exports",
    subtitle = "Competition for second-largest exporter position",
    x = "Year",
    y = "Export Value (USD)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

ggsave(here("figures/plot6_vietnam_bangladesh_comparison.png"), p6_vn_bd, width = 12, height = 7, dpi = 300)


# ==============================================================================
# PLOT 7: Textile Imports by Type
# ==============================================================================

cat("Creating Plot 7: Textile imports by type...\n")

textile_by_type <- textile_imports |>
  mutate(
    textile_type = case_when(
      cmd_code == "52" ~ "Cotton",
      cmd_code == "54" ~ "Man-made filaments",
      cmd_code == "55" ~ "Man-made staple fibers",
      cmd_code == "60" ~ "Knitted fabrics",
      TRUE ~ "Other"
    )
  ) |>
  group_by(reporter_desc, ref_year, textile_type) |>
  summarise(value_billions = sum(primary_value, na.rm = TRUE) / 1e9, .groups = "drop")

p7_textile_types <- textile_by_type |>
  filter(reporter_desc %in% c("China", "Bangladesh", "Viet Nam", "India")) |>
  mutate(reporter_desc = factor(reporter_desc, levels = country_order)) |>
  ggplot(aes(x = ref_year, y = value_billions, fill = textile_type)) +
  geom_area(position = "stack", alpha = 0.8) +
  facet_wrap(~reporter_desc, ncol = 2, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = "Textile Type") +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "Textile Imports by Type and Producer Country",
    subtitle = "Composition of textile inputs for apparel production",
    x = "Year",
    y = "Import Value (USD)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14)
  )

ggsave(here("figures/plot7_textile_types.png"), p7_textile_types, width = 12, height = 8, dpi = 300)


# ==============================================================================
# PLOT 8: Export Volatility 
# ==============================================================================

cat("Creating Plot 8: Export volatility analysis...\n")

volatility_data <- growth_rates |>
  filter(!is.na(yoy_growth)) |>
  group_by(reporter) |>
  summarise(
    mean_growth = mean(yoy_growth, na.rm = TRUE),
    sd_growth = sd(yoy_growth, na.rm = TRUE),
    cv = sd_growth / abs(mean_growth),
    .groups = "drop"
  ) |>
  arrange(cv) |>
  mutate(reporter = fct_reorder(reporter, cv))

p8_volatility <- volatility_data |>
  ggplot(aes(x = reporter, y = cv, fill = reporter)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(cv, 2)), vjust = -0.5, size = 5) +
  scale_fill_brewer(palette = "Set1") +
  coord_flip() +
  labs(
    title = "Export Growth Volatility by Country",
    subtitle = "Coefficient of variation (higher = more volatile)",
    x = NULL,
    y = "Coefficient of Variation"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text.y = element_text(size = 14)
  )

ggsave(here("figures/plot8_export_volatility.png"), p8_volatility, width = 10, height = 7, dpi = 300)


# ==============================================================================
# PLOT 9: Export Snapshots 
# ==============================================================================

cat("Creating Plot 9: Export growth comparison...\n")

export_growth_summary <- producer_clean |>
  filter(year %in% c(2010, 2015, 2020, 2024)) |>
  group_by(reporter, year) |>
  summarise(total = sum(as.numeric(value_billions), na.rm = TRUE), .groups = "drop") |>
  mutate(reporter = factor(reporter, levels = country_order))

p9_growth_bars <- export_growth_summary |>
  ggplot(aes(x = as.factor(year), y = total, fill = reporter)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_brewer(palette = "Set1", name = "Country") +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "Apparel Export Snapshots: 2010, 2015, 2020, 2024",
    subtitle = "Total exports by producer country at key intervals",
    x = "Year",
    y = "Export Value (USD)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

ggsave(here("figures/plot9_export_snapshots.png"), p9_growth_bars, width = 12, height = 7, dpi = 300)


# ==============================================================================
# PLOT 10: Trade Balance 
# ==============================================================================

cat("Creating Plot 10: Trade balance analysis...\n")

trade_balance <- pipeline_data |>
  filter(!is.na(textile_imports_billions), !is.na(apparel_exports_billions)) |>
  mutate(
    net_balance = apparel_exports_billions - textile_imports_billions,
    reporter = factor(reporter, levels = country_order)
  )

p10_trade_balance <- trade_balance |>
  ggplot(aes(x = year, y = net_balance, color = reporter)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1", name = "Country") +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "Apparel-Textile Trade Balance by Country",
    subtitle = "Apparel exports minus textile imports (positive = net exporter)",
    x = "Year",
    y = "Net Balance (USD, billions)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

ggsave(here("figures/plot10_trade_balance.png"), p10_trade_balance, width = 12, height = 7, dpi = 300)


# ==============================================================================
# PLOT 11: China's Dominance Over Time (Stacked Area)
# ==============================================================================

cat("Creating Plot 11: China vs competitors stacked area...\n")

china_vs_rest <- producer_clean |>
  mutate(
    group = if_else(reporter == "China", "China", "Other Producers Combined")
  ) |>
  group_by(year, group) |>
  summarise(total_exports = sum(value_billions, na.rm = TRUE), .groups = "drop")

p11_china_dominance <- china_vs_rest |>
  ggplot(aes(x = year, y = total_exports, fill = group)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(
    values = c("China" = "#377eb8", "Other Producers Combined" = "#e41a1c"),
    name = ""
  ) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  scale_y_continuous(labels = label_number(suffix = "B")) +
  labs(
    title = "China vs Other Producers: Combined Apparel Exports",
    subtitle = "China alone compared to Bangladesh + Vietnam + India combined",
    x = "Year",
    y = "Total Exports (USD)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

ggsave(here("figures/plot11_china_dominance.png"), p11_china_dominance, width = 12, height = 7, dpi = 300)


# ==============================================================================
# PLOT 12: Regional Shift - Asia Producers Market Share
# ==============================================================================

cat("Creating Plot 12: Producer market share evolution...\n")

market_share_data <- producer_clean |>
  group_by(year) |>
  mutate(
    world_total = sum(value_billions, na.rm = TRUE),
    share = value_billions / world_total * 100
  ) |>
  ungroup() |>
  group_by(reporter, year) |>
  summarise(share = sum(share, na.rm = TRUE), .groups = "drop") |>
  mutate(reporter = factor(reporter, levels = rev(country_order)))

p12_market_share <- market_share_data |>
  ggplot(aes(x = year, y = share, fill = reporter)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_fill_brewer(palette = "Set1", name = "Country", direction = -1) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Market Share Evolution Among Top 4 Apparel Exporters",
    subtitle = "Relative shares of combined exports (China, Bangladesh, Vietnam, India)",
    x = "Year",
    y = "Share of Combined Exports (%)"
  ) +
  theme_minimal(16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom"
  )

ggsave(here("figures/plot12_market_share.png"), p12_market_share, width = 12, height = 7, dpi = 300)


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("ALL VISUALIZATIONS CREATED!\n")
cat("========================================\n\n")

cat("Plots saved in figures/ (following best practices):\n")
cat("  1. plot1_producer_exports.png - Ordered by 2024 export value\n")
cat("  2. plot2_product_mix.png - Ordered by 2024 knit share\n")
cat("  3. plot3_growth_rates.png - Ordered facets\n")
cat("  4. plot4_pipeline_scatter.png - Ordered legend\n")
cat("  5. plot5_dependency_ratio.png - Ordered legend\n")
cat("  6. plot6_vietnam_bangladesh_comparison.png\n")
cat("  7. plot7_textile_types.png - Ordered facets\n")
cat("  8. plot8_export_volatility.png - Ordered low to high\n")
cat("  9. plot9_export_snapshots.png - Ordered by 2024 value\n")
cat("  10. plot10_trade_balance.png - Ordered legend\n")
cat("  11. plot11_china_dominance.png\n")
cat("  12. plot12_market_share.png - Ordered stacked area\n\n")



