# ==============================================================================
# Script 02: Clean and Prepare Data for Analysis
# Purpose: Calculate metrics needed for all visualizations
# Author: Sowmya Yerraguntla
# Date: 2024-11-30
# ==============================================================================

library(tidyverse)
library(here)

cat("\n========================================\n")
cat("STARTING DATA CLEANING\n")
cat("========================================\n\n")

# ==============================================================================
# PART 1: LOAD RAW DATA
# ==============================================================================

cat("Loading raw data...\n")

producer_exports <- read_csv(here("data/raw/producer_exports_all.csv"))
market_imports <- read_csv(here("data/raw/market_imports_all.csv"))
textile_imports <- read_csv(here("data/raw/textile_imports_all.csv"))

cat(sprintf("Producer exports: %d rows\n", nrow(producer_exports)))
cat(sprintf("Market imports: %d rows\n", nrow(market_imports)))
cat(sprintf("Textile imports: %d rows\n\n", nrow(textile_imports)))


# ==============================================================================
# PART 2: CLEAN PRODUCER EXPORTS (for RQ1, RQ2, RQ3)
# ==============================================================================

cat("Cleaning producer exports...\n")

producer_clean <- producer_exports |>
  select(
    year = ref_year,
    reporter = reporter_desc,
    hs_code = cmd_code,
    value_usd = primary_value,
    qty = qty,
    qty_unit = qty_unit_abbr
  ) |>
  mutate(
    value_usd = as.numeric(value_usd),
    value_billions = as.numeric(value_usd) / 1e9,
    qty = as.numeric(qty),
    hs_category = case_when(
      hs_code == "61" ~ "Knit",
      hs_code == "62" ~ "Woven",
      TRUE ~ "Other"
    )
  ) |>
  filter(!is.na(value_usd), value_usd > 0)

cat(sprintf("Cleaned: %d rows\n\n", nrow(producer_clean)))


# ==============================================================================
# PART 3: CALCULATE PRODUCT MIX (RQ2)
# ==============================================================================

cat("Calculating product mix (knit vs woven share)...\n")

product_mix <- producer_clean |>
  group_by(reporter, year) |>
  mutate(
    total_apparel = sum(as.numeric(value_usd), na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(hs_code == "61") |>
  mutate(
    knit_share = as.numeric(value_usd) / as.numeric(total_apparel)
  ) |>
  select(reporter, year, knit_share)

cat(sprintf("Product mix: %d rows\n\n", nrow(product_mix)))


# ==============================================================================
# PART 4: CALCULATE GROWTH RATES (RQ3)
# ==============================================================================

cat("Calculating year-over-year growth rates...\n")

growth_rates <- producer_clean |>
  group_by(reporter, hs_code) |>
  arrange(year) |>
  mutate(
    value_billions = as.numeric(value_billions),
    value_lag = lag(value_billions),
    yoy_growth = (value_billions - value_lag) / value_lag * 100
  ) |>
  ungroup() |>
  select(reporter, year, hs_code, value_billions, yoy_growth)

cat(sprintf("Growth rates: %d rows\n\n", nrow(growth_rates)))


# ==============================================================================
# PART 5: CLEAN MARKET IMPORTS (for RQ4, RQ5, RQ6, RQ9)
# ==============================================================================

cat("Cleaning market imports...\n")

market_clean <- market_imports |>
  select(
    year = ref_year,
    reporter = reporter_desc,
    partner = partner_desc,
    hs_code = cmd_code,
    value_usd = primary_value
  ) |>
  filter(!is.na(value_usd), value_usd > 0, partner != "World") |>
  mutate(
    value_usd = as.numeric(value_usd),
    value_billions = as.numeric(value_usd) / 1e9
  )

cat(sprintf("Market imports cleaned: %d rows\n\n", nrow(market_clean)))


# ==============================================================================
# PART 6: CALCULATE PARTNER SHARES (RQ4)
# ==============================================================================

cat("Calculating partner shares...\n")

partner_shares <- market_clean |>
  group_by(reporter, year, hs_code) |>
  mutate(
    total_imports = sum(as.numeric(value_usd), na.rm = TRUE),
    share = as.numeric(value_usd) / as.numeric(total_imports) * 100
  ) |>
  ungroup() |>
  arrange(reporter, year, hs_code, desc(share))

# Get top partners per market
top_partners <- partner_shares |>
  group_by(reporter, hs_code) |>
  slice_max(order_by = share, n = 8) |>
  pull(partner) |>
  unique()

# Create "Other" category
partner_shares_grouped <- partner_shares |>
  mutate(
    partner_grouped = if_else(partner %in% top_partners, partner, "Other")
  ) |>
  group_by(reporter, year, hs_code, partner_grouped) |>
  summarise(
    value_usd = sum(as.numeric(value_usd), na.rm = TRUE),
    share = sum(as.numeric(share), na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("Partner shares: %d rows\n\n", nrow(partner_shares_grouped)))


# ==============================================================================
# PART 7: CALCULATE HHI CONCENTRATION (RQ9)
# ==============================================================================

cat("Calculating HHI (market concentration)...\n")

hhi_data <- partner_shares |>
  group_by(reporter, year, hs_code) |>
  summarise(
    hhi = sum((as.numeric(share) / 100)^2, na.rm = TRUE),
    n_partners = n(),
    .groups = "drop"
  )

cat(sprintf("HHI calculated: %d rows\n\n", nrow(hhi_data)))


# ==============================================================================
# PART 8: TEXTILE INPUTS TO APPAREL PIPELINE (RQ7, RQ8)
# ==============================================================================

cat("Calculating textile input to apparel output pipeline...\n")

# Aggregate textile imports by country-year
textile_agg <- textile_imports |>
  group_by(ref_year, reporter_desc) |>
  summarise(
    textile_imports_usd = sum(as.numeric(primary_value), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    textile_imports_billions = as.numeric(textile_imports_usd) / 1e9
  )

# Aggregate apparel exports by country-year
apparel_agg <- producer_clean |>
  group_by(year, reporter) |>
  summarise(
    apparel_exports_billions = sum(as.numeric(value_billions), na.rm = TRUE),
    .groups = "drop"
  )

# Join for pipeline analysis
pipeline_data <- textile_agg |>
  left_join(
    apparel_agg,
    by = c("ref_year" = "year", "reporter_desc" = "reporter")
  ) |>
  rename(year = ref_year, reporter = reporter_desc) |>
  filter(!is.na(apparel_exports_billions)) |>
  mutate(
    textile_imports_billions = as.numeric(textile_imports_billions),
    apparel_exports_billions = as.numeric(apparel_exports_billions),
    dependency_ratio = as.numeric(textile_imports_billions) / as.numeric(apparel_exports_billions),
    period = if_else(year <= 2012, "2005-2012", "2013-2024")
  )

cat(sprintf("Pipeline data: %d rows\n\n", nrow(pipeline_data)))


# ==============================================================================
# PART 9: MISSING VALUE ANALYSIS (RQ13)
# ==============================================================================

cat("Analyzing missing quantity data...\n")

missing_qty_producer <- producer_exports |>
  group_by(reporter_desc, ref_year) |>
  summarise(
    total_records = n(),
    qty_present = sum(!is.na(qty)),
    pct_qty_present = qty_present / total_records * 100,
    .groups = "drop"
  ) |>
  mutate(data_type = "Producer Exports")

missing_qty_market <- market_imports |>
  group_by(reporter_desc, ref_year) |>
  summarise(
    total_records = n(),
    qty_present = sum(!is.na(qty)),
    pct_qty_present = qty_present / total_records * 100,
    .groups = "drop"
  ) |>
  mutate(data_type = "Market Imports")

missing_qty_all <- bind_rows(missing_qty_producer, missing_qty_market)

cat(sprintf("Missing value analysis: %d rows\n\n", nrow(missing_qty_all)))


# ==============================================================================
# PART 10: SAVE CLEANED DATA
# ==============================================================================

cat("Saving cleaned datasets...\n")

write_csv(producer_clean, here("data/clean/producer_exports_clean.csv"))
write_csv(product_mix, here("data/clean/product_mix.csv"))
write_csv(growth_rates, here("data/clean/growth_rates.csv"))
write_csv(market_clean, here("data/clean/market_imports_clean.csv"))
write_csv(partner_shares_grouped, here("data/clean/partner_shares.csv"))
write_csv(hhi_data, here("data/clean/hhi_concentration.csv"))
write_csv(pipeline_data, here("data/clean/pipeline_analysis.csv"))
write_csv(missing_qty_all, here("data/clean/missing_value_analysis.csv"))

cat("\nAll cleaned data saved to data/clean/\n")

cat("\n========================================\n")
cat("DATA CLEANING COMPLETE!\n")
cat("========================================\n\n")

cat("Files created:\n")
cat("  - producer_exports_clean.csv\n")
cat("  - product_mix.csv\n")
cat("  - growth_rates.csv\n")
cat("  - market_imports_clean.csv\n")
cat("  - partner_shares.csv\n")
cat("  - hhi_concentration.csv\n")
cat("  - pipeline_analysis.csv\n")
cat("  - missing_value_analysis.csv\n\n")

cat("Ready for visualization!\n")