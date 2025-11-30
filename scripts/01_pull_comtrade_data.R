# ==============================================================================
# Script 01: Pull UN Comtrade Data for Fast Fashion Project
# Purpose: Download apparel and textile trade data (2005-2024)
# Author: Sowmya Yerraguntla
# Date: 2024-11-30
# ==============================================================================

# Load packages
library(comtradr)
library(tidyverse)
library(here)

cat("\n========================================\n")
cat("STARTING DATA PULL\n")
cat("API limit: max 12 years per query\n")
cat("This will take 45-60 minutes\n")
cat("========================================\n\n")

# ==============================================================================
# PART 1: PRODUCER EXPORTS (China, Bangladesh, Vietnam, India)
# ==============================================================================

# Define producers
producers <- c("CHN", "BGD", "VNM", "IND")
producer_names <- c("China", "Bangladesh", "Vietnam", "India")

# HS codes for apparel
apparel_hs <- c("61", "62")

# Time periods (split into 12-year chunks)
periods <- list(
  list(start = 2005, end = 2016),
  list(start = 2017, end = 2024)
)

cat("PART 1: Pulling producer exports...\n\n")

# Initialize list
producer_exports_list <- list()
counter <- 1

# Loop through producers, HS codes, and periods
for (i in seq_along(producers)) {
  for (hs in apparel_hs) {
    for (p in seq_along(periods)) {
      
      cat(sprintf("Pulling %s HS %s (%d-%d)...\n", 
                  producer_names[i], hs, 
                  periods[[p]]$start, periods[[p]]$end))
      
      tryCatch({
        # Pull data
        temp_data <- ct_get_data(
          commodity_code = hs,
          reporter = producers[i],
          flow_direction = "export",
          start_date = periods[[p]]$start,
          end_date = periods[[p]]$end,
          frequency = "A"
        )
        
        # Store
        producer_exports_list[[counter]] <- temp_data
        counter <- counter + 1
        
        cat(sprintf("  ✓ Retrieved %d rows\n", nrow(temp_data)))
        
      }, error = function(e) {
        cat(sprintf("  ✗ Error: %s\n", e$message))
      })
      
      # Pause to respect API rate limits
      Sys.sleep(3)
    }
  }
}

# Combine all producer exports
producer_exports_all <- bind_rows(producer_exports_list)

# Save
write_csv(producer_exports_all, here("data/raw/producer_exports_all.csv"))
cat(sprintf("\n PART 1 COMPLETE: %d rows saved\n\n", nrow(producer_exports_all)))


# ==============================================================================
# PART 2: MARKET IMPORTS (USA, EU) by Partner
# ==============================================================================

cat("PART 2: Pulling US and EU imports by partner...\n\n")

# Define markets
markets <- c("USA")  # Start with USA only
market_imports_list <- list()
counter <- 1

# Loop through markets, HS codes, and periods
for (market in markets) {
  for (hs in apparel_hs) {
    for (p in seq_along(periods)) {
      
      cat(sprintf("Pulling %s HS %s imports (%d-%d)...\n", 
                  market, hs, 
                  periods[[p]]$start, periods[[p]]$end))
      
      tryCatch({
        temp_data <- ct_get_data(
          commodity_code = hs,
          reporter = market,
          flow_direction = "import",
          start_date = periods[[p]]$start,
          end_date = periods[[p]]$end,
          frequency = "A"
        )
        
        market_imports_list[[counter]] <- temp_data
        counter <- counter + 1
        
        cat(sprintf("  ✓ Retrieved %d rows\n", nrow(temp_data)))
        
      }, error = function(e) {
        cat(sprintf("  ✗ Error: %s\n", e$message))
      })
      
      Sys.sleep(3)
    }
  }
}

# Try EU with different codes
cat("\nTrying EU imports...\n")
eu_codes <- c("EUN", "EU27")

for (eu_code in eu_codes) {
  cat(sprintf("\nTrying EU code: %s\n", eu_code))
  
  for (hs in apparel_hs) {
    for (p in seq_along(periods)) {
      
      tryCatch({
        temp_data <- ct_get_data(
          commodity_code = hs,
          reporter = eu_code,
          flow_direction = "import",
          start_date = periods[[p]]$start,
          end_date = periods[[p]]$end,
          frequency = "A"
        )
        
        market_imports_list[[counter]] <- temp_data
        counter <- counter + 1
        
        cat(sprintf("  ✓ %s HS %s (%d-%d): %d rows\n", 
                    eu_code, hs, 
                    periods[[p]]$start, periods[[p]]$end,
                    nrow(temp_data)))
        
      }, error = function(e) {
        cat(sprintf("  ✗ Failed\n"))
      })
      
      Sys.sleep(3)
    }
  }
}

# Combine all market imports
market_imports_all <- bind_rows(market_imports_list)

# Save
write_csv(market_imports_all, here("data/raw/market_imports_all.csv"))
cat(sprintf("\n PART 2 COMPLETE: %d rows saved\n\n", nrow(market_imports_all)))


# ==============================================================================
# PART 3: TEXTILE INPUTS (HS 52, 54, 55, 60)
# ==============================================================================

cat("PART 3: Pulling textile inputs for producers...\n\n")

# Textile HS codes (sample key ones)
textile_hs <- c("52", "54", "55", "60")

textile_imports_list <- list()
counter <- 1

# Loop through producers, textile codes, and periods
for (i in seq_along(producers)) {
  for (hs in textile_hs) {
    for (p in seq_along(periods)) {
      
      cat(sprintf("Pulling %s textile HS %s (%d-%d)...\n", 
                  producer_names[i], hs,
                  periods[[p]]$start, periods[[p]]$end))
      
      tryCatch({
        temp_data <- ct_get_data(
          commodity_code = hs,
          reporter = producers[i],
          flow_direction = "import",
          start_date = periods[[p]]$start,
          end_date = periods[[p]]$end,
          frequency = "A"
        )
        
        textile_imports_list[[counter]] <- temp_data
        counter <- counter + 1
        
        cat(sprintf("  ✓ Retrieved %d rows\n", nrow(temp_data)))
        
      }, error = function(e) {
        cat(sprintf("  ✗ Error: %s\n", e$message))
      })
      
      Sys.sleep(3)
    }
  }
}

# Combine all textile imports
textile_imports_all <- bind_rows(textile_imports_list)

# Save
write_csv(textile_imports_all, here("data/raw/textile_imports_all.csv"))
cat(sprintf("\n PART 3 COMPLETE: %d rows saved\n\n", nrow(textile_imports_all)))


# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n========================================\n")
cat("DATA PULL COMPLETE!\n")
cat("========================================\n\n")

cat("Files saved in data/raw/:\n")
cat("  - producer_exports_all.csv\n")
cat("  - market_imports_all.csv\n")
cat("  - textile_imports_all.csv\n\n")

cat("Summary:\n")
cat(sprintf("  Producer exports: %d rows\n", nrow(producer_exports_all)))
cat(sprintf("  Market imports: %d rows\n", nrow(market_imports_all)))
cat(sprintf("  Textile imports: %d rows\n", nrow(textile_imports_all)))

cat("\n Ready for data cleaning!\n")