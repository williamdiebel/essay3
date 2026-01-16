library(data.table)
setwd("~/Dropbox/NetZero WD Shared Folder/Data")
data <- readRDS("complete_data_2022_instrument.rds")
setDT(data)

# Print all column names
cat("Total variables:", ncol(data), "\n\n")
cat("Variable names:\n")
print(sort(names(data)))

# Check for key variables mentioned by user
cat("\n\n=== Checking for specific variables ===\n")
vars_to_check <- c("cdp_sc_member", "esc_incidents", "eds", "scope_1", "ln_scope_1", 
                   "roa", "ROA", "at", "total_assets", "ln_at", "ln_total_assets",
                   "peer_cdp_share_lag", "gvkey", "year", "headquarter_country",
                   "sic", "sic_2", "industry", "firm_age", "sbti_commitment")

for (var in vars_to_check) {
  if (var %in% names(data)) {
    cat("✓", var, "EXISTS\n")
  } else {
    cat("✗", var, "NOT FOUND\n")
  }
}
