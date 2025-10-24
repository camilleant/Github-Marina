# First, fix the income ordering
income_order <- c("$0-$24,999", "$25,000-$49,999", "$50,000-$74,999", 
                  "$75,000-$99,999", "$100,000-$124,999", "$125,000-$149,999",
                  "$150,000-$174,999", "$175,000-$199,999", "$200,000 and up")

park24$Income <- factor(park24$Income, levels = income_order)

# ===== 1. COUNTS TABLE =====
cat("\n=== COUNTS: Parking Lot by Income ===\n")
counts <- addmargins(table(park24$Lot, park24$Income))
print(counts)

# ===== 2. ROW PERCENTAGES =====
# Shows: Of people parking at each lot, what % are in each income bracket?
cat("\n=== ROW %: Income Distribution Within Each Lot ===\n")
row_pct <- prop.table(table(park24$Lot, park24$Income), margin = 1) * 100
row_pct_rounded <- round(row_pct, 1)
print(row_pct_rounded)

# ===== 3. COLUMN PERCENTAGES =====
# Shows: Of people in each income bracket, what % park at each lot?
cat("\n=== COLUMN %: Lot Choice Within Each Income Group ===\n")
col_pct <- prop.table(table(park24$Lot, park24$Income), margin = 2) * 100
col_pct_rounded <- round(col_pct, 1)
print(col_pct_rounded)

# ===== BONUS: Pretty output with knitr =====
library(knitr)

cat("\n=== FORMATTED TABLES ===\n\n")

kable(counts, caption = "TABLE 1: Counts with Totals")

kable(row_pct_rounded, caption = "TABLE 2: Row Percentages (% within each lot)")

kable(col_pct_rounded, caption = "TABLE 3: Column Percentages (% within each income group)")