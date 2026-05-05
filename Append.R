#Library packages
library(tidyverse)


# ========================================
# Paths
# ========================================

# Directory containing original extracted LFS .dta files
raw_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/1. Raw data/Extracted"

# Main output directory for processed data
out_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data"

# Directory for intermediate modified files
mod_dir <- file.path(out_dir, "modified raw data", "Modified data")

# Directory for final combined dataset
comb_dir <- file.path(out_dir, "modified raw data", "Combined data")


# ========================================
# Variable sets
# ========================================

# Final harmonised list of variables to be retained across all datasets
final_vars <- c(
  "CASENOP", "AGE", "SEX", "RELIGE", "DISEA", "CRY12",
  "SC10MMJ", "SC20MMJ",
  "ILODEFR", "GRSSWK", "POTHR", "BUSHR", "MPNR02", "PUBLICR", "EMPMON",
  "FDPCH2", "FDPCH4", "FDPCH9", "FDPCH15", "FDPCH16", "FDPCH19",
  "MARDY6", "EDAGE", "PAIDHRU", "ETHUKEUL", "STATR", "GOVTOF2",
  "HIQUL15D", "HIQUL22D",
  "year", "quarter"
)


# ========================================
# Quarter mapping
# ========================================

# Mapping LFS quarter abbreviations to numeric codes
quarters <- c(
  "jm" = "1",
  "aj" = "2",
  "js" = "3",
  "od" = "4"
)

# Initialise empty container for file names
files_list <- list()


# ==========================================================================
# Generate file names for each quarter-year combination
# ==========================================================================
# Creates expected filenames in format:
# lfsp_jm15_eul.dta ... lfsp_jm24_eul.dta etc.
# ==========================================================================

for (q in names(quarters)) {
  
  temp <- paste0(
    "lfsp_", q,                # quarter prefix (jm/aj/js/od)
    sprintf("%02d", 15:24),    # year suffix (2015–2024 as 15–24)
    "_eul.dta"                 # file extension
  )
  
  # Append generated filenames to full file list
  files_list <- c(files_list, temp)
}


# Remove known missing file to prevent read error
files_list <- files_list[files_list != "lfsp_od24_eul.dta"]


# ========================================
# Extracting / cleaning files
# ========================================
# Loop through each file, import dataset, select variables, and save as RDS

for (i in files_list) {
  
  data <- read_dta(file.path(raw_dir, i)) %>%  # read LFS Stata file
    select(any_of(final_vars)) %>%             # keep only required variables
    mutate(
      year = paste0("20", substr(i, 8, 9)),    # extract year from filename
      quarter = quarters[[q]]                  # assign quarter value
    )
  
  # Save cleaned dataset as RDS file in modified directory
  saveRDS(
    data,
    file.path(mod_dir, paste0(substr(i, 1, 13), "_mod.rds"))
  )
}


# ========================================
# Appending dataset
# ========================================

# List all cleaned RDS files
files <- list.files(
  path       = mod_dir,
  full.names = TRUE
)

# Read each RDS file and combine into single dataset
lfs_data <- lapply(files, readRDS) %>%   # read each file
  bind_rows()                             # stack rows together


# Save final combined dataset
saveRDS(
  lfs_data,
  file.path(comb_dir, "combined_lfs_all_quarters.rds")
)
