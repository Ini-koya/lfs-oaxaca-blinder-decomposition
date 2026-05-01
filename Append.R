#library(haven)

#library(dplyr)

#library(purrr)

#library(fs)



# ========================================

# Paths

# ========================================

raw_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/1. Raw data/Extracted"


out_dir  <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data"

mod_dir  <- file.path(out_dir, "modified raw data", "Modified data")

comb_dir <- file.path(out_dir, "modified raw data", "Combined data")




# ========================================

# Variable sets

# ========================================



# Final harmonised variable order

final_vars <- c(
  
  "CASENOP", "AGE", "SEX", "RELIGE", "DISEA", "CRY12",
  
  "SC10MMJ", "SC20MMJ",
  
  "ILODEFR", "GRSSWK", "POTHR", "BUSHR", "MPNR02", "PUBLICR", "EMPMON",
  
  "FDPCH2", "FDPCH4", "FDPCH9", "FDPCH15", "FDPCH16", "FDPCH19",
  
  "MARDY", "EDAGE", "PAIDHRU", "ETHUKEUL", "STATR", "GOVTOF",
  
  "HIQUL15D", "HIQUL22D",
  
  "year", "quarter"
  
)


# ========================================
# Modified data 
# ========================================

# Quarter mapping
quarters <- c("jm" = "1",
              "aj" = "2",
              "js" = "3",
              "od" = "4")

files_list <- list() # Empty list container (not strictly used here), used to store file lists

# ========================================
# Loop over each quarter (jm, aj, js, od), 
# Generates file names for years 2015-2014 e.g.lfsp_aj17_eul
# These file names will be used to load and clean the quarterly LFS datasets.
# ========================================
for (q in names(quarters)) { 

  files_list <- paste0(
    "lfsp_", q,  # combines the fixed naming prefix with the quarter identifier
    sprintf("%02d", 15:24),# generates the year suffix (15–24) in two-digit format
    "_eul.dta"
  ) 
  
for (i in files_list) { 
  
  data <- read_dta(file.path(raw_dir,i))  %>%   # reads the LFS files in the loop
  select(any_of(final_vars)) %>% # selects the required variables and keeps them
  mutate(
  year=paste0("20",substr(i,8,9)), # extract year from file name variable 
  quarter= quarters[[q]]# generates quarter variable 
  ) 
  write.csv( # save data as csv in modified output directory
  data,
  file.path(mod_dir,paste0(substr(i,1,13),"_mod.csv")),
  row.names = FALSE) 
  }
                                                  
                                       
 
}

# ========================================
# Appending dataset
# ========================================

files <- list.files(
  path = mod_dir ,
  full.names = TRUE
)

lfs_data<-lapply(files,read.csv)  %>%   # read each file
  bind_rows()     # append them together


write.csv( # save combined data
  lfs_data,
  file.path(comb_dir,"combined_lfs_all_quarters.csv"),
  row.names = FALSE)
