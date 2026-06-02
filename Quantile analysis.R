# ======================================================
# Quantile analysis
# Purpose: Estimate wage gaps and decompose them into
# explained and unexplained components across ethnic groups across quantiles
# ======================================================

library(dineq)
library(tidyverse)
library(haven)
library(writexl)
library(data.table)
library(oaxaca)
library(ddecompose)
# ====================
# 2. Paths and data
# ====================

clean_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data/Cleaned data"
output_charts <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/Charts"
reg_output <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/regression results"
counterfactual_output <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/Counterfactual output"

lfs_data <- readRDS(file.path(clean_dir, "LFS_data_IMR"))
lfs_data <- as.data.table(lfs_data)

# ======================================================
# 4. Model setup
# ======================================================

# Define list of control variables used in all core regressions
vars <- c(
  "exper", "exper2", "female",  "bborn", "disabled",
  "tenure", "london", "public",
  "degree", "higher", "alevel", "gcse", "other",
  "manager", "professional", "associate", "administrative",
  "skilled", "caring", "sales", "elementary", "lcomp"
)

# Convert vector of variables into regression formula string
regressors <- paste(vars, collapse = " + ")

# Convert vector of variables into regression formula string(Oaxaca blinder decomp)
regressors_oaxaca <- paste(c(vars, "IMR"), collapse = " + ")

# Define reference ethnic groups for pairwise comparisons
reference_groups <- c("white", "indian", "chinese")

# Define all ethnic groups in dataset
ethnic_groups <- c(
  "white", "black", "indian", "pakistani",
  "bangladeshi", "chinese", "mixed"
)

# Initialise list to store regression and Oaxaca outputs
Model <- list()

#----------------------------------------------------------------
# C. RIF decomposiiton
#----------------------------------------------------------------
# Helper function: Recentered Influence Function for a given quantile
make_rif <- function(y, tau) {
  
  # Remove missing observations for estimating quantile and density
  y_nonmiss <- y[!is.na(y)]
  
  # Estimate unconditional quantile
  q_tau <- as.numeric(quantile(y_nonmiss, tau, na.rm = TRUE, names = FALSE))
  
  # Estimate density once
  dens <- density(y_nonmiss, bw = "SJ")
  
  # Approximate density at the quantile
  f_q <- approx(
    x = dens$x,
    y = dens$y,
    xout = q_tau,
    rule = 2
  )$y
  
  # Avoid division by very small density
  f_q <- max(f_q, 1e-4)
  
  # Return RIF, preserving missing values
  rif <- q_tau + (tau - as.numeric(y <= q_tau)) / f_q
  rif[is.na(y)] <- NA_real_
  
  return(rif)
}

# Quantiles for decomposition
quantiles <- c( 0.25, 0.5, 0.75, 0.9)

# Summary table
rif_summary <- data.frame()
# Loop over reference groups
for (Rgroup in reference_groups) {
  
  # Loop over comparison groups
  for (group in ethnic_groups) {
    
    if (group != Rgroup) {
      
      # Subset to reference and comparison group
      df_sub <- as.data.frame(
        lfs_data[lfs_data[[Rgroup]] == 1 | lfs_data[[group]] == 1, ]
      )
      
      # Loop over quantiles
      for (q in quantiles) {
        
        q_name <- paste0("q", q * 100)
        rif_var <- paste0("rif_", q_name)
        
        # ======================================================
        # Compute group-specific RIF outcome
        # ======================================================
        
        # Initialise RIF variable
        df_sub[[rif_var]] <- NA_real_
        
        # Compute RIF using reference-group quantile and density
        df_sub[df_sub[[Rgroup]] == 1, rif_var] <-
          make_rif(
            df_sub[df_sub[[Rgroup]] == 1, "loghrp"],
            q
          )
        
        # Compute RIF using comparison-group quantile and density
        df_sub[df_sub[[group]] == 1, rif_var] <-
          make_rif(
            df_sub[df_sub[[group]] == 1, "loghrp"],
            q
          )
        
        # ======================================================
        # Run Oaxaca decomposition on RIF outcome
        # ======================================================
        
        rif_result <- oaxaca(
          formula = as.formula(
            paste(
              rif_var,
              "~",
              regressors_oaxaca,
              "+ factor(year) |",
              group
            )
          ),
          data = df_sub,
          group.weights = 0,
          R = NULL
        )
        
        # Extract row where group.weight == 0
        # (reference-group coefficients used as weighting structure)
        overall <- as.data.frame(rif_result$twofold$overall)
        
        overall <- overall[
          as.numeric(overall$group.weight) == 0,
        ]
        
        # Extract decomposition components as numeric values
        explained <- as.numeric(overall[1, "coef(explained)"])
        unexplained <- as.numeric(overall[1, "coef(unexplained)"])
        gap <- explained + unexplained
        
        # ======================================================
        # Calculate actual wage quantiles for interpretation
        # ======================================================
        
        q_ref <- quantile(
          df_sub[df_sub[[Rgroup]] == 1, "loghrp"],
          q,
          na.rm = TRUE
        )
        
        q_comp <- quantile(
          df_sub[df_sub[[group]] == 1, "loghrp"],
          q,
          na.rm = TRUE
        )
        
        # ======================================================
        # Store summary results only
        # ======================================================
        
        rif_summary <- rbind(
          rif_summary,
          data.frame(
            reference_group = Rgroup,
            comparison_group = group,
            quantile = q,
            
            # Actual quantile values
            quantile_log_wage_reference = q_ref,
            quantile_log_wage_comparison = q_comp,
            raw_quantile_gap = q_ref - q_comp,
            
            # RIF-Oaxaca decomposition
            rif_gap = gap,
            explained = explained,
            unexplained = unexplained,
            
            # Shares of total gap
            explained_share = explained / gap,
            
            unexplained_share = unexplained / gap,
            
            N = rif_result$n$n.pooled
          )
        )
      }
      
      # Remove temporary objects
      rm(df_sub, rif_result,q_ref,q_comp,overall,explained,unexplained)
      gc()
    }
  }
}
# Export summary table
write_xlsx(rif_summary, file.path(reg_output, "RIF_quantile_summary.xlsx"))
