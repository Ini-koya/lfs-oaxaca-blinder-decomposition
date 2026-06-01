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
# ======================================================
# D. RIF-Oaxaca Quantile Decomposition
# ======================================================
hrp <- exp(lfs_data$loghrp)

cutoffs <- quantile(hrp, probs = seq(0.1, 1, 0.1), na.rm = TRUE)

data.frame(
  decile = 1:10,
  cutoff_value = cutoffs,
  count_at_or_below = sapply(cutoffs, \(x) sum(hrp <= x, na.rm = TRUE))
)

#----------------------------------------------------------------
# C. RIF Pairwise regressions
#----------------------------------------------------------------
# Helper function: RIF for quantile
make_rif <- function(y, tau) {
  q <- quantile(y, tau, na.rm = TRUE)

  dens <- density(y, na.rm = TRUE, bw = "SJ")
  f_q <- approx(dens$x, dens$y, xout = q)$y
  f_q <- max(f_q, 1e-4)

  q + (tau - as.numeric(y <= q)) / f_q
}

quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

Model <- list()

for (Rgroup in reference_groups) {
  Model[[Rgroup]] <- list()

  for (group in ethnic_groups) {
    if (group != Rgroup) {
      pair_name <- paste(Rgroup, group, sep = "_vs_")
      Model[[Rgroup]][[pair_name]] <- list()

      df_sub <- as.data.frame(
        lfs_data[lfs_data[[Rgroup]] == 1 | lfs_data[[group]] == 1, ]
      )

      for (q in quantiles) {
        q_name <- paste0("q", q * 100)
        rif_var <- paste0("rif_", q_name)

        df_sub[[rif_var]] <- make_rif(df_sub$loghrp, q)

        pair_model <- lm(
          as.formula(
            paste(rif_var, "~", group, "+", regressors_oaxaca, "+ factor(year)")
          ),
          data = df_sub
        )

        Model[[Rgroup]][[pair_name]][[q_name]] <- pair_model

        # remove temporary RIF variable
        df_sub[[rif_var]] <- NULL

        rm(pair_model)
        gc(FALSE)
      }

      rm(df_sub)
      gc()
    }
  }
}

#----------------------------------------------------------------
# C. RIF decomposiiton
#----------------------------------------------------------------
# Helper function: Recentered Influence Function for a given quantile
make_rif <- function(y, tau) {
  q <- quantile(y, tau, na.rm = TRUE)
  f_q <- approx(density(y, na.rm = TRUE, bw = "SJ")$x, # SJ bandwidth more robust
    density(y, na.rm = TRUE, bw = "SJ")$y,
    xout = q
  )$y
  f_q <- max(f_q, 1e-4) # floor to prevent division by near-zero
  q + (tau - as.numeric(y <= q)) / f_q
}

# Quantiles for decomposition
quantiles <- c(0.1, 0.25, 0.5, 0.75, 0.9)

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

        # Compute RIF outcome
        df_sub[[rif_var]] <- make_rif(df_sub$loghrp, q)

        # Run Oaxaca decomposition
        rif_result <- oaxaca(
          formula = as.formula(paste(
            rif_var, "~", regressors_oaxaca, "+ factor(year) |", group
          )),
          data = df_sub,
          R = NULL
        )

        # Extract reference-group coefficient results
        overall <- rif_result$twofold$overall[2, ]
        explained <- overall["coef(explained)"]
        unexplained <- overall["coef(unexplained)"]
        gap <- explained + unexplained

        # Store summary results only
        rif_summary <- rbind(rif_summary, data.frame(
          reference_group = Rgroup,
          comparison_group = group,
          quantile = q,
          mean_log_wage_reference = rif_result$y$y.A,
          mean_log_wage_comparison = rif_result$y$y.B,
          rif_gap = gap,
          explained = explained,
          unexplained = unexplained,
          explained_share = explained / gap,
          unexplained_share = unexplained / gap,
          N = rif_result$n$n.pooled
        ))
      }

      # Remove temporary objects
      rm(df_sub, rif_result)
      gc()
    }
  }
}

# Export summary table
write_xlsx(rif_summary, file.path(reg_output, "RIF_quantile_summary.xlsx"))
