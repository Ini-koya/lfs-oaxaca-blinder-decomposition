# ======================================================
# Oaxaca analysis
# Purpose: Estimate wage gaps and decompose them into
# explained and unexplained components across ethnic groups
# ======================================================


# --------------------
# 1. Libraries
# --------------------
# Load required packages for data manipulation, regression,
# Oaxaca decomposition, and output formatting

library(tidyverse)
library(labelled)
library(haven)
library(writexl)
library(tidyr)
library(RColorBrewer)

library(stats)
library(modelsummary)
library(oaxaca)
library(dreamerr)
library(fixest)


# --------------------
# 2. Paths and data
# --------------------
# Define file path and load cleaned Labour Force Survey data

clean_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data/Cleaned data"

lfs_data <- readRDS(file.path(clean_dir, "Clean.RDS"))

output_charts <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/Charts"


# --------------------
# 3. Further cleaning
# --------------------
# Create squared experience term for non-linear returns to experience

lfs_data <- lfs_data %>%
  mutate(exper2 = exper^2)


# ======================================================
# 4. Model setup
# ======================================================

# Define list of control variables used in all regressions
vars <- c(
  "exper", "exper2", "female", "marr", "bborn", "disabled",
  "tenure", "dep19", "London", "public",
  "degree", "higher", "alevel", "gcse", "other",
  "manager", "professional", "associate", "administrative",
  "skilled", "caring", "sales", "elementary", "lcomp"
)

# Convert vector of variables into regression formula string
regressors <- paste(vars, collapse = " + ")

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
# 5. Pairwise regressions and Oaxaca decomposition
# ======================================================

# Loop over each reference group
for (Rgroup in reference_groups) {
  
  # Create sub-list for each reference group
  Model[[Rgroup]] <- list()
  
  # Loop over comparison groups
  for (group in ethnic_groups) {
    
    # Skip comparison of group with itself
    if (group != Rgroup) {
      
      # Subset data to include only the two groups of interest
      df_sub <- lfs_data[
        lfs_data[[Rgroup]] == 1 | lfs_data[[group]] == 1,
      ]
      
      # Run OLS regression with year fixed effects
      pair_model <- feols(
        as.formula(paste("loghrp ~", regressors, "| year")),
        data = df_sub,
        data.save = TRUE
      )
      
      # Run Oaxaca-Blinder decomposition
      # Outcome: log hourly pay
      # Group variable: current comparison group
      oaxaca_result <- oaxaca(
        as.formula(paste("loghrp ~", regressors, "|", group)),
        data = df_sub,
        R = NULL
      )
      
      # Store both regression and Oaxaca results
      Model[[Rgroup]][[group]] <- list(
        regression = pair_model,
        oaxaca = oaxaca_result
      )
    }
  }
}

# Clean up temporary objects(optional)
rm(df_sub, model, oaxaca_result)


# ======================================================
# 6. Identify key contributors to wage gaps
# ======================================================

# --------------------------------------------
# 6a. Extracting variable level Oaxaca results
# --------------------------------------------

# Initialise empty dataframe to store variable-level contributions
Contributors <- data.frame()

# Loop through stored Oaxaca results to extract variable-level contributions
for (Rgroup in names(Model)) {
  
  for (group in names(Model[[Rgroup]])) {
    
    # Extract variable-level Oaxaca results
    # Uses pooled weighting scheme: group weight = 0.5
    var_coeff <- Model[[Rgroup]][[group]][["oaxaca"]][["twofold"]][["variables"]][[3]]
    
    # Convert matrix output into dataframe
    var_coeff <- as.data.frame(var_coeff)
    var_coeff$Variable <- rownames(var_coeff)
    
    # Extract explained contribution and absolute importance
    var_coeff$Explained_contribution <- as.numeric(var_coeff[, "coef(explained)"])
    var_coeff$Explained_importance <- abs(var_coeff$Explained_contribution)
    
    # Extract unexplained contribution and absolute importance
    var_coeff$Unexplained_contribution <- as.numeric(var_coeff[, "coef(unexplained)"])
    var_coeff$Unexplained_importance <- abs(var_coeff$Unexplained_contribution)
    
    # Store results for this pairwise comparison
    temp <- data.frame(
      Reference_group = rep(Rgroup, nrow(var_coeff)),
      Comparison_group = rep(group, nrow(var_coeff)),
      Variable = var_coeff$Variable,
      Explained_contribution = var_coeff$Explained_contribution,
      Explained_importance = var_coeff$Explained_importance,
      Unexplained_contribution = var_coeff$Unexplained_contribution,
      Unexplained_importance = var_coeff$Unexplained_importance
    )
    
    # Append comparison results to main contributors dataframe
    Contributors <- rbind(Contributors, temp)
  }
}


# -----------------------------------------------------
# 6b. Rank contributors within each pairwise comparison
# -----------------------------------------------------

# Create a unique identifier for each pairwise comparison
Contributors$Reference_Comparison_group <- paste0(
  Contributors$Reference_group,
  "-",
  Contributors$Comparison_group
)

# Rank variables by explained importance within each comparison (descending order)
Contributors <- Contributors %>%
  group_by(Reference_Comparison_group) %>%
  arrange(desc(Explained_importance), .by_group = TRUE) %>%
  mutate(rank_explained = row_number())

# Rank variables by unexplained importance within each comparison (descending order)
Contributors <- Contributors %>%
  group_by(Reference_Comparison_group) %>%
  arrange(desc(Unexplained_importance), .by_group = TRUE) %>%
  mutate(rank_unexplained = row_number())


# ------------------------------------------------------------
# 6c. Average variable importance across regressions + results
# ------------------------------------------------------------

# Collapse to one row per variable
# Lower mean rank = more consistently important variable
Mean_rank <- Contributors %>%
  group_by(Variable) %>%
  summarise(
    Mean_rank_explained = mean(rank_explained, na.rm = TRUE),
    Mean_rank_unexplained = mean(rank_unexplained, na.rm = TRUE),
    .groups = "drop"
  ) 


# Results: Explained contributors
Mean_rank %>%
  arrange(Mean_rank_explained) %>%
  head(10)

# Results: Unexplained contributors 
Mean_rank %>%
  arrange(Mean_rank_unexplained) %>%
  head(10)

# Clean up temporary objects(optional)
# rm(Contributors,Contributors_explained,Contributors_unexplained,pair_model,temp,var_coeff)

# ======================================================
# 8. Model validity checks
# Purpose: Assess robustness of the regression specification
# using endogeneity sensitivity checks and residual diagnostics
# ======================================================


# Loop over stored models and generate diagnostic plots
for (Rgroup in names(Model)) {
  
  for (group in names(Model[[Rgroup]])) {
    
    # Extract regression model
    model <- Model[[Rgroup]][[group]][["regression"]]
    
    # Create dataframe of fitted values and residuals
    diag_df <- data.frame(
      fitted = fitted(model),
      residuals = resid(model)
    )
    
    # --------------------
    # Residual vs fitted plot
    # --------------------
    # Used to assess linearity, heteroskedasticity,
    # outliers and model misspecification
    
    p_fitted <- ggplot(diag_df, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.4) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        title = paste("Residuals vs fitted:", Rgroup, "vs", group),
        x = "Fitted values",
        y = "Residuals"
      ) +
      theme_minimal()
    
    ggsave(
      filename = file.path(
        output_charts,
        paste0("residuals_fitted_", Rgroup, "_vs_", group, ".png")
      ),
      plot = p_fitted,
      width = 7,
      height = 5,
      dpi = 300
    )
    
    
    # --------------------
    # Residual distribution plot
    # --------------------
    # Used to assess normality, skewness and outliers
    
    p_dist <- ggplot(diag_df, aes(x = residuals)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.5) +
      geom_density() +
      labs(
        title = paste("Residual distribution:", Rgroup, "vs", group),
        x = "Residuals",
        y = "Density"
      ) +
      theme_minimal()
    
    ggsave(
      filename = file.path(
        output_charts,
        paste0("residuals_distribution_", Rgroup, "_vs_", group, ".png")
      ),
      plot = p_dist,
      width = 7,
      height = 5,
      dpi = 300
    )
  }
}




# ======================================================
# . Export results
# ======================================================

# --------------------
# 7a. Regression tables
# --------------------

# Combine all regression models into a named list
reg_models <- list()

for (Rgroup in names(Model)) {
  
  for (group in names(Model[[Rgroup]])) {
    
    reg_models[[paste(Rgroup, "vs", group)]] <-
      Model[[Rgroup]][[group]][["regression"]]
  }
}

# Export regression results to Word document
modelsummary(
  reg_models,
  output = "regression_results.docx",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|Within"
)


# --------------------
# 7b. Oaxaca summary table
# --------------------

# Initialise summary table
oaxaca_summary <- data.frame()

# Loop through Oaxaca results
for (Rgroup in names(Model)) {
  
  for (group in names(Model[[Rgroup]])) {
    
    ox <- Model[[Rgroup]][[group]][["oaxaca"]]
    
    # Extract overall decomposition results (pooled weights)
    overall <- ox$twofold$overall[3, ]
    
    # Extract wage gap components
    gap <- ox$y$y.diff
    explained <- overall["coef(explained)"]
    unexplained <- overall["coef(unexplained)"]
    
    # Store results
    temp <- data.frame(
      reference_group = Rgroup,
      comparison_group = group,
      mean_log_wage_reference = ox$y$y.A,
      mean_log_wage_comparison = ox$y$y.B,
      log_wage_gap = gap,
      explained = explained,
      unexplained = unexplained,
      explained_share = explained / gap,
      unexplained_share = unexplained / gap
    )
    
    oaxaca_summary <- rbind(oaxaca_summary, temp)
  }
}

# Clean up
rm(ox, temp, pair_model)

# Print final summary table
print(oaxaca_summary)


# ======================================================
# Notes
# ======================================================
# The Oaxaca decomposition is reported using the pooled
# (Reimers) weighting scheme, where coefficients are averaged
# across groups (group weight = 0.5).