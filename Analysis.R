# ======================================================
# Oaxaca analysis
# Purpose: Estimate wage gaps and decompose them into
# explained and unexplained components across ethnic groups
# ======================================================


# ====================
# 1. Libraries
# ====================
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
library(leaps)


# ====================
# 2. Paths and data
# ====================
# Define file path and load cleaned Labour Force Survey data

clean_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data/Cleaned data"

output_charts <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/Charts"

reg_output <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/regression results"

counterfactual_output <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/Counterfactual output"

# Load clean data
lfs_data <- readRDS(file.path(clean_dir, "Clean.RDS"))

# ====================
# 3. Further cleaning
# ====================
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
# 5. Pooled and Pairwise regressions and Oaxaca decomposition
# ======================================================

# Pooled regression

# Run OLS regression with year fixed effects
Pooled_model <- feols(
  as.formula(paste("loghrp ~", regressors, "| year")),
  data = lfs_data,
  data.save = TRUE)

individual_model <- list()

#individual regressions
for (group in ethnic_groups) {
  
  # Subset data to include only the group of interest
  df_sub <- lfs_data[lfs_data[[group]] == 1, ]
  
  # Run OLS regression with year fixed effects
  temp <- feols(
    as.formula(paste("loghrp ~", regressors, "| year")),
    data = df_sub,
    data.save = TRUE
  )
  
  # Store the model directly
  individual_model[[group]] <- temp
}

rm(df_sub)

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
rm(df_sub, model, oaxaca_result,pair_model)



# ======================================================
# 6. Counterfactual simulation analysis
# ======================================================
# Purpose:
# To assess how much selected factors contribute to the wage gap by
# estimating how the gap changes when group characteristics/coefficeints are equalised.

# This approach complements the Oaxaca-Blinder decomposition
# by providing a more intuitive measure of the contribution
# of individual factors to wage differentials which 
#the coefficient level

# ------------------------------------------------------
# 6.1.1  Setup
# ------------------------------------------------------

# Define reference ethnic groups for pairwise comparisons
core_comparison_groups <- c( "black", "pakistani",
  "bangladeshi","indian", "chinese")

core_ref_groups <- c(  "indian", "chinese")

counterfactual_model<- list()

for(ref in core_ref_groups){
  counterfactual_model[[ref]]<- list()
  
for (groups in core_comparison_groups) {
  
  counterfactual_model[[ref]][[groups]] <- list()
}
}

# ------------------------------------------------------
# 6.1.2 Calculate average characteristics by group
# ------------------------------------------------------
# For each ethnic group, calculate the mean value of each explanatory variable.
# These group averages are used to construct counter factual wage predictions.

# vars: vector of variable names
# ethnic_groups: column in lfs_data identifying groups
for(ref in core_ref_groups){
for (group in core_comparison_groups) {
  
  means_x <- c()
  
  for (var in vars) {
    
    temp <- lfs_data %>%
      filter(.data[[group]] == 1) %>%
      summarise(
        mean = mean(.data[[var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pull(mean)
    means_x[var] <- temp
  }
  
  counterfactual_model[[ref]][[group]][["x"]] <- means_x
}
}
# Clean up temporary objects(optional)
rm(temp,means_x)


# ------------------------------------------------------
# 6.1.3 Extract individual / pairwise pooled regression coefficients
# ------------------------------------------------------

for (ref in core_ref_groups) {
  
  for (group in core_comparison_groups) {
    
    
    # Pairwise pooled coefficients from the stored pairwise regression
    counterfactual_model[[ref]][[group]][["pooled_coefficients"]] <-
      coef(Model[[ref]][[group]][["regression"]])
    
    # Comparison group-specific coefficients
    counterfactual_model[[ref]][[group]][["individual_coefficients"]] <-
      coef(individual_model[[group]])
  }
  
  # Also store reference group-specific coefficients and characteristics
  counterfactual_model[[ref]][[ref]][["individual_coefficients"]] <-
    coef(individual_model[[ref]])
}
}
# ======================================================
# 6.1.4 Counterfactual analysis
# ======================================================
# For each variable:
# - replace both groups' coefficients with the pooled coefficient
# - keep characteristics fixed
# This captures the role of differences in returns
# ------------------------------------------------------
# A. Coefficient / returns counterfactual
# ------------------------------------------------------

counterfactual_results_coeff <- data.frame()

for (ref in core_ref_groups) {
  
  for (group in core_comparison_groups) {
    
    if (group == ref) next
    
    individual_comp <- counterfactual_model[[ref]][[group]][["individual_coefficients"]]
    individual_ref  <- counterfactual_model[[ref]][[ref]][["individual_coefficients"]]
    pooled_coef     <- counterfactual_model[[ref]][[group]][["pooled_coefficients"]]
    
    x_comp <- counterfactual_model[[ref]][[group]][["x"]]
    x_ref  <- counterfactual_model[[ref]][[ref]][["x"]]
    
    common_vars <- intersect(
      intersect(names(individual_comp), names(individual_ref)),
      intersect(names(pooled_coef), names(x_comp))
    )
    
    common_vars <- intersect(common_vars, names(x_ref))
    common_vars <- intersect(common_vars, vars)
    
    w_comp <- sum(individual_comp[common_vars] * x_comp[common_vars], na.rm = TRUE)
    w_ref  <- sum(individual_ref[common_vars]  * x_ref[common_vars], na.rm = TRUE)
    
    baseline_gap <- w_ref - w_comp
    
    for (v in common_vars) {
      
      cf_comp_coef <- individual_comp
      cf_ref_coef  <- individual_ref
      
      cf_comp_coef[v] <- pooled_coef[v]
      cf_ref_coef[v]  <- pooled_coef[v]
      
      w_comp_cf <- sum(cf_comp_coef[common_vars] * x_comp[common_vars], na.rm = TRUE)
      w_ref_cf  <- sum(cf_ref_coef[common_vars]  * x_ref[common_vars], na.rm = TRUE)
      
      cf_gap <- w_ref_cf - w_comp_cf
      
      contribution <- baseline_gap - cf_gap
      contribution_share <- 100 * contribution / baseline_gap
      
      temp <- data.frame(
        reference_group = ref,
        comparison_group = group,
        channel = "returns_coefficients",
        variable = v,
        Pooled_coefficient= pooled_coef[v],
        observed_gap = baseline_gap,
        counterfactual_gap = cf_gap,
        contribution = contribution,
        contribution_share = contribution_share
      
      )
      
      counterfactual_results_coeff <- rbind(counterfactual_results_coeff, temp)
    }
  }
}


# ------------------------------------------------------
# B. Characteristics / expected X counterfactual
# ------------------------------------------------------
# For each variable:
# - replace the comparison group's average characteristic
#   with the reference group's average characteristic
# - keep coefficients fixed
# This captures the role of differences in characteristics.

counterfactual_results_x <- data.frame()

for (ref in core_ref_groups) {
  
  for (group in core_comparison_groups) {
    
    if (group == ref) next
    
    individual_comp <- counterfactual_model[[ref]][[group]][["individual_coefficients"]]
    individual_ref  <- counterfactual_model[[ref]][[ref]][["individual_coefficients"]]
    
    x_comp <- counterfactual_model[[ref]][[group]][["x"]]
    x_ref  <- counterfactual_model[[ref]][[ref]][["x"]]
    
    common_vars <- intersect(
      intersect(names(individual_comp), names(individual_ref)),
      intersect(names(x_comp), names(x_ref))
    )
    
    common_vars <- intersect(common_vars, vars)
    
    w_comp <- sum(individual_comp[common_vars] * x_comp[common_vars], na.rm = TRUE)
    w_ref  <- sum(individual_ref[common_vars]  * x_ref[common_vars], na.rm = TRUE)
    
    baseline_gap <- w_ref - w_comp
    
    for (v in common_vars) {
      
      x_comp_cf <- x_comp
      x_comp_cf[v] <- x_ref[v]
      
      w_comp_cf <- sum(individual_comp[common_vars] * x_comp_cf[common_vars], na.rm = TRUE)
      w_ref_cf  <- w_ref
      
      cf_gap <- w_ref_cf - w_comp_cf
      
      contribution <- baseline_gap - cf_gap
      contribution_share <- 100 * contribution / baseline_gap
      
      temp <- data.frame(
        reference_group = ref,
        comparison_group = group,
        channel = "characteristics_expected_x",
        variable = v,
        observed_gap = baseline_gap,
        counterfactual_gap = cf_gap,
        contribution = contribution,
        contribution_share = contribution_share
      )
      
      counterfactual_results_x <- rbind(counterfactual_results_x, temp)
    }
  }
}


# ------------------------------------------------------
# C. Combine and export
# ------------------------------------------------------

counterfactual_results_all <- rbind(
  counterfactual_results_coeff,
  counterfactual_results_x
)

write_xlsx(
  counterfactual_results_coeff,
  file.path(counterfactual_output, "Coefficient_results.xlsx")
)

write_xlsx(
  counterfactual_results_x,
  file.path(counterfactual_output, "Characteristics_results.xlsx")
)

write_xlsx(
  counterfactual_results_all,
  file.path(counterfactual_output, "Counterfactual_results_all.xlsx")
)
# ======================================================
# 6. Export results
# ======================================================

# --------------------
# 6a. Regression tables
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


# ------------------------------------------------------
# 6b.Oaxaca summary and coefficient-level results
# ------------------------------------------------------

# Initialise output tables
oaxaca_summary <- data.frame()
oaxaca_coefficients <- data.frame()

# Loop through Oaxaca results
for (Rgroup in names(Model)) {
  for (group in names(Model[[Rgroup]])) {
    # Extract Oaxaca object
    ox <- Model[[Rgroup]][[group]][["oaxaca"]]
    
    # --------------------
    # Aggregate results
    # --------------------
    
    # Extract overall decomposition results using pooled weights
    overall <- ox$twofold$overall[3, ]
    
    # Extract wage gap components
    gap <- ox$y$y.diff
    explained <- overall["coef(explained)"]
    unexplained <- overall["coef(unexplained)"]
    
    # Store aggregate Oaxaca results
    temp_summary <- data.frame(
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
    
    oaxaca_summary <- rbind(oaxaca_summary, temp_summary)
    
    
    # --------------------
    # Coefficient-level results
    # --------------------
    
    # Extract variable-level decomposition results using pooled weights
    var_coeff <- ox$twofold$variables[[3]]
    
    # Convert matrix output into data frame
    var_coeff <- as.data.frame(var_coeff)
    var_coeff$Variable <- rownames(var_coeff)
    
    # Add group identifiers and overall decomposition values
    var_coeff$Reference_group <- Rgroup
    var_coeff$Comparison_group <- group
    var_coeff$log_wage_gap <- gap
    var_coeff$explained <- explained
    var_coeff$unexplained <- unexplained
    
    # Extract clean contribution columns
    temp_coeff <- data.frame(
      reference_group = var_coeff$Reference_group,
      comparison_group = var_coeff$Comparison_group,
      variable = var_coeff$Variable,
      explained_contribution = as.numeric(var_coeff[, "coef(explained)"]),
      unexplained_contribution = as.numeric(var_coeff[, "coef(unexplained)"]),
      explained_importance = abs(as.numeric(var_coeff[, "coef(explained)"])),
      unexplained_importance = abs(as.numeric(var_coeff[, "coef(unexplained)"])),
      explained_share = as.numeric(var_coeff[, "coef(explained)"]) / explained,
      unexplained_share = as.numeric(var_coeff[, "coef(unexplained)"]) / unexplained,
      log_wage_gap = gap,
      total_explained = explained,
      total_unexplained = unexplained
    )
    
    oaxaca_coefficients <- rbind(oaxaca_coefficients, temp_coeff)
  }
}

# Oaxaca results Summary
write_xlsx(oaxaca_summary, file.path(reg_output, "Oaxaca_summary.xlsx"))

# Oaxaca Coefficient-level results
write_xlsx(oaxaca_coefficients, file.path(reg_output, "Oaxaca_coef.xlsx"))

# Clean up temporary objects(optional)
rm(Rgroup, group, ox, overall, gap, explained, unexplained, temp_summary, var_coeff, temp_coeff, pair_model)








# ======================================================
# Model validity checks
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

# Clean up temporary objects(optional)
rm(model, p_dist, p_fitted, diag_df)
