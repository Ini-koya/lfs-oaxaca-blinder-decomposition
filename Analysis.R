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


# ====================
# 2. Paths and data
# ====================
# Define file path and load cleaned Labour Force Survey data

clean_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data/Cleaned data"

output_charts <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/Charts"

reg_output <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/4. Output/regression results"

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
# 5. pooled and Pairwise regressions and Oaxaca decomposition
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
# Counterfactual analysis
# ======================================================
# Purpose:
# To assess how much selected factors contribute to the wage gap by
# estimating how the gap changes when group characteristics are equalised.

# This approach complements the Oaxaca-Blinder decomposition
# by providing a more intuitive measure of the contribution
# of individual factors to wage differentials which 
#the coefficient level

# Define reference ethnic groups for pairwise comparisons
core_ethnic_groups <- c( "black", "indian", "pakistani",
  "bangladeshi", "chinese")

counterfactual_model<- list()

for (groups in core_ethnic_groups) {
  counterfactual_model[[groups]] <- list()
}


# ------------------------------------------------------
# 1. Calculate average characteristics by group
# ------------------------------------------------------
# For each ethnic group, calculate the mean value of each explanatory variable.
# These group averages are used to construct counter factual wage predictions.

# vars: vector of variable names
# ethnic_groups: column in lfs_data identifying groups
for (group in core_ethnic_groups) {
  
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
  
  counterfactual_model[[group]][["x"]] <- means_x
}

# Clean up temporary objects(optional)
rm(temp,means_x)


# ------------------------------------------------------
# 2. Generate individual/pooled regression coefficients
# ------------------------------------------------------
# Use coefficients from the pooled wage regression as the common returns structure.
# This ensures counterfactual differences reflect changes in characteristics,
# rather than differences in estimated coefficients across groups.

for (groups in core_ethnic_groups) {
  
  counterfactual_model[[groups]][["pooled_coefficients"]] <- coef(Pooled_model)
  
  counterfactual_model[[groups]][["individual_coefficients"]] <-
    coef(individual_model[[groups]])
}


# ------------------------------------------------------
# 3. Construct counterfactual predictions
# ------------------------------------------------------
# For each reference-comparison pair, replace the comparison group’s average
# characteristics with the reference group’s characteristics for selected variables
# or factor groups.

factor_groups <- list(
  "Experience / human capital accumulation" = c("exper", "exper2", "tenure"),
  "Education" = c("degree", "higher", "alevel", "gcse", "other"),
  "Demographics / personal characteristics" = c("female", "marr", "bborn", "disabled", "dep19"),
  "Job characteristics / occupation" = c(
    "manager", "professional", "associate", "administrative",
    "skilled", "caring", "sales", "elementary"
  ),
  "Location" = c("London"),
  "Sector / firm characteristics" = c("public", "lcomp")
)

group_means <- lfs_data %>%
  group_by(ethnic) %>%
  summarise(
    across(all_of(vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

pooled_coef <- coef(Pooled_model)
pooled_coef <- pooled_coef[names(pooled_coef) %in% vars]


# ------------------------------------------------------
# 4. Calculate counterfactual wage gaps
# ------------------------------------------------------
# For each replacement, calculate the predicted counterfactual wage gap.
# This shows what the wage gap would be if the comparison group had the same
# characteristics as the reference group for that factor.

counterfactual_results <- data.frame()

for (Rgroup in reference_groups) {
  
  for (group in ethnic_groups) {
    
    if (group != Rgroup) {
      
      ref_x <- group_means %>%
        filter(ethnic == Rgroup) %>%
        select(all_of(vars))
      
      comp_x <- group_means %>%
        filter(ethnic == group) %>%
        select(all_of(vars))
      
      ref_x <- as.numeric(ref_x[1, ])
      comp_x <- as.numeric(comp_x[1, ])
      
      names(ref_x) <- vars
      names(comp_x) <- vars
      
      ref_pred <- sum(ref_x[names(pooled_coef)] * pooled_coef)
      comp_pred <- sum(comp_x[names(pooled_coef)] * pooled_coef)
      
      observed_gap <- ref_pred - comp_pred
      
      for (factor_name in names(factor_groups)) {
        
        factor_vars <- factor_groups[[factor_name]]
        factor_vars <- factor_vars[factor_vars %in% names(pooled_coef)]
        
        comp_cf <- comp_x
        comp_cf[factor_vars] <- ref_x[factor_vars]
        
        comp_cf_pred <- sum(comp_cf[names(pooled_coef)] * pooled_coef)
        counterfactual_gap <- ref_pred - comp_cf_pred
        
        gap_change <- observed_gap - counterfactual_gap
        pct_reduction <- gap_change / observed_gap
        
        temp <- data.frame(
          Reference_group = Rgroup,
          Comparison_group = group,
          Factor = factor_name,
          observed_gap = observed_gap,
          counterfactual_gap = counterfactual_gap,
          gap_change = gap_change,
          pct_reduction = pct_reduction
        )
        
        counterfactual_results <- rbind(counterfactual_results, temp)
      }
    }
  }
}

# ------------------------------------------------------
# 5. Estimate contribution of each factor
# ------------------------------------------------------
# Calculate the difference between:
# - the observed/predicted wage gap
# - the counterfactual wage gap
#
# A larger reduction in the gap indicates that the factor is more important
# in explaining the wage difference.

counterfactual_results <- counterfactual_results %>%
  mutate(
    Comparison = paste0(Reference_group, " vs ", Comparison_group),
    observed_gap_pct = (exp(observed_gap) - 1) * 100,
    counterfactual_gap_pct = (exp(counterfactual_gap) - 1) * 100,
    gap_change_pct = (exp(gap_change) - 1) * 100,
    pct_reduction = pct_reduction * 100
  ) %>%
  select(
    Comparison,
    Factor,
    observed_gap,
    counterfactual_gap,
    gap_change,
    observed_gap_pct,
    counterfactual_gap_pct,
    gap_change_pct,
    pct_reduction
  )













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
