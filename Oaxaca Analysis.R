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

library(tidyverse)      # mutate, group_by, summarise, ggplot2, ggsave
library(haven)          # readRDS (if saved as haven format)
library(writexl)        # write_xlsx
library(oaxaca)         # oaxaca()
library(fixest)         # feols()
library(pscl)           # pR2() — McFadden pseudo R²
library(modelsummary)   # modelsummary()
library(data.table)     # as.data.table(), data.table subsetting
#library(sampleSelection)

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

# Data table conversion 
lfs_data <- as.data.table(lfs_data)
# ====================
# 3. Further cleaning
# ====================
# Create squared experience term for non-linear returns to experience

lfs_data <- lfs_data %>%
  mutate(exper2 = exper^2)

lfs_data <- lfs_data %>%
  mutate(age2 = age^2)

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

# Define list of control variables used in probit regression for Heckman correction
vars_IMR <- c(
  "age", "age2", "female", "marr", "bborn", "disabled",
  "dep19", "london","degree", "higher", "alevel", "gcse",
  "other", "black", "mixed", "indian", "pakistani", "bangladeshi",
  "chinese"
)

# Convert vector of variables into regression formula string
regressors <- paste(vars, collapse = " + ")


# Convert vector of variables into regression formula string(Heckman correction)
regressors_IMR <- paste(vars_IMR, collapse = " + ")

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
# Heckman Selection Correction
# Stage 1: Probit model of employment probability
# Estimated on full sample (employed and unemployed)
# Generates Inverse Mills Ratio (IMR) to correct for
# selection bias in the second stage wage equation
# ======================================================

probit_model <- glm(
  as.formula(paste("employed ~", regressors_IMR, "+ factor(year)","+ factor(region)")), 
  data = lfs_data, 
  family = binomial(link = "probit")
)

# Compute and add IMR to the main data set
lfs_data <- lfs_data %>%
    mutate(
         fitted_prob = fitted(probit_model),
        IMR = dnorm(qnorm(fitted_prob)) / pnorm(qnorm(fitted_prob)))


# McFadden's pseudo-R²
pR2(probit_model)

# Classification accuracy
predicted <- ifelse(fitted(probit_model) > 0.5, 1, 0)
table(predicted, lfs_data$employed)

# Summarize the IMR ratio by ethnicity
lfs_data %>%
  group_by(ethnic) %>%
  summarise(mean_IMR = mean(IMR, na.rm = TRUE))

rm(probit_model,predicted)
gc()

write_rds(lfs_data,file.path(clean_dir,"LFS_data_IMR"))

# ======================================================
# 5. Pooled and Pairwise regressions and Oaxaca decomposition
# ======================================================

#----------------------
# A. Pooled regression
#----------------------

# Run OLS regression with year fixed effects(without Heckman)
Pooled_model <- feols(
  as.formula(paste("loghrp ~", regressors, "| year")),
  data = lfs_data,
  data.save = TRUE)
  
  # Run OLS regression with year fixed effects(with Heckman)
  Pooled_model_IMR <- feols(
    as.formula(paste("loghrp ~", regressors,"+ IMR", "| year")),
    data = lfs_data,
    data.save = TRUE)

individual_model <- list()

#------------------------------------
# B. Individual etnicity regressions
#------------------------------------
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
gc()
#----------------------------------------------------------------
# C. Main analysis(Pairwise regressions)
#----------------------------------------------------------------

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
        as.formula(paste("loghrp ~", regressors_oaxaca, "| factor(year)")),
        data = df_sub,
        #data.save = TRUE  # Optional save data set used
      )

  
      # Store regression 
      Model[[Rgroup]][[group]] <- list(
        regression = pair_model)
        
        # Clean up at end of each iteration
        rm(df_sub, pair_model)
        gc()  # forces garbage collection
        
      
    }
  }
}


# --------------------
# Export Regression tables
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



#----------------------------------------------------------------
# C. Main analysis(OAXCA decomposition + heckman correction)
#----------------------------------------------------------------

# Loop over each reference group
for (Rgroup in reference_groups) {
  # Create sub-list for each reference group
  #Model[[Rgroup]] <- list()
  
  # Loop over comparison groups
  for (group in ethnic_groups) {
    # Skip comparison of group with itself
    if (group != Rgroup) {
      # Subset data to include only the two groups of interest
      df_sub <- lfs_data[
        lfs_data[[Rgroup]] == 1 | lfs_data[[group]] == 1,
      ]
      
      
      # Run Oaxaca-Blinder decomposition with Heckman correction
      # Outcome: log hourly pay
      # Group variable: current comparison group
      oaxaca_result <- oaxaca(
        as.formula(paste("loghrp ~", regressors_oaxaca,"+ factor(year)","|", group)),
        data = df_sub,
        R = NULL # Bootstrap setting
      )
      
      # Store both regression and Oaxaca results
      Model[[Rgroup]][[group]] <- list(
        oaxaca_IMR = oaxaca_result$twofold,
        y          = oaxaca_result$y,
        N          = oaxaca_result$n
      )
      
      # Clean up at end of each iteration
      rm(df_sub, oaxaca_result)
      gc()  # forces garbage collection
      
      
    }
  }
}


# ------------------------------------------------------
# Export .Oaxaca summary 
# ------------------------------------------------------

# Initialise output tables
oaxaca_summary <- data.frame()

# Loop through Oaxaca results
for (Rgroup in names(Model)) {
  for (group in names(Model[[Rgroup]])) {
    # Extract Oaxaca object
    ox  <- Model[[Rgroup]][[group]][["oaxaca_IMR"]]
    y   <- Model[[Rgroup]][[group]][["y"]]        
    N   <- Model[[Rgroup]][[group]][["N"]]
    
    # --------------------
    # Aggregate results
    # --------------------
    
    # Extract overall decomposition results using pooled weights 
    #;adjustable based on preference of common wage structure
    # Row 1 = Group B (comparison) coefficients as reference wage structure
    # Row 2 = Group A (reference) coefficients as reference wage structure  
    # Row 3 = Pooled 50/50 average of both groups' coefficients
    # Row 4 = Cotton weights — weighted by relative group size
    # Row 5 = Neumark — pooled regression coefficients (theoretically preferred)
    # Row 6 = Oaxaca-Ransom — alternative pooled using variance-covariance weights
    overall <- ox$overall[2, ] 
    
    # Extract wage gap components
    explained <- overall["coef(explained)"]
    unexplained <- overall["coef(unexplained)"]
    gap         <- explained + unexplained
    
    # Store aggregate Oaxaca results
    temp_summary <- data.frame(
      reference_group          = Rgroup,
      comparison_group         = group,
      mean_log_wage_reference  = y$y.A,
      mean_log_wage_comparison = y$y.B,
      log_wage_gap             = gap,
      explained                = explained,
      unexplained              = unexplained,
      explained_share          = explained / gap,
      unexplained_share        = unexplained / gap,
      N                        = N$n.pooled
    
    )
    
    oaxaca_summary <- rbind(oaxaca_summary, temp_summary)
    
   
  }
}

# Oaxaca results Summary
write_xlsx(oaxaca_summary, file.path(reg_output, "Oaxaca_summary.xlsx"))

# Clean up temporary objects(optional)
rm(Rgroup, group, ox, overall, gap, explained, unexplained, temp_summary)








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


corrs<-cor(lfs_data[, vars], use = "complete.obs"
