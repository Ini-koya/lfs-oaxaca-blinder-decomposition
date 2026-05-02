
#======================================================  
 # DESCRIPTIVE STATISTICS BY ETHNIC GROUP
#======================================================
#Library packages
library(tidyverse)
library(labelled)
library(haven)
library(writexl)
library(tidyr)

# ========================================
# Paths
# ========================================

clean_dir <- "C:/Users/sndui/OneDrive/Dissertation/Econometrics project/3. Input data/Cleaned data"

lfs_data <- read.RDS(file.path(clean_dir, "Clean.RDS"))


#=====================================================  
# 1. Sample composition
#=====================================================
  
# Observation count by ethnicity
lfs_data %>% table(ethnic)

# Gender split by ethnicity
lfs_data %>% prop.table(table(ethnic, female),margin=1) 

#=====================================================  
# 2. Wages by ethnicity and year
#=====================================================
  
#Median real hourly pay by year and ethnicity

lfs_data %>%
  group_by(year, ethnic) %>%
  summarise(median_realhrpay = round(median(realhrpay, na.rm = TRUE), 2)) %>%
  pivot_wider(names_from = ethnic, values_from = median_realhrpay)


#=====================================================
  # 3. Wage distribution by ethnicity
#=====================================================

# 3a. Basic wage summary by ethnicity
summary_wage <- lfs_data %>%
  group_by(ethnic) %>%
  summarise(
    count      = n(),
    mean_pay   = mean(realhrpay,   na.rm = TRUE),
    median_pay = median(realhrpay, na.rm = TRUE),
    sd_pay     = sd(realhrpay,     na.rm = TRUE),
    min_pay    = min(realhrpay,    na.rm = TRUE),
    max_pay    = max(realhrpay,    na.rm = TRUE)
  )

# 3b. Percentile distribution of real hourly pay by ethnicity
summary_wage_pct <- lfs_data %>%
  group_by(ethnic) %>%
  summarise(
    p10 = quantile(realhrpay, 0.10, na.rm = TRUE),
    p25 = quantile(realhrpay, 0.25, na.rm = TRUE),
    p50 = quantile(realhrpay, 0.50, na.rm = TRUE),
    p75 = quantile(realhrpay, 0.75, na.rm = TRUE),
    p90 = quantile(realhrpay, 0.90, na.rm = TRUE)
  )

# reshape from wide to long — ggplot2 requires one row per observation
summary_wage_long <- summary_wage_pct %>%
  pivot_longer(
    cols      = c(p10, p25, p50, p75, p90),
    names_to  = "percentile",
    values_to = "wage"
  )

# 3c. Plot percentile distribution of real hourly pay by ethnicity
ggplot(summary_wage_long, aes(x = percentile, y = wage, fill = ethnic)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels = c(
    "p10" = "10th",
    "p25" = "25th",
    "p50" = "50th",
    "p75" = "75th",
    "p90" = "90th"
  )) +
  labs(
    title   = "Real Hourly Pay Distribution by Ethnicity",
    x       = "Percentile",
    y       = "Real Hourly Pay (£)",
    fill    = "Ethnicity",
    caption = "Source: UK Labour Force Survey (2015-2024)"
  ) +
  theme_minimal()

#===================================================
  # 4. Age by ethnicity
#====================================================
  
lfs_data %>%
  group_by(ethnic) %>%
  summarise(mean_age = mean(age, na.rm = TRUE))


*******************************************************
  
  * 5. Immigration status by ethnicity

*******************************************************
  
  
  
  tab ethnic bborn, row

table ethnic, statistic(mean bborn)



*******************************************************
  
  * 6. Tenure by ethnicity

*******************************************************
  
  
  
  graph bar (mean) tenure, over(ethnic) blabel(bar)

table ethnic, statistic(mean tenure)



*******************************************************
  
  * 7. Number of dependent children by ethnicity

*******************************************************
  
  
  
  table ethnic, statistic(mean dep19)



*******************************************************
  
  * 8. Education by ethnicity

*******************************************************
  
  
  
  gen qual = .

replace qual = 1 if degree == 1

replace qual = 2 if higher == 1

replace qual = 3 if alevel == 1

replace qual = 4 if gcse == 1

replace qual = 5 if other == 1

replace qual = 6 if none == 1



label define qual_lbl ///
  
  1 "Degree or equivalent" ///
  
  2 "Higher education" ///
  
  3 "A-level or equivalent" ///
  
  4 "GCSE A*-C or equivalent" ///
  
  5 "Other qualifications" ///
  
  6 "No qualifications", replace



label values qual qual_lbl



tab ethnic qual, row

table ethnic qual, statistic(percent)



*******************************************************
  
  * 9. Marital status by ethnicity

*******************************************************
  
  
  
  label define marr_label ///
  
  1 "Married/cohabiting/civil partner" ///
  
  0 "Non-married", replace



label values marr marr_label



tab ethnic marr, row nofreq



*******************************************************
  
  * 10. Occupation by ethnicity

*******************************************************
  
  
  
  label define occup_lbl ///
  
  1 "Managers, Directors and Senior Officials" ///
  
  2 "Professional Occupations" ///
  
  3 "Associate Professional and Technical Occupations" ///
  
  4 "Administrative and Secretarial Occupations" ///
  
  5 "Skilled Trades Occupations" ///
  
  6 "Caring, Leisure and Other Service Occupations" ///
  
  7 "Sales and Customer Service Occupations" ///
  
  8 "Process, Plant and Machine Operatives" ///
  
  9 "Elementary Occupations", replace



label values occup occup_lbl



tab ethnic occup, row nofreq

