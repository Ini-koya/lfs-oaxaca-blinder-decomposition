# ========================================
# Paths/Library
# ========================================
# Library packages
library(tidyverse)
library(labelled)
library(haven)
library(writexl)
library(readxl)

# Setting file paths
raw_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data/modified raw data/Combined data"

out_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data/Cleaned data"


# LFS user guide: https://doc.ukdataservice.ac.uk/doc/8840/mrdoc/pdf/lfs_user_guide_vol3_variable_details_2021aj.pdf
# ========================================
# Load data (replace filename as needed)
# ========================================

lfs_data <- readRDS(file.path(raw_dir, "combined_lfs_all_quarters.rds"))


# ====================
## II. Data Cleaning
# ====================


# ========================================
# Rename / basic variables
# ========================================

var_label(lfs_data$year) <- "Year of LFS study"
var_label(lfs_data$quarter) <- "Quarter of LFS study"

# ========================================
# Employment status
# ========================================

# ILODEFR - Basic economic activity (ILO definition) (reported)
# (1)In employment
# (2)ILO unemployed
# (3)Inactive
# (4)Under 16

# Tabulate original variable
table(lfs_data$ILODEFR, useNA = "always")

# Generate employed dummy variable (Employed:1 or Unemployed:0)
lfs_data <- lfs_data %>%
  filter(ILODEFR %in% c(1, 2)) %>% # Only observations that employed or unemployed are required for this analysis
  filter(!is.na(ILODEFR))  %>%     # remove missing values
  mutate(employed = case_when(
    ILODEFR == 1 ~ 1,              #  Employed: 1
    ILODEFR == 2 ~ 0               #  Unemployed: 0
  )) 

# Label age variable
var_label(lfs_data$employed) <- "Employed:1 or Unemployed:0"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-ILODEFR)

# Tabulate new variable
table(lfs_data$employed, useNA = "always")

# ========================================
# Region dummy variable (1/0)
# ========================================
# Original LFS description:
# GOVTOF2 - Government Office Region (2 and 3 combined)
# 1 = North East, 2 = North West, 4 = Yorkshire and Humberside,
# 5 = East Midlands, 6 = West Midlands, 7 = East of England,
# 8 = London, 9 = South East, 10 = South West,
# 11 = Wales, 12 = Scotland, 13 = Northern Ireland

# Tabulate original variable
table(lfs_data$GOVTOF2, useNA = "always")

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
# Location is a key characteristic required for all observations included in the analysis.
lfs_data <- lfs_data %>% filter(!(GOVTOF2 %in% c(-8, -9)) & !is.na(GOVTOF2))

# Generate  residency dummy variable
lfs_data <- lfs_data %>%
  mutate(london = if_else(GOVTOF2 == 8, 1, 0))


# Generate region variable
lfs_data <- lfs_data %>%
  mutate(
    region = factor(
      GOVTOF2,
      levels = c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
      labels = c(
        "North East", "North West", "Yorkshire and Humber",
        "East Midlands", "West Midlands", "East of England",
        "London", "South East", "South West",
        "Wales", "Scotland", "Northern Ireland"
      )
    )
  )

# Label London residency variable
var_label(lfs_data$london) <- "London residency (1/0)"

# Assert no missing values in London
stopifnot(all(!is.na(lfs_data$london)))

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-GOVTOF2)

# Tabulate new variable
table(lfs_data$london, useNA = "always")

# ========================================
# Ethnicity dummy variables (1/0)
# ========================================
# Original LFS description:
# ETHUKEUL - Ethnicity (9 categories) UK level
# 1 = White, 2 = Mixed/Multiple ethnic groups, 3 = Indian,
# 4 = Pakistani, 5 = Bangladeshi, 6 = Chinese,
# 7 = Any other Asian background, 8 = Black/African/Caribbean/Black British,
# 9 = Other ethnic group, -8 = No answer, -9 = Not applicable

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
# Ethnicity is a key characteristic required for all observations included in the analysis.
lfs_data <- lfs_data %>% filter(!(ETHUKEUL %in% c(-8, -9)) & !is.na(ETHUKEUL))

# Ethnicity named vector
ethnic <- c(
  white        = 1,
  mixed        = 2,
  indian       = 3,
  pakistani    = 4,
  bangladeshi  = 5,
  chinese      = 6,
  black        = 8
)

# Loop creating ethnicity dummy variables
for (i in names(ethnic)) {
  lfs_data <- lfs_data %>%
    mutate(!!i := ifelse(ETHUKEUL == ethnic[i], 1, 0))
}

# Label ethnicity dummy variables
for (i in names(ethnic)) {
  var_label(lfs_data[[i]]) <- paste0(i, " (1/0)")
}

# Add aggregated ethnicity variable
lfs_data <- lfs_data %>%
  mutate(
    ethnic = case_when(
      white == 1 ~ "White",
      mixed == 1 ~ "Mixed",
      indian == 1 ~ "Indian",
      pakistani == 1 ~ "Pakistani",
      bangladeshi == 1 ~ "Bangladeshi",
      chinese == 1 ~ "Chinese",
      black == 1 ~ "Black",
      TRUE ~ NA_character_
    ),
    ethnic = factor(ethnic)
  )

# Label aggregated ethnicity variable
var_label(lfs_data$ethnic) <- "Aggregated ethnicity variable"


# Drop rows where ethnicity is missing
lfs_data <- lfs_data %>% filter(!is.na(ethnic))

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-ETHUKEUL)

# Summarise ethnicity
table(lfs_data$ethnic, useNA = "always")

# ========================================
# British-born dummy variable (1/0)
# ========================================
# Original LFS description:
# CRY12 - Country of Birth
# 921 = England, 922 = Northern Ireland, 923 = Scotland,
# 924 = Wales, 926 = UK/Britain (Did not know country)
# 372 = Republic of Ireland
# 356 = India
# 586 = Pakistan
# 616 = Poland
# 997 = Other


# CRY01 - Country of Birth
# 921 = England
# 922 = Northern Ireland
# 923 = Scotland
# 924 = Wales
# 926 = UK/Britain (country unknown)
# 372 = Republic of Ireland
# 344 = Hong Kong
# 156 = China
# 997 = Other

# Tabulate original variable
table(lfs_data$CRY12, useNA = "always")
table(lfs_data$CRY01, useNA = "always")

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
# Country of birth is a required characteristic for all observations. 
# Observations are only excluded where both CRY12 and CRY01 are simultaneously 
# invalid, observations are kept where at least one variable carries a valid value.
lfs_data <- lfs_data %>% filter(!(
    (CRY12 %in% c(-8, -9) | is.na(CRY12)) & 
    (CRY01 %in% c(-8, -9) | is.na(CRY01))
  ))
# Generate british-born dummy variable
lfs_data <- lfs_data %>%
  mutate(
    bborn = case_when(
      CRY12 %in% c(921, 922, 923, 924, 926) ~ 1, # British born
      CRY01 %in% c(921, 924, 923, 922, 926) ~ 1, # British born
      TRUE ~ 0 # Everything else 0
    )
  )

# Label british-born variable
var_label(lfs_data$bborn) <- "UK born individual (1/0)"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-CRY12)
lfs_data <- lfs_data %>% select(-CRY01)

# Tabulate new variable
table(lfs_data$bborn, useNA = "always")

# ========================================
# Female dummy variable (1/0)
# ========================================
# Original LFS description:
# SEX[Sex of respondent (Values information: 1.0 = Male, 2.0 = Female)]

# Tabulate original variable
table(lfs_data$SEX, useNA = "always")

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
# Gender is a key characteristic required for all observations included in the analysis.
lfs_data <- lfs_data %>% filter(!(SEX %in% c(-8, -9)) & !is.na(SEX))

# Generate Female dummy variable (1: Female | 0: Male)
lfs_data <- lfs_data %>%
  mutate(female = case_when(
    SEX == 2.0 ~ 1,
    TRUE ~ 0
  ))

# Label Sex variable
var_label(lfs_data$female) <- "Female (1: Female | 0: Male)"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-SEX)

# Tabulate new variable
table(lfs_data$female, useNA = "always")

# ======================================
# Relationship status dummy variable
# ======================================
# Original LFS description:
# MARDY6 - married/Co-habiting/Civil Partners
# 1 = married/Cohabiting/Civil Partner, 2 = Non married,

# Tabulate original variable
table(lfs_data$MARDY6, useNA = "always")

# Dropping missing / no answers: -8 = No answer,
# -9 = Not applicable, it is inferred this means an observation is not married
lfs_data <- lfs_data %>% filter(!(MARDY6 %in% -8) & !is.na(MARDY6))

# Cleaning marriage variable
lfs_data <- lfs_data %>%
  mutate(
    marr = case_when(
      MARDY6 == 2 ~ 0,
      MARDY6 == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Label Marriage variable
var_label(lfs_data$marr) <- "married/cohabiting/civil partner"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-MARDY6)

# Tabulate new variable
table(lfs_data$marr, useNA = "always")

# ======================================
# Occupation dummy variables
# ======================================
# Occupation variables:
# SC10MMJ is used for surveys before 2021
# SC20MMJ is used for surveys from 2021 onwards

# Both variables capture the respondent’s major occupation group (main job)
# using the same classification:

# 1 = Managers, Directors and Senior Officials
# 2 = Professional Occupations
# 3 = Associate Professional and Technical Occupations
# 4 = Administrative and Secretarial Occupations
# 5 = Skilled Trades Occupations
# 6 = Caring, Leisure and Other Service Occupations
# 7 = Sales and Customer Service Occupations
# 8 = Process, Plant and Machine Operatives
# 9 = Elementary Occupations

# Tabulate original variable
table(lfs_data$SC10MMJ, useNA = "always")
table(lfs_data$SC20MMJ, useNA = "always")

# Generate aggregated occupation variable
lfs_data <- lfs_data %>%
  mutate(
    occup = case_when(
      year < 2021 ~ SC10MMJ,
      year >= 2021 ~ SC20MMJ
    )
  ) %>%
  # Labour-related variables:
  # Observations with "No answer" or "Not applicable" are removed for employed individuals,
  # while unemployed individuals are retained and these values are recoded as NA.
  filter(!(employed == 1 & (occup %in% c(-8, -9) | is.na(occup)))) %>%
  mutate(occup = if_else(occup %in% c(-8, -9), NA_real_, as.numeric(occup)))

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-c(SC10MMJ, SC20MMJ))

# Generate individual occupation dummies
lfs_data <- lfs_data %>%
  mutate(
    manager = if_else(is.na(occup), NA_real_, as.numeric(occup == 1)),
    professional = if_else(is.na(occup), NA_real_, as.numeric(occup == 2)),
    associate = if_else(is.na(occup), NA_real_, as.numeric(occup == 3)),
    administrative = if_else(is.na(occup), NA_real_, as.numeric(occup == 4)),
    skilled = if_else(is.na(occup), NA_real_, as.numeric(occup == 5)),
    caring = if_else(is.na(occup), NA_real_, as.numeric(occup == 6)),
    sales = if_else(is.na(occup), NA_real_, as.numeric(occup == 7)),
    operative = if_else(is.na(occup), NA_real_, as.numeric(occup == 8)),
    elementary = if_else(is.na(occup), NA_real_, as.numeric(occup == 9))
  )

# Clean aggregate occupation variable
lfs_data <- lfs_data %>%
  mutate(
    occup = factor(
      occup,
      levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
      labels = c(
        "Managers, Directors and Senior Officials",
        "Professional Occupations",
        "Associate Professional and Technical Occupations",
        "Administrative and Secretarial Occupations",
        "Skilled Trades Occupations",
        "Caring, Leisure and Other Service Occupations",
        "Sales and Customer Service Occupations",
        "Process, Plant and Machine Operatives",
        "Elementary Occupations"
      )
    )
  )

# Label occupation dummy variables
var_label(lfs_data$manager) <- "Managers, Directors and Senior Officials"
var_label(lfs_data$professional) <- "Professional Occupations"
var_label(lfs_data$associate) <- "Associate Professional and Technical Occupations"
var_label(lfs_data$administrative) <- "Administrative and Secretarial Occupations"
var_label(lfs_data$skilled) <- "Skilled Trades Occupations"
var_label(lfs_data$caring) <- "Caring, Leisure and Other Service Occupations"
var_label(lfs_data$sales) <- "Sales and Customer Service Occupations"
var_label(lfs_data$operative) <- "Process, Plant and Machine Operatives"
var_label(lfs_data$elementary) <- "Elementary Occupations"

# Tabulate new variable
table(lfs_data$occup, useNA = "always")

# Unemployed individuals with a valid occupation code likely reflect their most recent sector
# of employment — this is not considered problematic (384 observations)
table(lfs_data$employed == 0 & !is.na(lfs_data$occup))

# ===============================
# Public/private sector dummy
# ===============================
# Original LFS description:
# PUBLICR - Public or private sector (reported)
# 1 = Private, 2 = Public

# Tabulate original variable
table(lfs_data$PUBLICR, useNA = "always")

# Generate public sector dummy variable
lfs_data <- lfs_data %>%
  mutate(
    public = case_when(
      PUBLICR == 2 ~ 1, # Public sector
      PUBLICR == 1 ~ 0, # Private sector
      TRUE ~ PUBLICR
    ))
  
  # Labour-related variables:
  # observations with "No answer" or "Not applicable" are removed for employed individuals,
  # while unemployed individuals are retained and these values are recoded as NA.
  lfs_data <- lfs_data %>% filter(!(employed == 1 & public %in% c(-8, -9))) %>%
  mutate(public = if_else(public %in% c(-8, -9), NA_real_, as.numeric(public)))

# Label public sector variable
var_label(lfs_data$public) <- "Public sector worker"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-PUBLICR)

# Tabulate original variable
table(lfs_data$public, useNA = "always")

# ========================================
# Highest qualification dummy variables (1/0)
# ========================================
# Original LFS description:
# HIQUL15D - Highest qualification (detailed grouping) — used before 2022
# HIQUL22D - Highest qualification (detailed grouping) — used from 2022 onwards
# 1 = Degree or equivalent, 2 = Higher education, 3 = GCE A level or equivalent,
# 4 = GCSE grades A*-C or equivalent, 5 = Other qualification, 6 = No qualification
# Dropping missing / no answers: -8 = No answer, -9 = Not applicable, 7 = Don't know

# Tabulate original variable
table(lfs_data$HIQUL15D, useNA = "always")
table(lfs_data$HIQUL22D, useNA = "always")


# Combine HIQUL15D and HIQUL22D into single variable
lfs_data <- lfs_data %>%
  mutate(
    hqual = case_when(
      year < 2022 ~ HIQUL15D,
      year >= 2022 ~ HIQUL22D
    )
  )

# Drop original LFS variables
lfs_data <- lfs_data %>% select(-c(HIQUL15D, HIQUL22D))

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable, 7 = Don't know
lfs_data <- lfs_data %>%
  filter(!(hqual %in% c(-8, -9, 7))  & !is.na(hqual))


# Generate qualification dummy variables
lfs_data <- lfs_data %>%
  mutate(
    degree = if_else(is.na(hqual), NA_real_, as.numeric(hqual == 1)),
    higher = if_else(is.na(hqual), NA_real_, as.numeric(hqual == 2)),
    alevel = if_else(is.na(hqual), NA_real_, as.numeric(hqual == 3)),
    gcse   = if_else(is.na(hqual), NA_real_, as.numeric(hqual == 4)),
    other  = if_else(is.na(hqual), NA_real_, as.numeric(hqual == 5)),
    none   = if_else(is.na(hqual), NA_real_, as.numeric(hqual == 6))
  )

# Aggregate variable
lfs_data <- lfs_data %>%
  mutate(
    qualification = case_when(
      hqual == 1 ~ "Degree or equivalent",
      hqual == 2 ~ "Higher education",
      hqual == 3 ~ "A-level or equivalent",
      hqual == 4 ~ "GCSE A*-C or equivalent",
      hqual == 5 ~ "Other qualification",
      hqual == 6 ~ "No qualification",
      TRUE ~ NA_character_
    ),
    qualification = factor(qualification)
  )

var_label(lfs_data$qualification) <- "Highest qualification"

# Label qualification dummy variables
var_label(lfs_data$degree) <- "Degree or equivalent"
var_label(lfs_data$higher) <- "Higher education"
var_label(lfs_data$alevel) <- "GCE, A-level or equivalent"
var_label(lfs_data$gcse) <- "GCSE grades A*-C or equivalent"
var_label(lfs_data$other) <- "Other qualifications"
var_label(lfs_data$none) <- "No qualification"

# Tabulate original variable
table(lfs_data$qualification, useNA = "always")

# ==================================================================
# Age when completed full-time education & Experience approximation
# ==================================================================

# Original LFS description:
# EDAGE - Age when completed full-time education
# 96 = Still in education, 97 = Never had an education
# -8 = No answer, -9 = Not applicable

# Tabulate original variable
table(lfs_data$EDAGE, useNA = "always")
table(lfs_data$AGE, useNA = "always")

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
# Still in education (96) excluded as experience proxy is not applicable
lfs_data <- lfs_data %>%
  filter(!(EDAGE %in% c(-8, -9, 96)) & !is.na(EDAGE))

# Label variable
var_label(lfs_data$EDAGE) <- "Age when completed full-time education"

# Mincer style experience proxy age minus years of education(age finished schooling)
# Generate years of education

lfs_data <- lfs_data %>%
  mutate(
    exper = case_when(
      EDAGE != 97 ~ AGE - EDAGE, # normal case
      EDAGE == 97 ~ AGE - 18,    # "never had education"
    )
  )

# Ensure positive experience
lfs_data <- lfs_data %>%
  filter(exper >= 0)

# Assertion for positive experience
stopifnot(all(lfs_data$exper >= 0))

var_label(lfs_data$exper) <- "Experience approximation"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-EDAGE)

# Tabulate original variable
table(lfs_data$exper, useNA = "always")


# ========================================
# Employment (full time or part-time)
# ========================================
# FTPTWK - Whether full or part time in main job

# (1) Full-time
# (2) Part-time

# Tabulate original variable
table(lfs_data$FTPTWK, useNA = "always")

# Full-time dummy vairable( Full-time:1 & part-time:0)
lfs_data <- lfs_data %>%
  mutate(
    Full_time = case_when(
      FTPTWK == 1 ~ 1,
      FTPTWK == 2 ~ 0,
      TRUE ~ FTPTWK
    )
  ) %>%
  # Labour-related variables:
  # observations with "No answer" or "Not applicable" are removed for employed individuals,
  # while unemployed individuals are retained and these values are recoded as NA.
  filter(!(employed == 1 & Full_time %in% c(-8, -9))) %>%
  mutate(Full_time = if_else(Full_time %in% c(-8, -9), NA_real_, as.numeric(Full_time)))


# Label Full_time variable
var_label(lfs_data$Full_time) <- "Full-time:1 or Part-time/other:0"

# Tabulate new variable
table(lfs_data$Full_time, useNA = "always")

# Unemployed individuals with a valid full-time/part-time code likely reflect their most 
# recent employment status — this is not considered problematic
table(lfs_data$employed == 0 & is.na(lfs_data$Full_time))
table(lfs_data$employed == 0 & !is.na(lfs_data$Full_time))

# ===============================
#  Hourly wages
# ===============================

# Original LFS description:
# PAIDHRU - Paid hours based on usual hours
# 97 = 97 or more hours, -8 = No answer, -9 = Not applicable
# GRSSWK - Gross weekly pay in main job
# -8 = No answer, -9 = Not applicable

# Tabulate original variable
table(lfs_data$PAIDHRU, useNA = "always")
table(lfs_data$GRSSWK, useNA = "always")


# Labour-related variables:
# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
# observations with "No answer" or "Not applicable" are removed for employed individuals,
# while unemployed individuals are retained and these values are recoded as NA.
lfs_data <- lfs_data %>%
  filter(!(employed == 1 & (PAIDHRU %in% c(-8, -9) | is.na(PAIDHRU)))) %>% # Employed treatment
  mutate(PAIDHRU = if_else(PAIDHRU %in% c(-8, -9), NA_real_, as.numeric(PAIDHRU))) %>% # Unemployed treatment
  filter(!(employed == 1 & (GRSSWK %in% c(-8, -9) | is.na(GRSSWK)))) %>% # Employed treatment 
  mutate(GRSSWK = if_else(GRSSWK %in% c(-8, -9), NA_real_, as.numeric(GRSSWK))) # Unemployed treatment

# Generate hourly wage variable
lfs_data <- lfs_data %>%
  mutate(hrpay = case_when(
    GRSSWK > 0 & PAIDHRU > 0 ~ GRSSWK / PAIDHRU,
    TRUE ~ NA_real_
  ))


# Drop if employed and missing wage information
lfs_data <- lfs_data %>%
  filter(!(employed == 1 & is.na(hrpay)))

# Label hourly wage variable
var_label(lfs_data$hrpay) <- "Average hourly pay (gross weekly pay / paid hours)"

# Drop original LFS variables
lfs_data <- lfs_data %>% select(-c(GRSSWK, PAIDHRU))

# Tabulate new variable
table(lfs_data$hrpay, useNA = "always")

# Ensure all employed individuals have positive wage
stopifnot(all(lfs_data$hrpay[lfs_data$employed == 1] >= 0))

# ===============================
# Deflate wage using CPIH
# ===============================
# using ONS CPI index: https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l55o/mm23

# Generate Year and quarter Identifier in current LFS data for merge to CPI index
lfs_data <- lfs_data %>%
  mutate(
    quarter_str = case_when(
      quarter == 1 ~ "Q1",
      quarter == 2 ~ "Q2",
      quarter == 3 ~ "Q3",
      quarter == 4 ~ "Q4"
    ),
    YearQuarter = paste0(year, " ", quarter_str)
  )

# Importing CPIH data
CPIH <- read_xlsx(
  "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data/other inputs/CPIH.xlsx",
  skip = 7
) %>%
  rename(YearQuarter = `Year/Quarter`)


# Merge CPI data to LFS data
lfs_data <- left_join(lfs_data, CPIH, by = "YearQuarter")

# Assert each data point has been allocated a CPI Index value
stopifnot(all(!is.na(lfs_data$Index)))

# Calculating real wages
lfs_data <- lfs_data %>%
  mutate(realhrpay = hrpay * (100 / Index))

var_label(lfs_data$realhrpay) <- "Real hourly pay"

# Logarithmic transformation of wages
lfs_data <- lfs_data %>%
  mutate(loghrp = case_when(
    realhrpay > 0 ~ log(realhrpay),
    TRUE ~ NA_real_
  ))
# Label real wages
var_label(lfs_data$loghrp) <- "Log hourly pay"


# Drop unnecessary/original LFS variables
lfs_data <- lfs_data %>% select(-c(BUSHR, POTHR, quarter_str, YearQuarter))

# Tabulate new variable
table(lfs_data$realhrpay, useNA = "always")
table(lfs_data$loghrp, useNA = "always")


# ===============================
# Company size dummy variable
# ===============================

# MPNR02: Number of employees at workplace
# Categories:
# 1 = 1–10 employees
# 2 = 11–19
# 3 = 20–24
# 4 = Don't know but under 25
# 5 = 25–49
# 6 = 50–249
# 7 = 250–499
# 8 = Don't know but between 50 and 499
# 9 = 500 or more
#
# Special codes:
# -8 = No answer
# -9 = Not applicable

# Tabulate original variable
table(lfs_data$MPNR02, useNA = "always")


# Creating large/medium company dummy variable: defined as above 250 and above as medium/large
lfs_data <- lfs_data %>%
  mutate(
    lcomp = case_when(
      MPNR02 %in% c(7, 8, 9) ~ 1,
      MPNR02 %in% c(1, 2, 3, 4, 5, 6) ~ 0,
      TRUE ~ MPNR02
    ))
  
  # Labour-related variables:
  # Dropping missing / no answers: -8 = No answer, -9 = Not applicable
  # observations with "No answer" or "Not applicable" are removed for employed individuals,
  # while unemployed individuals are retained and these values are recoded as NA.
  lfs_data <- lfs_data %>%  filter(!(employed == 1 & lcomp %in% c(-8, -9))) %>%
  mutate(lcomp = if_else(lcomp %in% c(-8, -9), NA_real_, as.numeric(lcomp)))

# Label comapny size variable
var_label(lfs_data$lcomp) <- "1 if large/medium sized company, 0 if small/micro"

# Drop unnecessary/original LFS variables
lfs_data <- lfs_data %>% select(-c(MPNR02))

# Tabulate new variable
table(lfs_data$lcomp, useNA = "always")

# ========================================
# Disability dummy variable (1/0)
# ========================================
# Original LFS description:
# DISEA - Disability: Equality Act
# 1 = Equality Act Disabled, 2 = Not Equality Act Disabled


# DISCURR - Current disability
# Disability status:
# 1 = Both current disability and work-limiting disability
# 2 = Current disability only
# 3 = Work-limiting disability only
# 4 = Not disabled

# Tabulate original variable
table(lfs_data$DISEA, useNA = "always")


# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
# -9 = Not applicable, does not require being dropped here
lfs_data <- lfs_data %>% filter(!(DISEA %in% -8))
lfs_data <- lfs_data %>% filter(!(DISCURR %in% -8))

# Generate disability dummy variable
lfs_data <- lfs_data %>%
  mutate(
    disabled = case_when(
      DISEA == 1 ~ 1, # Equality Act Disabled
      DISCURR %in% c(1, 2, 3) ~ 1, # Broadly considered disabled
      TRUE ~ 0 # Not Equality Act Disabled)
    )
  )

# Label disability variable
var_label(lfs_data$disabled) <- "Equality Act Disabled (1/0)"

# Assert no missing values
stopifnot(all(!is.na(lfs_data$disabled)))

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-DISEA)
lfs_data <- lfs_data %>% select(-DISCURR)

# Tabulate new variable
table(lfs_data$disabled, useNA = "always")


# ========================================
# Employment tenure (months)
# ========================================
# Original LFS description:
# EMPMON - Length of time continuously employed (including self-employed)

# Tabulate original variable
table(lfs_data$EMPMON, useNA = "always")

# Generate tenure variable
lfs_data <- lfs_data %>%
  mutate(tenure = EMPMON) %>%
  
  # Labour-related variables:
  # observations with "No answer" or "Not applicable" are removed for employed individuals,
  # while unemployed individuals are retained and these values are recoded as NA.
  filter(!(employed == 1 & tenure %in% c(-8, -9))) %>%
  mutate(tenure = if_else(tenure %in% c(-8, -9), NA_real_, as.numeric(tenure)))

# Label employment tenure variable
var_label(lfs_data$tenure) <- "Length of time continuously employed (months)"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-EMPMON)

# Tabulate new variable
table(lfs_data$tenure, useNA = "always")

# ========================================
# Dependent children
# ========================================
# Original LFS description:
# FDPCH19 - Number of dependent children in family aged under 19

# Tabulate original variable
table(lfs_data$FDPCH19, useNA = "always")

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
# -9 = Not applicable, not applicable does not warrant dropping
lfs_data <- lfs_data %>% filter(FDPCH19 != -8, !is.na(FDPCH19))

# Generate dependent children variable
lfs_data <- lfs_data %>%
  mutate(dep19 = case_when(
    FDPCH19 != -9 ~ FDPCH19,
    FDPCH19 == -9 ~ 0
  ))

# Label dependent children variable
var_label(lfs_data$dep19) <- "Number of dependent children aged under 19"

# Drop original LFS dependent children variables
lfs_data <- lfs_data %>% select(-c(FDPCH9, FDPCH4, FDPCH2, FDPCH15, FDPCH16, FDPCH19))

# Tabulate new variable
table(lfs_data$dep19, useNA = "always")

# ========================================
# Age
# ========================================
#  AGE- Age of respondent 
# (0-99) Age of respondent

# Tabulate original variable
table(lfs_data$AGE)

# Generate age variable
lfs_data <- lfs_data %>%
  mutate(age = AGE) %>%
  filter(!is.na(age))

# Label age variable
var_label(lfs_data$age) <- "Age"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-AGE)

# Filter for age above 18
lfs_data <- lfs_data %>% filter(age >= 18)


# ===============================================================================
# IV. Keep and order final variables
# ===============================================================================

# Employed subset of data
lfs_data_employed <- filter(lfs_data, employed == 1)


# Keep only required variables
lfs_data <- lfs_data %>%
  select(
    year, quarter, age, CASENOP, disabled, tenure, dep19, london,
    white, black, mixed, indian, pakistani, bangladeshi, chinese, ethnic,
    bborn, female, marr, manager, professional, associate, administrative,
    skilled, caring, sales, operative, elementary, public,
    degree, higher, alevel, gcse, other, none,
    exper, realhrpay, hrpay, loghrp, lcomp, Index, occup, employed, Full_time, region
  )

# All variables — assert no missing values
core_var <- c(
  "year", "age", "disabled", "tenure", "dep19", "london",
  "white", "black", "mixed", "indian", "pakistani", "bangladeshi", "chinese",
  "bborn", "female", "marr", "manager", "professional", "associate", "administrative",
  "skilled", "caring", "sales", "operative", "elementary", "public",
  "degree", "higher", "alevel", "gcse", "other", "none",
  "exper", "realhrpay", "hrpay", "lcomp", "Index", "occup", "employed", "Full_time"
)

#for (var in core_var) {
 #stopifnot(
  #  all(!is.na(filter(lfs_data, employed == 1)[[var]]))
 # )
#}

#for (var in core_var) {
 # print(table(lfs_data[[var]], useNA = "always"))
#}
# Continuous variables — assert all values are non-negative
#continuous_var <- c(
#  "year", "age", "tenure",
#  "exper", "realhrpay", "hrpay", "Index"
#)

# Assert continuous variables are all positive
#for (var in continuous_var) {
#  stopifnot(all(lfs_data[[var]] >= 0, na.rm = TRUE))
#}


# ========================================
# V. Save cleaned dataset
# ========================================

# Save as RDS (R native format)
saveRDS(lfs_data, file.path(out_dir, "Clean.RDS"))

# Save as Excel
library(writexl)
write_xlsx(lfs_data, file.path(out_dir, "Clean.xlsx"))