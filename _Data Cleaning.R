# ========================================
# Paths/Library
# ========================================
#Library packages
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
# Region dummy variable (1/0)
# ========================================
# Original LFS description:
# GOVTOF2 - Government Office Region (2 and 3 combined)
# 1 = North East, 2 = North West, 4 = Yorkshire and Humberside,
# 5 = East Midlands, 6 = West Midlands, 7 = East of England,
# 8 = London, 9 = South East, 10 = South West,
# 11 = Wales, 12 = Scotland, 13 = Northern Ireland

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>% filter(GOVTOF2 > 0 & !is.na(GOVTOF2))

# Generate London residency dummy variable
lfs_data <- lfs_data %>%
  mutate(London = ifelse(GOVTOF2 == 8, 1, 0))

# Label London residency variable
var_label(lfs_data$London) <- "London residency (1/0)"

# Assert no missing values in London
stopifnot(all(!is.na(lfs_data$London)))

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-GOVTOF2)

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
lfs_data <- lfs_data %>% filter(ETHUKEUL > 0 & !is.na(ETHUKEUL))

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

# Summarise ethnicity
table(lfs_data$ethnic)

# Drop rows where ethnicity is missing
lfs_data <- lfs_data %>% filter(!is.na(ethnic))

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-ETHUKEUL)


# ========================================
# British-born dummy variable (1/0)
# ========================================
# Original LFS description:
# CRY12 - Country of Birth
# 921 = England, 922 = Northern Ireland, 923 = Scotland,
# 924 = Wales, 926 = UK/Britain (Did not know country)

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>% filter(CRY12 > 0 & !is.na(CRY12))

# Generate british-born dummy variable
lfs_data <- lfs_data %>%
  mutate(
    bborn = case_when(
      CRY12 %in% c(921, 922, 923, 924, 926) ~ 1, # British born
      TRUE ~ 0 # Everything else 0
    )
  )

# Label british-born variable
var_label(lfs_data$bborn) <- "British born individual (1/0)"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-CRY12)


# ========================================
# Female dummy variable (1/0)
# ========================================
# Original LFS description:
# SEX[Sex of respondent (Values information: 1.0 = Male, 2.0 = Female)]

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>% filter(SEX > 0 & !is.na(SEX))

# Generate Female dummy variable (1: Female | 0: Male)
lfs_data <- lfs_data %>%
  mutate(female = ifelse(SEX == 2.0, 1, 0))

# Label Sex variable
var_label(lfs_data$female) <- "Female (1: Female | 0: Male)"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-SEX)


# ======================================
# Relationship status dummy variable
# ======================================
# Original LFS description:
# MARDY6 - married/Co-habiting/Civil Partners
# 1 = married/Cohabiting/Civil Partner, 2 = Non marr,

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>% filter(MARDY6 > 0 & !is.na(MARDY6))

# Cleaning marriage variable
lfs_data <- lfs_data %>%
  mutate(
    marr = case_when(
      MARDY6 == 2 ~ 0,
      MARDY6 == 1 ~ 1,
      TRUE ~ NA_real_
    )
  )

# Label Marriage variable
var_label(lfs_data$marr) <- "married/cohabiting/civil partner"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-MARDY6)

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

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>%
  filter(
    (year < 2021 & SC10MMJ > 0 & !is.na(SC10MMJ)) |
      (year >= 2021 & SC20MMJ > 0 & !is.na(SC20MMJ))
  )

# Generate aggregated occupation variable
lfs_data <- lfs_data %>%
  mutate(
    occup = case_when(
      year < 2021 ~ SC10MMJ,
      year >= 2021 ~ SC20MMJ,
      TRUE ~ NA_real_
    )
  )

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-c(SC10MMJ, SC20MMJ))

# generate individual occupation dummies
lfs_data <- lfs_data %>%
  mutate(
    manager = ifelse(occup == 1, 1, 0),
    professional = ifelse(occup == 2, 1, 0),
    associate = ifelse(occup == 3, 1, 0),
    administrative = ifelse(occup == 4, 1, 0),
    skilled = ifelse(occup == 5, 1, 0),
    caring = ifelse(occup == 6, 1, 0),
    sales = ifelse(occup == 7, 1, 0),
    operative = ifelse(occup == 8, 1, 0),
    elementary = ifelse(occup == 9, 1, 0)
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


# ===============================
# Public/private sector dummy
# ===============================
# Original LFS description:
# PUBLICR - Public or private sector (reported)
# 1 = Private, 2 = Public

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>% filter(PUBLICR > 0 & !is.na(PUBLICR))

# Generate public sector dummy variable
lfs_data <- lfs_data %>%
  mutate(
    public = case_when(
      PUBLICR == 2 ~ 1, # Public sector
      PUBLICR == 1 ~ 0, # Private sector
      TRUE ~ NA_real_
    )
  )

# Label public sector variable
var_label(lfs_data$public) <- "Public sector worker"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-PUBLICR)


# ========================================
# Highest qualification dummy variables (1/0)
# ========================================
# Original LFS description:
# HIQUL15D - Highest qualification (detailed grouping) — used before 2022
# HIQUL22D - Highest qualification (detailed grouping) — used from 2022 onwards
# 1 = Degree or equivalent, 2 = Higher education, 3 = GCE A level or equivalent,
# 4 = GCSE grades A*-C or equivalent, 5 = Other qualification, 6 = No qualification
# Dropping missing / no answers: -8 = No answer, -9 = Not applicable, 7 = Don't know

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
lfs_data <- lfs_data %>% filter(hqual > 0 & hqual != 7 & !is.na(hqual))

# Generate qualification dummy variables
lfs_data <- lfs_data %>%
  mutate(
    degree = ifelse(hqual == 1, 1, 0),
    higher = ifelse(hqual == 2, 1, 0),
    alevel = ifelse(hqual == 3, 1, 0),
    gcse   = ifelse(hqual == 4, 1, 0),
    other  = ifelse(hqual == 5, 1, 0),
    none   = ifelse(hqual == 6, 1, 0)
  )

# Label qualification dummy variables
var_label(lfs_data$degree) <- "Degree or equivalent"
var_label(lfs_data$higher) <- "Higher education"
var_label(lfs_data$alevel) <- "GCE, A-level or equivalent"
var_label(lfs_data$gcse) <- "GCSE grades A*-C or equivalent"
var_label(lfs_data$other) <- "Other qualifications"
var_label(lfs_data$none) <- "No qualification"


# ==================================================================
# Age when completed full-time education & Experience approximation
# ==================================================================

# Original LFS description:
# EDAGE - Age when completed full-time education
# 96 = Still in education, 97 = Never had an education
# Dropping missing / no answers: -8 = No answer, -9 = Not applicable

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>%
  filter(EDAGE >= 0 & EDAGE <= 95 & !is.na(EDAGE))

# Label variable
var_label(lfs_data$EDAGE) <- "Age when completed full-time education"

# Mincer style experience proxy age minus years of education(age finished schooling minus preschool years)
# Generate years of education

lfs_data <- lfs_data %>% mutate(exper = AGE - EDAGE - 5)

lfs_data <- lfs_data %>%
  filter(exper >= 0)

# Assertion for positive experience
stopifnot(lfs_data$exper >= 0)

var_label(lfs_data$exper) <- "Experience approximation"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-EDAGE)

# ===============================
#  Hourly wages
# ===============================

# Original LFS description:
# PAIDHRU - Paid hours based on usual hours
# 97 = 97 or more hours, -8 = No answer, -9 = Not applicable
# GRSSWK - Gross weekly pay in main job
# -8 = No answer, -9 = Not applicable
# Dropping missing / no answers: -8 = No answer, -9 = Not applicable

# Drop invalid values for hours and wages
lfs_data <- lfs_data %>%
  filter(GRSSWK > 0 & PAIDHRU > 0)

# Generate hourly wage variable
lfs_data <- lfs_data %>%
  mutate(hrpay = GRSSWK / PAIDHRU)

# Drop negative hourly pay
lfs_data <- lfs_data %>%
  filter(hrpay >= 0)

# Label hourly wage variable
var_label(lfs_data$hrpay) <- "Average hourly pay (gross weekly pay / paid hours)"

# Drop original LFS variables
lfs_data <- lfs_data %>% select(-c(GRSSWK, PAIDHRU))

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
lfs_data <- full_join(lfs_data, CPIH, by = "YearQuarter")

# Assert each data point has been allocated a CPI Index value
stopifnot(!is.na(lfs_data$Index))

# Calculating real wages
lfs_data <- lfs_data %>%
  mutate(realhrpay = hrpay * (100 / Index))

var_label(lfs_data$realhrpay) <- "Real hourly pay"

# Logarithmic transformation of wages
lfs_data <- lfs_data %>%
  mutate(loghrp = log(lfs_data$realhrpay))

# Label real wages
var_label(lfs_data$loghrp) <- "Log hourly pay"


# Drop unnecessary/original LFS variables
lfs_data <- lfs_data %>% select(-c(BUSHR, POTHR, quarter_str, YearQuarter))


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

# Drop invalid values for hours and wages
lfs_data <- lfs_data %>%
  filter(MPNR02 > 0 & MPNR02 > 0)

# Creating large/medium company dummy variable: defined as above 250 and above as medium/large
lfs_data <- lfs_data %>%
  mutate(lcomp = case_when(
    MPNR02 %in% c(1, 2, 3, 4, 5, 6) ~ 0,
    MPNR02 %in% c(7, 8, 9) ~ 1
  ))

# Label comapny size variable
var_label(lfs_data$lcomp) <- "1 if large/medium sized company, 0 if small/micro"

# Drop unnecessary/original LFS variables
lfs_data <- lfs_data %>% select(-c(MPNR02))


# ========================================
# Disability dummy variable (1/0)
# ========================================
# Original LFS description:
# DISEA - Disability: Equality Act
# 1 = Equality Act Disabled, 2 = Not Equality Act Disabled

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>% filter(DISEA > 0 & !is.na(DISEA))


# Generate disability dummy variable
lfs_data <- lfs_data %>%
  mutate(
    disabled = case_when(
      DISEA == 1 ~ 1, # Equality Act Disabled
      DISEA == 2 ~ 0, # Not Equality Act Disabled
      TRUE ~ NA_real_
    )
  )

# Label disability variable
var_label(lfs_data$disabled) <- "Equality Act Disabled (1/0)"

# Assert no missing values
stopifnot(all(!is.na(lfs_data$disabled)))

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-DISEA)


# ========================================
# Employment tenure (months)
# ========================================
# Original LFS description:
# EMPMON - Length of time continuously employed (including self-employed)

# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>% filter(EMPMON > 0 & !is.na(EMPMON) & EMPMON >= 0)

# Generate tenure variable

lfs_data <- lfs_data %>% mutate(tenure = EMPMON)

# Label employment tenure variable
var_label(lfs_data$tenure) <- "Length of time continuously employed (months)"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-EMPMON)


# ========================================
# Dependent children
# ========================================
# Original LFS description:
# FDPCH19 - Number of dependent children in family aged under 19
# Dropping missing / no answers: -8 = No answer, -9 = Not applicable
lfs_data <- lfs_data %>% filter(FDPCH19 >= 0 & !is.na(FDPCH19))

# Generate dependent children variable
lfs_data <- lfs_data %>%
  mutate(dep19 = FDPCH19)

# Label dependent children variable
var_label(lfs_data$dep19) <- "Number of dependent children aged under 19"

# Drop original LFS dependent children variables
lfs_data <- lfs_data %>% select(-c(FDPCH9, FDPCH4, FDPCH2, FDPCH15, FDPCH16, FDPCH19))


# ========================================
# Age
# ========================================
# Generate age variable
lfs_data <- lfs_data %>%
  mutate(age = AGE) %>%
  filter(!is.na(age))

# Label age variable
var_label(lfs_data$age) <- "Age"

# Drop original LFS variable
lfs_data <- lfs_data %>% select(-AGE)


# ===============================================================================
# IV. Keep and order final variables
# ===============================================================================

# Keep only required variables
lfs_data <- lfs_data %>%
  select(
    year, quarter, age, CASENOP, disabled, tenure, dep19, ILODEFR, London,
    white, black, mixed, indian, pakistani, bangladeshi, chinese, ethnic,
    bborn, female, marr, manager, professional, associate, administrative,
    skilled, caring, sales, operative, elementary, public,
    degree, higher, alevel, gcse, other, none,
    exper, realhrpay, hrpay, loghrp, lcomp, Index, occup
  )

# All variables — assert no missing values
core_var <- c(
  "year", "age", "ILODEFR", "disabled", "tenure", "dep19", "London",
  "white", "black", "mixed", "indian", "pakistani", "bangladeshi", "chinese",
  "bborn", "female", "marr", "manager", "professional", "associate", "administrative",
  "skilled", "caring", "sales", "operative", "elementary", "public",
  "degree", "higher", "alevel", "gcse", "other", "none",
  "exper", "realhrpay", "hrpay", "lcomp", "Index", "occup"
)

for (var in core_var) {
  stopifnot(all(!is.na(lfs_data[[var]])))
}

# Continuous variables — assert all values are non-negative
continuous_var <- c(
  "year", "age", "ILODEFR", "tenure",
  "exper", "realhrpay", "hrpay", "Index", "occup"
)

# Assert core variables are all positive
for (var in core_var) {
  stopifnot(all(lfs_data[[var]] >= 0))
}


# ========================================
# V. Save cleaned dataset
# ========================================

# Save as RDS (R native format)
saveRDS(lfs_data, file.path(out_dir, "Clean.RDS"))

# Save as Excel
library(writexl)
write_xlsx(lfs_data, file.path(out_dir, "Clean.xlsx"))