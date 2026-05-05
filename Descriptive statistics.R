
#======================================================  
 # DESCRIPTIVE STATISTICS BY ETHNIC GROUP
#======================================================
#Library packages
library(tidyverse)
library(labelled)
library(haven)
library(writexl)
library(tidyr)
library(RColorBrewer)

# ========================================
# Paths
# ========================================

clean_dir <- "C:/Users/sndui/OneDrive/Dissertation/Dissertation project/3. Input data/Cleaned data"

lfs_data <- readRDS(file.path(clean_dir, "Clean.RDS"))


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
  summarise(mean_age = mean(age, na.rm = TRUE)) %>%
  
  ggplot(data = ., aes(
    x = reorder(ethnic, mean_age),  # THIS does the ordering
    y = mean_age,
    fill = ethnic
  )) +
  
  geom_col() +
  
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    title = "Mean Age by Ethnic Group",
    x = "Ethnic Group",
    y = "Mean Age", fill= NULL
  ) +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#=====================================================
  # 5. Immigration status by ethnicity
#=====================================================

ggplot(lfs_data, aes(x = ethnic, fill = factor(bborn))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("0" = "tomato", "1" = "steelblue"),
                    labels = c("0" = "Not British-born", "1" = "British-born")) +
  
  labs(
    title = "British-born Status by Ethnic Group",
    x = "Ethnic Group",
    y = "Percentage", fill=NULL
  ) +  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )


#=====================================================
  # 6. Tenure by ethnicity
#=====================================================
  
library(RColorBrewer)

lfs_data %>% 
  group_by(ethnic) %>%
  summarise(mean_tenure = mean(tenure, na.rm = TRUE)) %>% 
  ggplot(aes(x = ethnic, y = mean_tenure, fill = ethnic)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Tenure by ethnic group",
    x = "Ethnic group",
    y = "Tenure (Months)",
    fill = "Ethnicity"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )



#======================================================
  # 7. Number of dependent children by ethnicity
#======================================================
  lfs_data %>%
  group_by(ethnic) %>%
  summarise(mean_dep19 = mean(dep19, na.rm = TRUE)) %>%
  
  ggplot(aes(
    x = reorder(ethnic, mean_dep19),
    y = mean_dep19,
    fill = ethnic
  )) +
  
  geom_col() +
  
  geom_text(
    aes(label = round(mean_dep19, 2)),
    vjust = -0.3
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  
  labs(
    title = "Number of dependent kids (≤19) by ethnic group",
    x = "Ethnic group",
    y = "Mean dependent kids (≤19)",
    fill = "Ethnicity"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"
  )

  # stat_summary(fun = mean)
  # adds group means without pre-calculating them
  
#======================================================
  # 8. Education by ethnicity
#======================================================  
ggplot(
  lfs_data %>%
    mutate(
      qual = factor(
        case_when(
          degree == 1 ~ 1,
          higher  == 1 ~ 2,
          alevel  == 1 ~ 3,
          gcse    == 1 ~ 4,
          other   == 1 ~ 5,
          none    == 1 ~ 6
        ),
        levels = 1:6,
        labels = c(
          "Degree or equivalent",
          "Higher education",
          "A-level or equivalent",
          "GCSE A*-C or equivalent",
          "Other qualifications",
          "No qualifications"
        )
      )
    ),
  aes(x = ethnic, fill = qual)
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Qualification distribution by ethnic group",
    x = "Ethnic group",
    y = "Percentage",
    fill = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#=====================================================
  # 9. Marital status by ethnicity
#=====================================================
  
lfs_data %>%
  mutate(
    marr = factor(marr,
                  levels = c(0, 1),
                  labels = c("Non-married", "Married/cohabiting/civil partner")
    )
  ) %>%
  ggplot(aes(x = ethnic, fill = marr)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Marital status by ethnicity",
    x = "Ethnic group",
    y = "Percentage",
    fill = "Marital status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#=====================================================
# 10. Occupation
#=====================================================

lfs_data %>%
  mutate(
    occup = factor(occup,
                   levels = 1:9,
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
  ) %>%
  ggplot(aes(x = ethnic, fill = occup)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Occupation distribution by ethnicity",
    x = "Ethnic group",
    y = "Percentage",
    fill = "Occupation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
