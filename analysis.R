###############################################################################
# MATH 167R - Final Project
# Drug Consumption & Personality Traits Analysis
# Group P: Kanishka Yadav, Krish Makwana, Eric Nguyen, Ramjot Dhillon
# Spring 2026 
# Instructor: Andrea Gottlieb
###############################################################################

# =============================================================================
# SECTION 1: Load Libraries
# =============================================================================
# install.packages("tidyverse")
# install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

# =============================================================================
# SECTION 2: Load & Rename Columns
# =============================================================================
df <- read.csv("data/drug_consumption.csv", header = FALSE)

colnames(df) <- c(
  "id", "age", "gender", "education", "country", "ethnicity", "nscore", "escore",    
  "oscore", "ascore", "cscore", "impulsive", "ss", "alcohol", "amphet", "amyl",
  "benzos", "caff", "cannabis", "choc", "coke", "crack", "ecstasy", "heroin", 
  "ketamine", "legalh", "lsd", "meth", "mushrooms", "nicotine", "semer", "vsa"
)
  
cat("Dimensions: ", nrow(df)," rows ",ncol(df), " cols\n")
str(df)
summary(df)
head(df)

# =============================================================================
# SECTION 3: Data Cleaning & Preparation
# =============================================================================

# Age bracket levels and labels
age_levels <- c(-0.95197, -0.07854, 0.49788, 1.09449, 1.82213, 2.59171)
age_labels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")


# Drug use frequency levels and labels
drug_levels <- c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6")
drug_labels <- c("Never", ">10 yrs ago", "Last decade", "Last year", 
                 "Last month", "Last week", "Last day")

# Modified df with derived variables
df_modified <- df%>%mutate(
  age_group = factor(age, levels=age_levels, labels=age_labels, ordered=TRUE),
  gender_label = factor(gender, levels=c(-0.48246, 0.48246), labels=c("Female","Male")),
  cannabis_user = ifelse(cannabis %in% c("CL0", "CL1"), "Non-User", "User"),
  cannabis_freq = factor(cannabis, levels=drug_levels, labels=drug_labels, ordered=TRUE)
)

cat("Age group counts:\n", table(df_modified$age_group))
cat("Gender counts:\n", table(df_modified$gender_label))
cat("\nCannabis user counts:\n", table(df_modified$cannabis_user))
cat("\nCannabis frequency:\n", table(df_modified$cannabis_freq))

str(df_modified)
summary(df_modified)
head(df_modified)


