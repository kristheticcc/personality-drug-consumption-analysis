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
  
cat("Dimensions: ", nrow(df)," rows and ",ncol(df), " cols\n")
str(df)

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
df_modified <- df

df_modified$age_group <- factor(df_modified$age, levels = age_levels, labels = age_labels,
                                ordered = TRUE)

df_modified$gender_label <- factor(df_modified$gender,levels = c(-0.48246, 0.48246),
                                   labels = c("Female", "Male"))

df_modified$cannabis_user <- ifelse(
  df_modified$cannabis == "CL0" | df_modified$cannabis == "CL1", 
  "Non-User", "User")

df_modified$cannabis_freq <- factor(df_modified$cannabis, levels = drug_levels, 
                                    labels = drug_labels,ordered = TRUE)

cat("Age group counts:\n", table(df_modified$age_group))
cat("Gender counts:\n", table(df_modified$gender_label))
cat("Cannabis user counts:\n", table(df_modified$cannabis_user))
cat("Cannabis frequency:\n", table(df_modified$cannabis_freq))

# =============================================================================
# SECTION 4: EDA (Exploratory Data Analysis)
# =============================================================================
personality_vars <- c("nscore", "escore", "oscore", "ascore", "cscore", 
                      "impulsive", "ss")

personality_summary <- sapply(df_modified[, personality_vars], summary)
personality_table <- t(personality_summary)
cat("Overall Personality Score Summary:\n")
personality_table

cat("Mean Personality Scores by Cannabis User Status:\n")
aggregate(cbind(nscore, escore, oscore, ascore, cscore, impulsive, ss) ~ cannabis_user, 
          data = df_modified, FUN = mean)
cat("SD Personality Scores by Cannabis User Status:\n")
aggregate(cbind(nscore, escore, oscore, ascore, cscore, impulsive, ss) ~ cannabis_user,
          data = df_modified, FUN = sd)

cat("Mean Personality Scores by Gender:\n")
aggregate(cbind(nscore, escore, oscore, ascore, cscore, impulsive, ss) ~ gender_label,
          data = df_modified, FUN = mean)
cat("SD of Personality Scores by Gender:\n")
aggregate(cbind(nscore, escore, oscore, ascore, cscore, impulsive, ss) ~ gender_label,
          data = df_modified, FUN = sd)

cat("Mean Personality Scores by Age Group:\n")
aggregate(cbind(nscore, escore, oscore, ascore, cscore, impulsive, ss) ~ age_group,
          data = df_modified, FUN = mean)
cat("SD Personality Scores by Age Group:\n")
aggregate(cbind(nscore, escore, oscore, ascore, cscore, impulsive, ss) ~ age_group,
          data = df_modified, FUN = sd)

cat("Cannabis Use Frequency Table:\n")
print(table(df_modified$cannabis_freq))
cat("Cannabis User Proportions(%):\n")
print(round(prop.table(table(df_modified$cannabis_user)) * 100, 1))

# All personality scores by cannabis user status
par(mfrow = c(2,4), mar = c(4,4,3,1))
for (var in personality_vars){
  boxplot(df_modified[[var]] ~ df_modified$cannabis_user,
          main = var, xlab = "Cannabis User Status", ylab = "Score",
          col = c("lightblue", "darkorange"))
}
par(mfrow=c(1,1))

# Distribution of each personality score
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
for (var in personality_vars) {
  hist(df_modified[[var]], main = var, xlab = "Score",
       col = "darkgreen", border = "white", breaks = 30)
}
par(mfrow = c(1, 1))

# Cannabis use frequency
barplot(table(df_modified$cannabis_freq),
        main = "Cannabis Use Frequency Distribution",
        xlab = "Frequency Category", ylab = "No. of Respondents",
        col = "red", las = 2)

# All personality scores by gender
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
for (var in personality_vars){
  boxplot(df_modified[[var]] ~ df_modified$gender_label,
          main = var, xlab = "Gender", ylab = "Score",
          col = c("tomato", "steelblue"))
}
par(mfrow = c(1, 1))

# All personality scores by age group
par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
for (var in personality_vars){
  boxplot(df_modified[[var]] ~ df_modified$age_group,
          main = var, xlab = "Age Group", ylab = "Score",
          col = rainbow(6), las = 2)
}
par(mfrow = c(1, 1))

# =============================================================================
# SECTION 5: 
# =============================================================================



