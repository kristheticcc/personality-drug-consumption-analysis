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
# SECTION 5: Advanced Statistical Analysis
# =============================================================================

#-------------------------------------------------------------------------------
# 5A: Two-Sample T-Tests
# Compare mean personality scores between cannabis users and non-users
# HO: mean score is equal between users and non-users
# Ha: mean score differs between users and non-users
#-------------------------------------------------------------------------------
cat("=== Two-Sample T-Tests: Cannabis Users vs Non-Users ===\n\n")
for(var in personality_vars) {
  users <- df_modified[[var]][df_modified$cannabis_user=="User"]
  non_users <- df_modified[[var]][df_modified$cannabis_user=="Non-User"]
  
  result <- t.test(users, non_users)
  
  cat("Variable: ", var, "\n")
  cat(" t=", round(result$statistic, 3),
      "| df=", round(result$parameter, 1),
      "| p-value=", round(result$p.value, 4),
      "\n")
  if(result$p.value < 0.05) {
    cat(" --> Reject HO: Significant difference\n\n")
  } else{
    cat(" --> Fail to reject HO: no significant difference\n\n")
  }
}

#-------------------------------------------------------------------------------
# 5B: Chi-Square Tests of Independence
# Test whether cannabis user status is independent of gender and age group
# HO: cannabis user status and [gender/age group] are independent
# Ha: they are not independent
#-------------------------------------------------------------------------------
cat("=== Chi-Square Test: Cannabis User Status vs Gender ===\n")
gender_table <- table(df_modified$cannabis_user,
                      df_modified$gender_label)
print(gender_table)
chisq_gender <- chisq.test(gender_table)
print(chisq_gender)
if(chisq_gender$p.value < 0.05) {
  cat(" --> Reject HO: cannabis use and gender are not independent\n\n")
} else{
  cat(" --> Fail to reject HO: no significant association\n\n")
}

cat("=== Chi-Square Test: Cannabis User Status vs Age Group ===\n")
age_table <- table(df_modified$cannabis_user,
                      df_modified$age_group)
print(age_table)
chisq_age <- chisq.test(age_table)
print(chisq_age)
if(chisq_age$p.value < 0.05) {
  cat(" --> Reject HO: cannabis use and age group are not independent\n\n")
} else{
  cat(" --> Fail to reject HO: no significant association\n\n")
}

#-------------------------------------------------------------------------------
# 5C: One-Way ANOVA + Tukey HSD
# Test whether sensation seeking scores differ across cannabis frequency groups
# HO: mean SS score is equal across all cannabis frequency groups
# Ha: at least one group mean differs
#-------------------------------------------------------------------------------
cat("=== One-Way ANOVA: Sensation Seeking by Cannabis Frequency ===\n")
anova_model <- aov(ss ~cannabis_freq, data=df_modified)
print(summary(anova_model))

anova_summary <- summary(anova_model)[[1]]
p_anova <- anova_summary$`Pr(>F)`[1]

if(p_anova < 0.05) {
  cat(" --> Reject HO: SS scores differ significantly across frequency groups\n")
  cat("\n=== Tukey HSD Post-Hoc Test ===\n")
  tukey_result <- TukeyHSD(anova_model)
  print(tukey_result)
} else{
  cat(" --> Fail to reject HO: no significant difference across groups\n")
}

#-------------------------------------------------------------------------------
# 5D: Logistic Regression
# Predict cannabis user status from 7 personality scores
# HO: personality traits do not predict cannabis use
# Ha: at least one personality trait significantly predicts cannabis use
#-------------------------------------------------------------------------------
# Convert cannabis_user to binary (1=User, 0=Non-User)
df_modified$cannabis_binary <- ifelse(df_modified$cannabis_user=="User", 1, 0)

cat("=== Logistic Regression: Prediction Cannabis Use ===\n")
log_model <- glm(cannabis_binary ~ nscore + escore + oscore + 
                   ascore + cscore + impulsive + ss, data=df_modified, 
                 family = binomial())
print(summary(log_model))

# Odds ratio
cat("\n=== Odds Ratios ===\n")
print(round(exp(coef(log_model)), 4))

# VIF check for multicollinearity
# install.packages("car")
library(car)
cat("\n=== Variance Inflation Factors (VIF) ===\n")
vif_vals <- vif(log_model)
print(round(vif_vals, 3))
if(any(vif_vals > 5)) {
  cat(" --> Warning: Some predictors have VIF > 5, multicollinearity may be present\n")
} else{
  cat(" --> All VIF values acceptable (<5), no serious multicollinearity\n")
}