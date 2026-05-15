###############################################################################
# MATH 167R - Final Project
# Drug Consumption & Personality Traits Analysis
# Group P: Kanishka Yadav, Krish Makwana, Eric Nguyen, Ramjot Dhillon
# Spring 2026
# Instructor: Andrea Gottlieb
###############################################################################

# ---------------------------------------------------------------------------
# 1. Load libraries
# ---------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------------------------------------------------------
# 2. Load the dataset
# ---------------------------------------------------------------------------

df <- read.csv("data/drug_consumption.csv", header = FALSE)


# ---------------------------------------------------------------------------
# 3. Rename the columns
# ---------------------------------------------------------------------------

colnames(df) <- c(
  "id", "age", "gender", "education", "country", "ethnicity",
  "nscore", "escore", "oscore", "ascore", "cscore", "impulsive", "ss",
  "alcohol", "amphet", "amyl", "benzos", "caff", "cannabis", "choc",
  "coke", "crack", "ecstasy", "heroin", "ketamine", "legalh", "lsd",
  "meth", "mushrooms", "nicotine", "semer", "vsa"
)


# ---------------------------------------------------------------------------
# 4. Check the dataset
# ---------------------------------------------------------------------------

cat("Dimensions:", nrow(df), "rows and", ncol(df), "columns\n")

head(df)
str(df)


# ---------------------------------------------------------------------------
# 5. Convert drug consumption classes into ordered categories
# ---------------------------------------------------------------------------
# CL0 = Never Used
# CL1 = Used over a Decade Ago
# CL2 = Used in Last Decade
# CL3 = Used in Last Year
# CL4 = Used in Last Month
# CL5 = Used in Last Week
# CL6 = Used in Last Day

drug_cols <- c(
  "alcohol", "amphet", "amyl", "benzos", "caff", "cannabis",
  "choc", "coke", "crack", "ecstasy", "heroin", "ketamine",
  "legalh", "lsd", "meth", "mushrooms", "nicotine", "semer", "vsa"
)

df[drug_cols] <- lapply(
  df[drug_cols],
  factor,
  levels = c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6"),
  ordered = TRUE
)


# ---------------------------------------------------------------------------
# 6. Boxplot: Impulsiveness by cannabis consumption
# ---------------------------------------------------------------------------

ggplot(df, aes(x = cannabis, y = impulsive)) +
  geom_boxplot() +
  labs(
    title = "Impulsiveness by Cannabis Consumption",
    x = "Cannabis Consumption Class",
    y = "Impulsiveness Score"
  )


# ---------------------------------------------------------------------------
# 7. Boxplot: Sensation seeking by nicotine consumption
# ---------------------------------------------------------------------------

ggplot(df, aes(x = nicotine, y = ss)) +
  geom_boxplot() +
  labs(
    title = "Sensation Seeking by Nicotine Consumption",
    x = "Nicotine Consumption Class",
    y = "Sensation Seeking Score"
  ) 

# ---------------------------------------------------------------------------
# 8. Density plot: Neuroticism score
# ---------------------------------------------------------------------------

ggplot(df, aes(x = nscore)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(
    title = "Density Plot of Neuroticism Score",
    x = "Neuroticism Score",
    y = "Density"
  )


# ---------------------------------------------------------------------------
# 9. Density plot: Impulsiveness by cannabis consumption
# ---------------------------------------------------------------------------

ggplot(df, aes(x = impulsive, fill = cannabis)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Density of Impulsiveness by Cannabis Consumption",
    x = "Impulsiveness Score",
    y = "Density",
    fill = "Cannabis Class"
  ) 


# ---------------------------------------------------------------------------
# 10. Bar chart: Cannabis consumption distribution
# ---------------------------------------------------------------------------

ggplot(df, aes(x = cannabis)) +
  geom_bar() +
  labs(
    title = "Cannabis Consumption Class Distribution",
    x = "Cannabis Consumption Class",
    y = "Count"
  ) 


# ---------------------------------------------------------------------------
# 11. Correlation heatmap for personality variables
# ---------------------------------------------------------------------------
personality_cols <- c(
  "nscore", "escore", "oscore", "ascore",
  "cscore", "impulsive", "ss"
)

cor_matrix <- cor(df[, personality_cols], use = "complete.obs")

cor_df <- as.data.frame(as.table(cor_matrix))

ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 4) +
  scale_fill_gradient2(
    low = "deepskyblue3",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Heatmap of Personality Variables",
    x = "",
    y = "",
    fill = "Correlation"
  ) 


