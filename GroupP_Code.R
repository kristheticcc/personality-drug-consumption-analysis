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
# =============================================================================
# SECTION 6: Visualizations
# =============================================================================

library(scales)
library(GGally)

plot_dir <- "plots"
dir.create(plot_dir, showWarnings = FALSE)


df_viz <- df_modified

# -----------------------------------------------------------------------------
# Color settings
# -----------------------------------------------------------------------------

single_bar_color <- "#4E79A7"
boxplot_color <- "#A6CEE3"
violin_color <- "#74A9CF"
recent_color <- "#2B8CBE"
never_color <- "#E15759"

class_colors <- c(
  "Never used"      = "#D9D9D9",
  "Over decade ago" = "#C6DBEF",
  "Last decade"     = "#9ECAE1",
  "Last year"       = "#6BAED6",
  "Last month"      = "#4292C6",
  "Last week"       = "#2171B5",
  "Last day"        = "#084594"
)

group_colors <- c(
  "Never Used" = "#BDBDBD",
  "Past User" = "#74A9CF",
  "Recent User" = "#E15759"
)

recent_group_colors <- c(
  "Non-Recent User" = "#4E79A7",
  "Recent User" = "#E15759"
)

profile_colors <- c(
  "Never Users" = "#BDBDBD",
  "Recent Users" = "#4E79A7",
  "Daily Users" = "#E15759"
)

heatmap_low <- "#2166AC"
heatmap_mid <- "white"
heatmap_high <- "#B2182B"

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.subtitle = element_text(size = 11),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
)

save_plot <- function(plot, file_name, width = 8, height = 5) {
  ggsave(
    filename = file.path(plot_dir, file_name),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
}

# -----------------------------------------------------------------------------
# Labels and variable groups
# -----------------------------------------------------------------------------

personality_cols <- c(
  "nscore", "escore", "oscore", "ascore",
  "cscore", "impulsive", "ss"
)

personality_labels <- c(
  nscore = "Neuroticism",
  escore = "Extraversion",
  oscore = "Openness",
  ascore = "Agreeableness",
  cscore = "Conscientiousness",
  impulsive = "Impulsiveness",
  ss = "Sensation Seeking"
)

drug_cols <- c(
  "alcohol", "amphet", "amyl", "benzos", "caff", "cannabis",
  "choc", "coke", "crack", "ecstasy", "heroin", "ketamine",
  "legalh", "lsd", "meth", "mushrooms", "nicotine", "semer", "vsa"
)

drug_name_labels <- c(
  alcohol = "Alcohol",
  amphet = "Amphetamines",
  amyl = "Amyl Nitrite",
  benzos = "Benzodiazepines",
  caff = "Caffeine",
  cannabis = "Cannabis",
  choc = "Chocolate",
  coke = "Cocaine",
  crack = "Crack",
  ecstasy = "Ecstasy",
  heroin = "Heroin",
  ketamine = "Ketamine",
  legalh = "Legal Highs",
  lsd = "LSD",
  meth = "Methadone",
  mushrooms = "Mushrooms",
  nicotine = "Nicotine",
  semer = "Semer",
  vsa = "VSA"
)

drug_levels <- c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6")

drug_class_labels <- c(
  CL0 = "Never used",
  CL1 = "Over decade ago",
  CL2 = "Last decade",
  CL3 = "Last year",
  CL4 = "Last month",
  CL5 = "Last week",
  CL6 = "Last day"
)

recent_levels <- c("CL3", "CL4", "CL5", "CL6")

# -----------------------------------------------------------------------------
# Prepare visualization data
# -----------------------------------------------------------------------------

df_viz[personality_cols] <- lapply(df_viz[personality_cols], as.numeric)

df_viz[drug_cols] <- lapply(
  df_viz[drug_cols],
  factor,
  levels = drug_levels,
  ordered = TRUE
)

make_use_group <- function(x) {
  case_when(
    as.character(x) == "CL0" ~ "Never Used",
    as.character(x) %in% c("CL1", "CL2") ~ "Past User",
    as.character(x) %in% recent_levels ~ "Recent User",
    TRUE ~ NA_character_
  )
}

for (drug in drug_cols) {
  df_viz[[paste0(drug, "_num")]] <- as.integer(df_viz[[drug]]) - 1
  
  df_viz[[paste0(drug, "_group")]] <- factor(
    make_use_group(df_viz[[drug]]),
    levels = c("Never Used", "Past User", "Recent User")
  )
  
  df_viz[[paste0(drug, "_recent")]] <- factor(
    ifelse(df_viz[[drug]] %in% recent_levels, "Recent User", "Non-Recent User"),
    levels = c("Non-Recent User", "Recent User")
  )
}

drug_long <- df_viz %>%
  select(id, all_of(drug_cols)) %>%
  pivot_longer(
    cols = all_of(drug_cols),
    names_to = "drug",
    values_to = "class"
  ) %>%
  mutate(
    drug_label = factor(drug_name_labels[drug], levels = drug_name_labels[drug_cols]),
    class = factor(class, levels = drug_levels, ordered = TRUE),
    class_label = factor(
      drug_class_labels[as.character(class)],
      levels = drug_class_labels[drug_levels],
      ordered = TRUE
    ),
    class_num = as.integer(class) - 1,
    use_group = factor(make_use_group(class), levels = c("Never Used", "Past User", "Recent User"))
  )

# -----------------------------------------------------------------------------
# 6A. Drug-use distribution plots
# -----------------------------------------------------------------------------

for (d in drug_cols) {
  p <- drug_long %>%
    filter(drug == d) %>%
    ggplot(aes(x = class_label)) +
    geom_bar(fill = single_bar_color) +
    labs(
      title = paste("Consumption Class Distribution:", drug_name_labels[d]),
      x = "Consumption Class",
      y = "Number of Participants"
    ) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
  
  save_plot(p, paste0("bar_each_drug_", d, ".png"))
}

p_grouped <- ggplot(drug_long, aes(x = drug_label, fill = class_label)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = class_colors) +
  labs(
    title = "Drug-Use Class Counts Across All Drugs",
    x = "Drug",
    y = "Number of Participants",
    fill = "Consumption Class"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_grouped, "grouped_bar_all_drugs.png", width = 12, height = 6)

p_stacked <- ggplot(drug_long, aes(x = drug_label, fill = class_label)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = class_colors) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proportion of Drug-Use Classes Across All Drugs",
    x = "Drug",
    y = "Percentage of Participants",
    fill = "Consumption Class"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_stacked, "stacked_100_bar_all_drugs.png", width = 12, height = 6)

# -----------------------------------------------------------------------------
# 6B. Boxplots and violin plots
# -----------------------------------------------------------------------------

plot_pairs <- tibble::tribble(
  ~drug,       ~trait,       ~trait_label,
  "cannabis",  "oscore",     "Openness",
  "cannabis",  "ss",         "Sensation Seeking",
  "cannabis",  "impulsive",  "Impulsiveness",
  "ecstasy",   "ss",         "Sensation Seeking",
  "coke",      "impulsive",  "Impulsiveness",
  "nicotine",  "nscore",     "Neuroticism",
  "alcohol",   "escore",     "Extraversion",
  "heroin",    "cscore",     "Conscientiousness",
  "lsd",       "oscore",     "Openness",
  "mushrooms", "oscore",     "Openness"
)

make_boxplot <- function(drug, trait, trait_label) {
  p <- ggplot(df_viz, aes(x = .data[[drug]], y = .data[[trait]])) +
    geom_boxplot(fill = boxplot_color, color = "#333333") +
    labs(
      title = paste(trait_label, "by", drug_name_labels[drug], "Consumption Class"),
      x = paste(drug_name_labels[drug], "Class"),
      y = paste(trait_label, "Score")
    )
  
  save_plot(p, paste0("boxplot_", drug, "_", trait, ".png"))
}

make_violin <- function(drug, trait, trait_label) {
  p <- ggplot(df_viz, aes(x = .data[[drug]], y = .data[[trait]])) +
    geom_violin(fill = violin_color, alpha = 0.75, color = "#333333") +
    geom_boxplot(width = 0.1, fill = "white", color = "#333333") +
    labs(
      title = paste(trait_label, "by", drug_name_labels[drug], "Consumption Class"),
      x = paste(drug_name_labels[drug], "Class"),
      y = paste(trait_label, "Score")
    )
  
  save_plot(p, paste0("violin_", drug, "_", trait, ".png"))
}

for (i in seq_len(nrow(plot_pairs))) {
  make_boxplot(plot_pairs$drug[i], plot_pairs$trait[i], plot_pairs$trait_label[i])
  make_violin(plot_pairs$drug[i], plot_pairs$trait[i], plot_pairs$trait_label[i])
}

# -----------------------------------------------------------------------------
# 6C. Density plots
# -----------------------------------------------------------------------------

density_pairs <- tibble::tribble(
  ~drug_group,       ~trait,       ~title,
  "cannabis_group",  "ss",         "Sensation Seeking by Cannabis Use Group",
  "lsd_group",       "oscore",     "Openness by LSD Use Group",
  "coke_group",      "impulsive",  "Impulsiveness by Cocaine Use Group",
  "nicotine_group",  "nscore",     "Neuroticism by Nicotine Use Group",
  "alcohol_group",   "escore",     "Extraversion by Alcohol Use Group",
  "heroin_group",    "cscore",     "Conscientiousness by Heroin Use Group"
)

make_density <- function(drug_group, trait, title) {
  p <- ggplot(df_viz, aes(x = .data[[trait]], fill = .data[[drug_group]])) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = group_colors) +
    labs(
      title = title,
      x = personality_labels[trait],
      y = "Density",
      fill = "Use Group"
    )
  
  save_plot(p, paste0("density_", drug_group, "_", trait, ".png"))
}

for (i in seq_len(nrow(density_pairs))) {
  make_density(density_pairs$drug_group[i], density_pairs$trait[i], density_pairs$title[i])
}

# -----------------------------------------------------------------------------
# 6D. Correlation heatmaps
# -----------------------------------------------------------------------------

df_numeric <- df_viz
df_numeric[drug_cols] <- lapply(df_numeric[drug_cols], function(x) as.integer(x) - 1)

make_heatmap <- function(data, columns, title, file_name, width = 10, height = 7) {
  cor_matrix <- cor(data[, columns], use = "complete.obs")
  cor_df <- as.data.frame(as.table(cor_matrix))
  
  p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Freq, 2)), size = 3) +
    scale_fill_gradient2(
      low = heatmap_low,
      mid = heatmap_mid,
      high = heatmap_high,
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    labs(title = title, x = "", y = "", fill = "Correlation") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  save_plot(p, file_name, width, height)
}

make_heatmap(
  df_numeric,
  personality_cols,
  "Correlation Heatmap: Personality Traits",
  "heatmap_personality_traits.png"
)

make_heatmap(
  df_numeric,
  drug_cols,
  "Drug Co-Use Heatmap",
  "drug_co_use_heatmap.png"
)

personality_drug_cor <- cor(
  df_numeric[, personality_cols],
  df_numeric[, drug_cols],
  use = "complete.obs"
)

personality_drug_df <- as.data.frame(as.table(personality_drug_cor))

p_personality_drug <- ggplot(personality_drug_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3) +
  scale_fill_gradient2(
    low = heatmap_low,
    mid = heatmap_mid,
    high = heatmap_high,
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Heatmap: Personality Traits vs Drug-Use Classes",
    x = "Drug",
    y = "Personality Trait",
    fill = "Correlation"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

save_plot(p_personality_drug, "heatmap_personality_vs_drug_use.png", width = 12, height = 6)

# -----------------------------------------------------------------------------
# 6E. Demographic distribution plots
# -----------------------------------------------------------------------------

make_bar <- function(x_var, title, x_label, file_name, width = 8, height = 5) {
  p <- ggplot(df_viz, aes(x = .data[[x_var]])) +
    geom_bar(fill = single_bar_color) +
    labs(
      title = title,
      x = x_label,
      y = "Number of Participants"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  save_plot(p, file_name, width, height)
}

make_bar("age_group", "Age Group Distribution", "Age Group", "demographic_age_group.png")
make_bar("gender_label", "Gender Distribution", "Gender", "demographic_gender.png", 7, 5)

# -----------------------------------------------------------------------------
# 6F. Drug use by demographic groups
# -----------------------------------------------------------------------------

make_stacked_percent <- function(group_var, drug, title, file_name, width = 9, height = 5) {
  p <- ggplot(df_viz, aes(x = .data[[group_var]], fill = .data[[drug]])) +
    geom_bar(position = "fill") +
    scale_fill_manual(values = class_colors) +
    scale_y_continuous(labels = percent) +
    labs(
      title = title,
      x = group_var,
      y = "Percentage of Participants",
      fill = paste(drug_name_labels[drug], "Class")
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  save_plot(p, file_name, width, height)
}

for (drug in c("alcohol", "cannabis", "nicotine", "coke", "ecstasy")) {
  make_stacked_percent(
    "gender_label",
    drug,
    paste(drug_name_labels[drug], "Consumption by Gender"),
    paste0("gender_", drug, "_percentage.png")
  )
}

for (drug in c("cannabis", "alcohol", "nicotine", "ecstasy", "coke")) {
  p <- ggplot(df_viz, aes(x = age_group, fill = .data[[drug]])) +
    geom_bar() +
    scale_fill_manual(values = class_colors) +
    labs(
      title = paste(drug_name_labels[drug], "Consumption by Age Group"),
      x = "Age Group",
      y = "Number of Participants",
      fill = paste(drug_name_labels[drug], "Class")
    )
  
  save_plot(p, paste0("age_group_", drug, "_stacked.png"), width = 9, height = 5)
}

# -----------------------------------------------------------------------------
# 6G. Recent-user and never-used percentage charts
# -----------------------------------------------------------------------------

recent_summary <- drug_long %>%
  mutate(recent_user = class %in% recent_levels) %>%
  group_by(drug_label) %>%
  summarise(recent_percentage = mean(recent_user) * 100, .groups = "drop") %>%
  arrange(desc(recent_percentage))

p_recent <- ggplot(
  recent_summary,
  aes(x = reorder(drug_label, recent_percentage), y = recent_percentage)
) +
  geom_col(fill = recent_color) +
  coord_flip() +
  labs(
    title = "Percentage of Recent Users by Drug",
    subtitle = "Recent use = CL3, CL4, CL5, CL6",
    x = "Drug",
    y = "Recent User Percentage"
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"))

save_plot(p_recent, "recent_user_percentage_chart.png", width = 10, height = 7)

never_summary <- drug_long %>%
  mutate(never_used = class == "CL0") %>%
  group_by(drug_label) %>%
  summarise(never_percentage = mean(never_used) * 100, .groups = "drop") %>%
  arrange(desc(never_percentage))

p_never <- ggplot(
  never_summary,
  aes(x = reorder(drug_label, never_percentage), y = never_percentage)
) +
  geom_col(fill = never_color) +
  coord_flip() +
  labs(
    title = "Percentage of Participants Who Never Used Each Drug",
    subtitle = "Never used = CL0",
    x = "Drug",
    y = "Never-Used Percentage"
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%"))

save_plot(p_never, "never_used_percentage_chart.png", width = 10, height = 7)

# -----------------------------------------------------------------------------
# 6H. Personality profile by cannabis user group
# -----------------------------------------------------------------------------

profile_df <- df_viz %>%
  mutate(
    cannabis_profile_group = case_when(
      cannabis == "CL0" ~ "Never Users",
      cannabis == "CL6" ~ "Daily Users",
      cannabis %in% recent_levels ~ "Recent Users",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(cannabis_profile_group)) %>%
  mutate(
    cannabis_profile_group = factor(
      cannabis_profile_group,
      levels = c("Never Users", "Recent Users", "Daily Users")
    )
  )

profile_long <- profile_df %>%
  group_by(cannabis_profile_group) %>%
  summarise(across(all_of(personality_cols), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(
    cols = all_of(personality_cols),
    names_to = "trait",
    values_to = "mean_score"
  ) %>%
  mutate(trait = factor(personality_labels[trait], levels = personality_labels[personality_cols]))

p_profile <- ggplot(profile_long, aes(x = trait, y = mean_score, fill = cannabis_profile_group)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = profile_colors) +
  labs(
    title = "Personality Profile by Cannabis User Group",
    x = "Personality Trait",
    y = "Average Score",
    fill = "User Group"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_profile, "personality_profile_grouped_bar.png", width = 10, height = 6)

# -----------------------------------------------------------------------------
# 6I. Faceted boxplot
# -----------------------------------------------------------------------------

selected_drugs <- c("cannabis", "ecstasy", "lsd", "mushrooms", "coke", "nicotine")

facet_boxplot_data <- df_viz %>%
  select(all_of(selected_drugs), ss) %>%
  pivot_longer(
    cols = all_of(selected_drugs),
    names_to = "drug",
    values_to = "consumption_class"
  ) %>%
  mutate(
    consumption_class = factor(consumption_class, levels = drug_levels, ordered = TRUE),
    drug = factor(drug_name_labels[drug], levels = drug_name_labels[selected_drugs])
  )

p_faceted_box <- ggplot(facet_boxplot_data, aes(x = consumption_class, y = ss)) +
  geom_boxplot(fill = boxplot_color, color = "#333333") +
  facet_wrap(~ drug, ncol = 3) +
  labs(
    title = "Sensation Seeking by Drug Consumption Class",
    x = "Consumption Class",
    y = "Sensation Seeking Score"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(p_faceted_box, "faceted_boxplot_ss_by_drug_class.png", width = 12, height = 8)

# -----------------------------------------------------------------------------
# 6J. Jitter plots
# -----------------------------------------------------------------------------

make_jitter <- function(x, y, color_group, title, subtitle, file_name) {
  p <- ggplot(df_viz, aes(x = .data[[x]], y = .data[[y]], color = .data[[color_group]])) +
    geom_jitter(width = 0.15, height = 0.15, alpha = 0.65, size = 2) +
    scale_color_manual(values = recent_group_colors) +
    labs(
      title = title,
      subtitle = subtitle,
      x = personality_labels[x],
      y = personality_labels[y],
      color = "Group"
    )
  
  save_plot(p, file_name, width = 8, height = 6)
}

make_jitter(
  "impulsive", "ss", "cannabis_recent",
  "Impulsiveness vs Sensation Seeking",
  "Colored by Recent Cannabis Use",
  "jitter_impulsive_ss_cannabis.png"
)

make_jitter(
  "oscore", "ss", "lsd_recent",
  "Openness vs Sensation Seeking",
  "Colored by Recent LSD Use",
  "jitter_oscore_ss_lsd.png"
)

make_jitter(
  "nscore", "cscore", "nicotine_recent",
  "Neuroticism vs Conscientiousness",
  "Colored by Recent Nicotine Use",
  "jitter_nscore_cscore_nicotine.png"
)

make_jitter(
  "escore", "ss", "ecstasy_recent",
  "Extraversion vs Sensation Seeking",
  "Colored by Recent Ecstasy Use",
  "jitter_escore_ss_ecstasy.png"
)

cat("All visualization plots saved in the 'plots' folder.\n")

