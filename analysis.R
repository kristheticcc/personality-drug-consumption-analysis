# Load libraries

# install.packages("tidyverse")
# install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
df <- read.csv("data/drug_consumption.csv", header = FALSE)
head(df,3)

colnames(df) <- c(
  "id", "age", "gender", "education", "country", "ethnicity", "nscore", "escore",    
  "oscore", "ascore", "cscore", "impulsive", "ss", "alcohol", "amphet", "amyl",
  "benzos", "caff", "cannabis", "choc", "coke", "crack", "ecstasy", "heroin", 
  "ketamine", "legalh", "lsd", "meth", "mushrooms", "nicotine", "semer", "vsa"
)
head(df,3)
  
cat("No. of rows:", nrow(df), "No. of cols:",ncol(df), "\n")

str(df)
summary(df)

