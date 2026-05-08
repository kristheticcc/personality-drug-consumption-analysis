# Drug Consumption & Personality Traits Analysis

**MATH 167R: Statistical Analysis with R**
**Spring 2026 | Professor Gottlieb | Group P**
**Members:** Krish Makwana, Kanishka Yadav, Eric Nguyen, Ramjot Dhillon

## Overview
Analysis of the [Drug Consumption (Quantified)](https://archive.ics.uci.edu/dataset/373/drug+consumption+quantified) dataset from the UCI Machine Learning Repository. We explore the relationship between personality traits and drug use behavior across 1,885 respondents.

## Research Questions


## Setup

### Requirements
- R
- RStudio
- Packages: `tidyverse`, `ggplot2`

### Dataset
The dataset is not included in this repo in its original form. The dataset inside data/ is the same dataset that we renamed to drug_consumption.csv to utilize csv format. The drug_consumption.csv will be included in the folder where the repository is cloned, but if someone wishes to see and use the dataset in original form, here are the steps:

1. Download from [UCI ML Repository](https://archive.ics.uci.edu/dataset/373/drug+consumption+quantified) 

2. Extract the zip file

3. Rename `drug_consumption.data` -> `drug_consumption.csv`

4. Place the file in the `data/` folder (Note: drug_consumption.csv will already be in data/ if the repo is cloned)

## Running the Analysis

1. Open `personality-drug-consumption-analysis.Rproj` in RStudio
2. Run `analysis.R`

## Repository Structure
```
├── data/
│   └── drug_consumption.csv
├── analysis.R
├── README.md
└── .gitignore
```
