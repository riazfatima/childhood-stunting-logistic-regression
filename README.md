# Childhood Stunting Prediction Using Logistic Regression

## Overview

This project investigates factors associated with childhood stunting among children under five years of age using data from the Pakistan Punjab Multiple Indicator Cluster Survey (MICS6).

A binary logistic regression model was developed to identify significant demographic, maternal, and socioeconomic predictors of stunting.

## Dataset

**Data Source**

* Pakistan Punjab MICS6 Survey
* Child Dataset (CH)
* Birth History Dataset (BH)

The MICS6 datasets are not included in this repository.

Researchers interested in reproducing the analysis should obtain the datasets through the UNICEF MICS program and place them in the appropriate data directory before running the script.

## Variables Used

### Outcome Variable

* Stunted (Yes/No)

### Predictor Variables

* Child age (months)
* Child sex
* Ever breastfed
* Maternal BMI
* Household wealth index
* Mother's education level

## Methods

1. Data cleaning and dataset merging
2. Missing value imputation
3. Creation of stunting indicator using WHO Height-for-Age Z-score (HAZ) standards
4. Train-test split (70:30)
5. Logistic regression modeling
6. Odds ratio estimation with 95% confidence intervals
7. Model evaluation using:

   * Confusion Matrix
   * Accuracy
   * ROC Curve
   * Area Under the Curve (AUC)

## Results

### Model Performance

* Accuracy: 68.4%
* AUC: 0.658

### Key Findings

* Older children showed higher odds of stunting.
* Children who were not breastfed had higher odds of stunting.
* Higher maternal BMI was associated with lower odds of stunting.
* Higher household wealth was associated with lower odds of stunting.
* Higher maternal education was associated with lower odds of stunting.

## Software

* R
* haven
* dplyr
* caret
* pROC

## Reproducibility
## How to Run

1. Download MICS6 dataset from UNICEF MICS portal
2. Place files in /data folder
3. Open R or RStudio
4. Run:

source("scripts/logistic_regression.R")

Required R packages:

* haven
* dplyr
* caret
* pROC

## Author

Fatima Riaz
