# Data-Cleaning-Exploratory-Data-Analysis-EDA-

# Nairobi Pediatric Oral Health Analysis

An exploratory data analysis (EDA) and statistical pipeline built in **R** to clean, manage, and analyze pediatric dental health survey data collected across multiple medical camps in Nairobi, Kenya (Korogocho, Dandora, and Alice Home).

---

## 📊 Project Overview

This repository contains a complete R workflow designed to handle "dirty" real-world epidemiological data, perform rigorous data wrangling, generate descriptive visualizations, and execute both parametric and non-parametric statistical tests to evaluate children's oral hygiene habits, dental characteristics, and associated risk factors.

---

## 🗂️ Project Structure & Workflow

The analysis script is structured into sequential phases:

1. **Data Ingestion & Cleaning:**
* Handling European-style CSV formats (`read.csv2`) and standard formats (`read.csv`).
* Mapping missing values (`na.strings = c("", "??-")`).
* Standardizing string variables, fixing typos, and mapping inconsistent categories (e.g., cleaning `overjet`, `crossbite`, and `eta`/age fields).
* Managing columns with high missing data ratios (e.g., dropping uninformative columns like `malaria` or `neema_already`).


2. **Exploratory Data Analysis (EDA) & Descriptive Statistics:**
* Generating frequency distributions, summary statistics, and cross-tabulations using `Hmisc` and `epiDisplay`.
* Visualizing categorical and continuous variables using histograms, boxplots, and bar charts.
* Grouping age into clinical categories using custom break intervals (`cut()`).


3. **Stratified & Inferential Statistics:**
* **Normality Testing:** Shapiro-Wilk test (`shapiro.test`) to evaluate distribution shapes.
* **Group Comparisons:**
* Independent Two-Sample T-tests & Mann-Whitney U tests (`wilcox.test`) for 2-group comparisons (e.g., toothbrush usage vs. age).
* Kruskal-Wallis test and Dunn’s Post-Hoc Test (`FSA` package) with Bonferroni correction for multi-group comparisons across medical camps.


* **Categorical Association:** Chi-Square tests of independence (`chisq.test`) for dietary habits (sweets vs. juices).
* **Randomness Analysis:** Runs tests (`randtests`) to detect structural data ordering and field collection clustering.


4. **Data Export:**
* Exporting cleaned datasets (`write.csv`, `write.csv2`) and saving workspace sessions (`.RData`).



---

## 📦 Required R Packages

Ensure you have the following R packages installed before running the script:

```r
install.packages(c("Hmisc", "epiDisplay", "FSA", "randtests"))

```

* **`Hmisc`**: Advanced descriptive summary statistics.
* **`epiDisplay`**: Epidemiological data presentation tools and enhanced frequency tables (`tab1`).
* **`FSA`**: Fisheries and Stock Assessment (used here for Dunn's post-hoc tests).
* **`randtests`**: Non-parametric randomness tests for sequence data.

---
