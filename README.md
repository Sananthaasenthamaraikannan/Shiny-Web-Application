# Survival Analysis - Shiny Web Application - Sananthaa J S & Kaushal Mayur
Interactive Shiny Web Application - Progress, Workflow and the Application.
Interactive Shiny or Shinydashboard application Development that explores the DIG Trial dataset.
An application that will help users understand key aspects of the data and explore relationships between variables.

Overview

This Shiny dashboard is a friendly exploratory app built to analyse data from the Digitalis Investigation Group (DIG) heart failure trial.
To Explore baseline characteristics of patients,Visualise clinical profiles, such as age and ejection fraction,Examine survival outcomes using Kaplan–Meier curves.

Data
Primary Dataset: DIG-1.csv

Key variables included:
TRTMT – Treatment group (0 = Placebo, 1 = Digoxin)
SEX – Sex (1 = Male, 2 = Female)
RACE – Race (1 = White, 2 = Nonwhite)
AGE – Age at randomisation
EJF_PER – Ejection fraction (%)
DEATH – Mortality status (0 = Alive, 1 = Dead)
DEATHDAY – Days to death or last follow-up
FUNCTCLS, BMI, etc. – Additional clinical variables

R packages:
shiny
shinydashboard
tidyverse
DT
survival
survminer


App layout and navigation:

Header:
Displays the title "DIG Heart Failure Trial Explorer"
Sidebar
Includes navigation menu and filters:

Tabs:
Overview
Clinical Profile
Survival Analysis
Data

Filters:
Treatment group (Placebo / Digoxin)
Sex (Male / Female)
Age range slider
Ejection fraction (EF) range slider

Dashboard Body (Tabs)
Overview:
Three value boxes
Number of filtered patients
Percentage on digoxin
Death rate
Summary table by treatment group
NYHA functional class plot

Clinical Profile:
Scatter plot: Age vs Ejection Fraction
Colour by treatment or death status
Optional LOESS smoothing
Histogram of EF

Survival Analysis:
curves
Printed survival summary (events, estimates, etc.)

Data:
Fully filterable table using DT
Button to download the current filtered dataset

Technologies Used:

- **R / RStudio**
- **Shiny**
- **tidyverse** (data wrangling)
- **survival** (survival objects, Cox models)
- **survminer** (KM plots & visual summaries) *(if used)*
- **ggplot2** (visualizations) *(if used)*
- **DT** (interactive tables) *(if used)*

Key Features:
- **Kaplan–Meier survival curves** for cohort comparison  
- **Cox proportional hazards modeling** (hazard ratio interpretation)  
- **Interactive filtering** by cohort/subgroup variables  
- **Summary tables** for events, follow-up time, and model outputs  
- **Reproducible workflow**: consistent preprocessing → analysis → visualization

What Users Can Do:

- Upload or load a dataset (depending on app configuration)
- Select cohorts/groups to compare
- Visualize survival probability over time (KM)
- Review model estimates (Cox HRs) and associated confidence intervals
- Explore subgroup effects via filters (e.g., treatment arm, sex, risk strata)
- Export tables/plots *(if enabled in your app)*

How It Works (Process):

1. **Data intake**
   - Load clinical time-to-event dataset (time, event indicator + grouping variables)
2. **Preprocessing**
   - Clean/validate fields, handle missingness, standardize types
3. **Survival object creation**
   - `Surv(time, event)` setup for analysis
4. **Modeling**
   - Kaplan–Meier estimation and Cox model fitting
5. **Visualization & reporting**
   - Interactive plots and summary tables rendered in Shiny UI

How We Built It:

- Designed a Shiny UI for fast clinical interpretation (filters → plots → tables)
- Implemented server-side reactive logic to:
  - validate inputs
  - update analysis outputs dynamically
  - generate KM curves and Cox summaries per user selections
- Used structured wrangling steps to ensure consistent analysis-ready input format

What We Learned:

- Translating biostatistical workflows into an interactive product experience
- Building **reactive pipelines** in Shiny without breaking reproducibility
- Presenting survival outputs in a way that is interpretable for non-technical users
- Handling clinical dataset constraints (missingness, subgroup sparsity, validation)

Potential Improvements:

- Add **diagnostics** (PH assumption checks, Schoenfeld residuals)
- Add **more endpoints** (competing risks, stratified Cox, time-varying effects)
- Improve **data validation UI** (auto-detect column roles, clearer error messages)
- Add **export** options (download plots, tables, model summaries as CSV/PDF)
- Add **modular Shiny architecture** for scalability (modules + testing)

