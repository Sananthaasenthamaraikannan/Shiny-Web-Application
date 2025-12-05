# Shiny-Web-Application - Sananthaa J S & Kaushal Mayur
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
