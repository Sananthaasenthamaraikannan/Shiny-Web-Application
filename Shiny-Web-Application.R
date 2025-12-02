library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(survival)
library(survminer)

dig_raw<-read.csv("D:/R Computing/Assignment - 5/DIG-1.csv", stringsAsFactors = FALSE)

dig<-dig_raw %>%
  mutate(TRTMT=factor(TRTMT,levels=c(0, 1), labels=c("Placebo", "Digoxin")),
         SEX=factor(SEX,levels=c(1, 2), labels=c("Male", "Female")),
         RACE=factor(RACE,levels=c(1, 2), labels=c("White", "Nonwhite")),
         DEATH=factor(DEATH,levels=c(0, 1), labels=c("Alive", "Dead")),
         death_event=as.numeric(DEATH=="Dead"))
age_range<-range(dig$AGE, na.rm=TRUE)
ef_range<-range(dig$EJF_PER, na.rm=TRUE)
age_range
ef_range

ui<-dashboardPage(skin="black",
                  dashboardHeader(title="DIG Heart Failure Trial Explorer"),
                  dashboardSidebar(sidebarMenu(id="tabs",
                                               menuItem("Overview",tabName="overview"),
                                               menuItem("Clinical Profile",tabName="clinical"),
                                               menuItem("Survival Analysis",tabName="survival"),
                                               menuItem("Data",tabName="data")),
                                   hr(),
                                   h4("Filters"),
                                   helpText("Use these filters to subset the DIG trial."),
                                   checkboxGroupInput(
                                     inputId = "f_trt",
                                     label   = "Treatment:",
                                     choices = c("Placebo", "Digoxin"),
                                     selected = c("Placebo", "Digoxin")),
                                   checkboxGroupInput(
                                     inputId = "f_sex",
                                     label   = "Sex:",
                                     choices = c("Male", "Female"),
                                     selected = c("Male", "Female")),
                                   sliderInput(
                                     inputId = "f_age",
                                     label   = "Age at randomisation:",
                                     min     = floor(age_range[1]),
                                     max     = ceiling(age_range[2]),
                                     value   = age_range),
                                   sliderInput(
                                     inputId = "f_ef",
                                     label   = "Ejection fraction (%):",
                                     min     = floor(ef_range[1]),
                                     max     = ceiling(ef_range[2]),
                                     value   = ef_range)),
                  dashboardBody(
                    tags$head(
                      tags$style(HTML("
        .box { border-radius: 8px; }
        .content-wrapper { background-color: #f5f7fa; }
      "))),
                    
                    tabItems(tabItem(tabName="overview",
                                     fluidRow(valueBoxOutput("vb_n"),valueBoxOutput("vb_trt_prop"),valueBoxOutput("vb_death_rate")),
                                     fluidRow(
                                       box(
                                         width = 6, title = "Baseline summary by treatment",
                                         status = "primary", solidHeader = TRUE,
                                         DTOutput("tbl_overview")),
                                       box(
                                         width = 6, title = "NYHA functional class by treatment",
                                         status = "info", solidHeader = TRUE,
                                         plotOutput("plot_funclass")
                                       ))),
                             tabItem(
                               tabName = "data",
                               fluidRow(
                                 box(
                                   width = 12, title = "Filtered data",
                                   status = "primary", solidHeader = TRUE,
                                   downloadButton("download_data", "Download filtered data as CSV"),
                                   br(), br(),
                                   DTOutput("tbl_data")))))))
