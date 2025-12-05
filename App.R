library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(survival)
library(survminer)
dig_raw <- read.csv("DIG-1.csv", stringsAsFactors = FALSE)
dig <- dig_raw %>%
  mutate(TRTMT=factor(TRTMT,levels=c(0, 1),labels=c("Placebo", "Digoxin")),
         SEX=factor(SEX,levels=c(1, 2),labels=c("Male", "Female")),
         RACE=factor(RACE,levels=c(1, 2),labels=c("White", "Nonwhite")),
         DEATH=factor(DEATH,levels=c(0, 1),labels=c("Alive", "Dead")),
         death_event=as.numeric(DEATH=="Dead"))
age_range <- range(dig$AGE, na.rm = TRUE)
ef_range  <- range(dig$EJF_PER, na.rm = TRUE)
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "DIG Heart Failure Trial Explorer"),
  dashboardSidebar(sidebarMenu(
    id = "tabs",
    menuItem("Overview",tabName="overview"),
    menuItem("Clinical Profile",tabName="clinical"),
    menuItem("Survival Analysis",tabName="survival"),
    menuItem("Data",tabName="data")),
    hr(),
    h4("Filters"),
    helpText("Use these filters to subset the DIG trial."),
    checkboxGroupInput("f_trt", "Treatment:", choices = c("Placebo","Digoxin"),
                       selected=c("Placebo","Digoxin")),
    checkboxGroupInput("f_sex", "Sex:", choices=c("Male","Female"),
                       selected=c("Male","Female")),
    sliderInput("f_age", "Age at randomisation:",
                min=floor(age_range[1]), max=ceiling(age_range[2]),
                value=age_range),
    sliderInput("f_ef", "Ejection fraction (%):",
                min=floor(ef_range[1]), max=ceiling(ef_range[2]),
                value=ef_range)),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { border-radius: 8px; }
        .content-wrapper { background-color: #f5f7fa; }"))),
    tabItems(
      tabItem(
        tabName="overview",
        fluidRow(
          valueBoxOutput("vb_n"),
          valueBoxOutput("vb_trt_prop"),
          valueBoxOutput("vb_death_rate")),
        fluidRow(
          box(width=6, title="Baseline summary by treatment",
              status="primary", solidHeader=TRUE,
              DTOutput("tbl_overview")),
          box(width=6, title = "NYHA functional class by treatment",
              status="info", solidHeader=TRUE,
              plotOutput("plot_funclass")))),
      tabItem(
        tabName="clinical",
        fluidRow(
          box(width=6, title = "Age vs Ejection Fraction",
              status="warning", solidHeader=TRUE,
              selectInput("cp_colour", "Colour points by:",
                          choices=c("Treatment"="TRTMT",
                                      "Death status"="DEATH"),
                          selected="TRTMT"),
              checkboxInput("cp_smooth", "Add smoothing line", value=TRUE),
              plotOutput("plot_age_ef")),
          box(width=6, title="Distribution of Ejection Fraction",
              status="warning", solidHeader=TRUE,
              plotOutput("plot_ef_hist")))),
      tabItem(
        tabName="survival",
        fluidRow(
          box(width=8, title="Curves: Time to death",
              status="success", solidHeader=TRUE,
              plotOutput("plot_surv")),
          box(width=4, title="Survival summary",
              status="success", solidHeader=TRUE,
              verbatimTextOutput("txt_surv_summary")))),
      tabItem(
        tabName="data",
        fluidRow(
          box(width=12, title="Filtered data",
              status="primary", solidHeader=TRUE,
              downloadButton("download_data", "Download filtered data as CSV"),
              br(), br(),
              DTOutput("tbl_data"))
          )))))
server <- function(input, output, session) {}
shinyApp(ui, server)
