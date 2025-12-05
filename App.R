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
        tabName="data",
        fluidRow(
          box(width=12, title="Filtered data",
              status="primary", solidHeader=TRUE,
              downloadButton("download_data", "Download filtered data as CSV"),
              br(), br(),
              DTOutput("tbl_data"))
          )))))

server <- function(input, output, session) {
  
  
  filtered_data <- reactive({
    df <- dig
    
    df <- df[df$TRTMT %in% input$f_trt, ]
    
    
    df <- df[df$SEX %in% input$f_sex, ]
    
    df <- df[!is.na(df$AGE) &
               df$AGE >= input$f_age[1] &
               df$AGE <= input$f_age[2], ]
    
    
    df <- df[!is.na(df$EJF_PER) &
               df$EJF_PER >= input$f_ef[1] &
               df$EJF_PER <= input$f_ef[2], ]
    
    df
  })
  
  
  output$vb_n <- renderValueBox({
    df <- filtered_data()
    n <- nrow(df)
    
    valueBox(
      value = n,
      subtitle = "Patients in filtered subset",
      color = "light-blue"
    )
  })
  
  output$vb_trt_prop <- renderValueBox({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      valueBox(
        value = "0%",
        subtitle = "On digoxin (among filtered)",
        color = "green"
      )
    } else {
      trt_tab <- table(df$TRTMT)
      if ("Digoxin" %in% names(trt_tab)) {
        prop_dig <- 100 * trt_tab[["Digoxin"]] / sum(trt_tab)
      } else {
        prop_dig <- 0
      }
      
      valueBox(
        value = sprintf("%.1f%%", prop_dig),
        subtitle = "On digoxin (among filtered)",
        color = "green"
      )
    }
  })
  
  output$vb_death_rate <- renderValueBox({
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      valueBox(
        value = "NA",
        subtitle = "Death rate (follow-up)",
        color = "red"
      )
    } else {
      death_rate <- mean(df$DEATH == "Dead", na.rm = TRUE) * 100
      
      valueBox(
        value = sprintf("%.1f%%", death_rate),
        subtitle = "Death rate (follow-up)",
        color = "red"
      )
    }
  })
  
  output$tbl_overview <- renderDT({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    sum_tbl <- df %>%
      group_by(TRTMT) %>%               
      summarise(
        n        = n(),
        mean_age = mean(AGE, na.rm = TRUE),
        sd_age   = sd(AGE, na.rm = TRUE),
        mean_ef  = mean(EJF_PER, na.rm = TRUE),
        sd_ef    = sd(EJF_PER, na.rm = TRUE),
        mean_bmi = mean(BMI, na.rm = TRUE),
        sd_bmi   = sd(BMI, na.rm = TRUE)
      )
    
    datatable(sum_tbl, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$plot_funclass <- renderPlot({
    df <- filtered_data()
    
    if (nrow(df) == 0 || !("FUNCTCLS" %in% names(df))) {
      return(NULL)
    }
    
    df %>%
      mutate(FUNCTCLS = factor(FUNCTCLS)) %>%
      filter(!is.na(FUNCTCLS)) %>%
      group_by(TRTMT, FUNCTCLS) %>%
      summarise(n = n()) %>%
      group_by(TRTMT) %>%
      mutate(prop = n / sum(n)) %>%
      ggplot(aes(x = TRTMT, y = prop, fill = FUNCTCLS)) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = "Treatment",
        y = "Proportion",
        fill = "NYHA class"
      )
  })
  
 
  
  output$plot_age_ef <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    if (input$cp_colour == "TRTMT") {
      p <- ggplot(df, aes(x = AGE, y = EJF_PER, colour = TRTMT))
      colour_lab <- "Treatment"
    } else {
      p <- ggplot(df, aes(x = AGE, y = EJF_PER, colour = DEATH))
      colour_lab <- "Death status"
    }
    
    p <- p +
      geom_point(alpha = 0.7) +
      labs(
        x = "Age at randomisation (years)",
        y = "Ejection fraction (%)",
        colour = colour_lab
      )
    
    if (input$cp_smooth) {
      p <- p + geom_smooth(se = FALSE, method = "loess")
    }
    
    p
  })
  
  output$plot_ef_hist <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = EJF_PER, fill = TRTMT)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
      labs(
        x = "Ejection fraction (%)",
        y = "Count",
        fill = "Treatment"
      )
  })
  
  output$tbl_data <- renderDT({
    filtered_data()
  }, options = list(pageLength = 20, scrollX = TRUE))
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("DIG_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}
shinyApp(ui, server)