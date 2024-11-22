library(shinydashboard)
library(tidyverse)
library(plotly)

# Data
# df <- read.csv("U.S._Chronic_Disease_Indicators__CDI___2023_Release.csv")

df <- read.csv("alcohol_cdi.csv")

# List of all territories
unique_states <- df$LocationDesc %>% 
  unique() %>% 
  sort()

header <- dashboardHeader(title = "Alcohol")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu1",
    # Tabs
    menuItem("Per Capita Alcohol Consumption",
             tabName = "pcac",
             icon = icon("wine-glass")),
    menuItem("Alcohol Use Among Youth",
             tabName = "auay",
             icon = icon("child")),
    menuItem("Chronic Liver Disease Mortality",
             tabName = "cldm",
             icon = icon("skull")),
  
    # Conditional sidebar
    conditionalPanel(
      condition = "input.menu1 == 'pcac'",
      selectInput("state_1", "State", 
                  choices = unique_states),
      selectInput("year_one_1", "Year 1", 
                  choices = c(2010, 2014, 2016, 2018, 2019, 2020),
                  selected = 2010),
      selectInput("year_two_1", "Year 2", 
                  choices = c(2010, 2014, 2016, 2018, 2019, 2020),
                  selected = 2020)),
    
    conditionalPanel(
      condition = "input.menu1 == 'auay'",
      selectInput("state_2", "Select State", choices = unique_states),
      selectInput("stratification_category_2", "Select Stratification Category", choices = c("Overall", "Gender", "Race/Ethnicity"), selected = "Overall"),
      selectInput("stratification_2", "Select Stratification", choices = NULL),
      checkboxInput("trendline_2", "Show Trendline", value = TRUE)),
    
    conditionalPanel(
      condition = "input.menu1 == 'cldm'",
      selectInput("year_3", "Select Year", choices = c(2012, 2014, 2020), selected = 2020),
      selectInput("data_type_3", "Select Data Type", choices = c("Age-adjusted Rate", "Crude Rate"), selected = "Crude Rate"),
      selectInput("stratification_category_3", "Select Stratification Category", choices = c("Overall", "Gender", "Race/Ethnicity"), selected = "Overall"),
      selectInput("stratification_3", "Select Stratification", choices = NULL),
      checkboxInput("trendline_3", "Show Trendline", value = TRUE))
  )
)

body <- dashboardBody(
  tabItems(
    # Items in tab
    tabItem(tabName = "pcac",
            fluidRow(
              box(title = "Summary of Per Capita Alcohol Consumption",
                  status = "primary", solidHeader = TRUE, width = 12,
                  textOutput("state_norm_summary")),
              box(width = 6, plotOutput("pcac_bar_1")),
              box(width = 6, plotOutput("pcac_bar_2")),
              box(title = "Summary of Change in Per Capita Alcohol Consumption",
                  status = "primary", solidHeader = TRUE, width = 12,
                  textOutput("state_change_summary")),
              box(width = 12, plotOutput("pcac_comparison_bar")))
    ),
    tabItem(tabName = "auay",
            fluidRow(
              box(width = 12, plotlyOutput("auay_scatterPlot"))
            )
    ),
    tabItem(tabName = "cldm",
            fluidRow(
              box(title = "Note About Categories",
                  status = "primary", solidHeader = TRUE, width = 12,
                  htmlOutput("cat")),
              box(width = 12, plotlyOutput("cldm_scatterPlot"))
            )
    )
  )
)

dashboardPage(header, sidebar, body)