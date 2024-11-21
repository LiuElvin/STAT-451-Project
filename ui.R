library(shinydashboard)
library(tidyverse)

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
             icon = icon("whiskey-glass")),
    menuItem("Chronic Liver Disease Mortality",
             tabName = "cldm",
             icon = icon("skull")),
    
    # Sidebar
    selectInput("state", "State", 
                choices = unique_states),
    selectInput("year_one", "Year 1", 
                choices = 2010:2021,
                selected = 2010),
    selectInput("year_two", "Year 2", 
                choices = 2010:2021,
                selected = 2020)
  ),
  
  # Conditional sidebar
  conditionalPanel(
    condition = "input.menu1 == 'pcac'",
    selectInput("car2", "Select a car (Tab 2):", choices = 1:5),
    numericInput("customCyl", "Custom Cylinders:", value = 4, min = 2, max = 8)
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
    tabItem(tabName = "cldm",
            fluidRow(
              
            )
    )
  )
)

dashboardPage(header, sidebar, body)