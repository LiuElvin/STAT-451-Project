library(shinydashboard)
library(tidyverse)

df <- read.csv("U.S._Chronic_Disease_Indicators__CDI___2023_Release.csv")

unique_territories <- df$LocationDesc %>% 
  unique()

unique_states <- sort(unique_territories[unique_territories != "United States" &
                                           unique_territories != "District of Columbia" &
                                           unique_territories != "Virgin Islands" &
                                           unique_territories != "Puerto Rico" &
                                           unique_territories != "Guam"])

header <- dashboardHeader(title = "Alcohol")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Per Capita Alcohol Consumption",
             tabName = "pcac",
             icon = icon("whiskey-glass")),
    selectInput("state", "State", 
                choices = unique_states),
    selectInput("year_one", "Year 1", 
                choices = 2010:2021,
                selected = 2010),
    selectInput("year_two", "Year 2", 
                choices = 2010:2021,
                selected = 2020)
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pcac",
            fluidRow(
              box(title = "Summary of Per Capita Alcohol Consumption",
                  status = "primary", solidHeader = TRUE, width = 12,
                  textOutput("state_norm_summary")),
              box(width = 6, plotOutput("pcac_bar_1")),
              box(width = 6, plotOutput("pcac_bar_2"))),
            fluidRow(
              box(title = "Summary of Change in Per Capita Alcohol Consumption",
                  status = "primary", solidHeader = TRUE, width = 12,
                  textOutput("state_change_summary")),
              box(width = 12, plotOutput("pcac_comparison_bar")))
    )
  )
)

dashboardPage(header, sidebar, body)