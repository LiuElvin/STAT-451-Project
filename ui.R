library(shinydashboard)
library(tidyverse)
library(plotly)

df <- read.csv("alcohol_cdi.csv")

x <- filter(df, LocationDesc != "United States")

unique_states <- x$LocationDesc %>% 
  unique() %>% 
  sort()

header <- dashboardHeader(title = "Alcohol Trends")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "menu1",
    # Tabs
    menuItem("Alcohol Use Among Youth",
             tabName = "auay",
             icon = icon("child")),
    menuItem("Chronic Liver Disease Mortality",
             tabName = "cldm",
             icon = icon("skull")),
    menuItem("Per Capita Alcohol Consumption",
             tabName = "pcac",
             icon = icon("wine-glass")),
    menuItem("State Alcohol Regulation",
             tabName = "csar",
             icon = icon("scale-balanced")),
    # menuItem("Mental Health Trends",
    #          tabName = "mht",
    #          icon = icon("smile")),
    # 

    conditionalPanel(
      condition = "input.menu1 == 'auay'",
      selectInput("state_2", "Select State", choices = unique_states),
      selectInput("stratification_category_2", "Select Stratification Category", choices = c("Overall", "Gender", "Race/Ethnicity"), selected = "Overall"),
      selectInput("stratification_2", "Select Stratification", choices = NULL),
      checkboxInput("trendline_2", "Show United States Average", value = TRUE)),
    conditionalPanel(
      condition = "input.menu1 == 'cldm'",
      selectInput("state_4", "Select State", choices = unique_states),
      selectInput("year_4_1", "Select Year 1", choices = c(2010:2020), selected = 2010),
      selectInput("year_4_2", "Select Year 2", choices = c(2010:2020), selected = 2020),
      selectInput("data_type_4", "Select Data Type", choices = c("Age-adjusted Rate", "Crude Rate", "Number"), selected = "Crude Rate")),
    conditionalPanel(
      condition = "input.menu1 == 'pcac'",
      selectInput("state_1", "State", choices = unique_states),
      selectInput("year_one_1", "Year 1", choices = c(2010, 2014, 2016, 2018, 2019, 2020), selected = 2010),
      selectInput("year_two_1", "Year 2",  choices = c(2010, 2014, 2016, 2018, 2019, 2020), selected = 2020)),
    conditionalPanel(
      condition = "input.menu1 == 'csar'",
      selectInput("plot_3", "Select Plot Type", choices = c("Boxplot", "Scatterplot"), selected = "Boxplot"),
      selectInput("year_3", "Select Year", choices = c(2012, 2014, 2020), selected = 2020),
      selectInput("data_type_3", "Select Data Type", choices = c("Age-adjusted Rate", "Crude Rate"), selected = "Crude Rate"),
      selectInput("stratification_category_3", "Select Stratification Category", choices = c("Overall", "Gender", "Race/Ethnicity"), selected = "Overall"),
      selectInput("stratification_3", "Select Stratification", choices = NULL),
      checkboxInput("trendline_3", "Show Trendline", value = TRUE))
    # conditionalPanel(
    #   condition = "input.menu1 == 'mht'",
    #   selectInput(
    #     "selected_race", 
    #     "Select Race/Ethnicity", choices = NULL))
  )
)

body <- dashboardBody(
  tabItems(
    # Items in tabs
    tabItem(tabName = "auay",
            fluidRow(
              box(title = "Summary of Statewide Youth Alcohol Use",
                  status = "primary", solidHeader = TRUE, width = 6,
                  htmlOutput("auay_summary")),
              conditionalPanel(
                condition = "input.trendline_2 == true",
                box(title = "Summary of United States Youth Alcohol Use",
                    status = "primary", solidHeader = TRUE, width = 6,
                    htmlOutput("us_summary"))
              ),
              box(width = 12, plotlyOutput("auay_linePlot"))
            )
    ),
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
    tabItem(tabName = "csar",
            fluidRow(
              box(title = "Legend for Categories of State Alcohol Regulation",
                  status = "primary", solidHeader = TRUE, width = 6,
                  htmlOutput("cat_1")),
              box(title = "Summary of Chronic Liver Disease Mortality by Category",
                  status = "primary", solidHeader = TRUE, width = 6,
                  htmlOutput("cat_2")),
              box(width = 12, plotlyOutput("csar_plot"))
            )
    ),
    tabItem(tabName = "cldm",
            fluidRow(
              box(title = "Summary of Statewide Chronic Liver Disease Mortality",
                  status = "primary", solidHeader = TRUE, width = 6,
                  htmlOutput("cldm_state")),
              box(title = "Summary of United States Chronic Liver Disease Mortality",
                  status = "primary", solidHeader = TRUE, width = 6,
                  htmlOutput("cldm_us")),
              box(width = 12, plotlyOutput("cldm_plot_1")),
              box(width = 12, plotlyOutput("cldm_plot_2"))
            )
    )
    #     tabItem(
    #   tabName = "mht",
    #   fluidRow(
    #     box(
    #       title = "Overall Trend in Average Mentally Unhealthy Days",
    #       status = "primary", solidHeader = TRUE, width = 12,
    #       plotOutput("grand_mean_plot")
    #     )
    #   ),
    #   fluidRow(
    #     box(
    #       title = "Trends by Race",
    #       status = "primary", solidHeader = TRUE, width = 12,
    #       plotOutput("selected_race_plot")
    #     )
    #   )
    # )
  )
)

dashboardPage(header, sidebar, body)
