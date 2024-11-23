# Load necessary libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)


# Load the dataset
df <- read.csv("U.S._Chronic_Disease_Indicators__CDI___2023_Release.csv")
df_alcohol <- df[df$Topic == "Alcohol", ]

# Define available years for Chronic Liver Disease Mortality
available_years <- sort(unique(df_alcohol[df_alcohol$Question == "Chronic liver disease mortality", ]$YearStart))

# Define available Data Value Type
available_data_type <- c("Age-adjusted Rate", "Crude Rate")

# Define available Stratification Category
available_stratification_category <- sort(unique(df_alcohol[df_alcohol$Question == "Chronic liver disease mortality", ]$StratificationCategory1))

# Define the Shiny app UI
ui <- fluidPage(
  # Application title
  titlePanel("Chronic Liver Disease Mortality by Alcohol Outlet Density Regulation"),
  
  # Sidebar with inputs for filtering and customizing the plot
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", choices = available_years, selected = 2020),
      selectInput("data_type", "Select Data Type", choices = available_data_type, selected = "Crude Rate"),
      selectInput("stratification_category", "Select Stratification Category", choices = available_stratification_category, selected = "Overall"),
      selectInput("stratification", "Select Stratification", choices = NULL),
      checkboxInput("trendline", "Show Trendline", value = TRUE)
    ),
    
    # Main panel for displaying the plot
    mainPanel(
      plotlyOutput("scatterPlot")
    )
  )
)

  # Define the server function
server <- function(input, output, session) {
  
  # Reactive expression to return the relevant stratification values
  stratification_values <- reactive({
    # Filter based on the selected category
    filtered_values <- df_alcohol[df_alcohol$StratificationCategory1 == input$stratification_category, ]
    unique(filtered_values$Stratification1)  # Return the unique Stratification1 values
  })
  
  # Update the 'stratification' input choice
  observe({
    updateSelectInput(session, "stratification", choices = stratification_values())
  })
  
  # Reactive expression to filter the data based on the selected year
  filtered_data <- reactive({
    df_year <- df[df$YearStart == input$year, ]
    
    # Filter for Chronic Liver Disease Mortality data
    df_year_cldm <- df_year[df_year$Question == "Chronic liver disease mortality" & 
                            df_year$DataValueUnit == "cases per 100,000" & 
                            df_year$DataValueType == input$data_type & 
                            df_year$StratificationCategory1 == input$stratification_category &
                            df_year$Stratification1 == input$stratification, ]
    
    # Filter for Local Authority Regulation data
    df_regulation <- df %>%
      filter(YearStart == 2020,
             Question == "Local authority to regulate alcohol outlet density", 
             DataValue != "")
    
    # Merge the datasets
    result_df <- df_regulation %>%
      select(YearStart, LocationDesc, DataValue) %>%
      left_join(df_year_cldm %>% select(YearStart, LocationDesc, DataValueAlt), 
                by = c("LocationDesc")) %>%
      rename(DataValue_regulation = DataValue, 
             DataValue_cldm = DataValueAlt)
    
    # Check if the merge resulted in any rows
    if (nrow(result_df) == 0) {
      return(NULL)
    }
    
    # Map the category to number
    category_mapping <- c(
      "Category 1 - State had exclusive local alcohol retail licensing" = 1,
      "Category 2 - State had joint local and state alcohol retail licensing" = 2,
      "Category 3 - State had exclusive state alcohol retail licensing but with local zoning authority" = 3,
      "Category 4 - State had mixed alcohol retail licensing policies" = 4,
      "Category 5 - State had nearly exclusive state alcohol retail licensing" = 5,
      "Category 6 - State had exclusive state alcohol retail licensing" = 6
    )
    
    result_df <- result_df %>%
      mutate(CategoryNumber = category_mapping[DataValue_regulation])
    
    return(result_df)
  })
  
  # Render the interactive plot using plotly
  output$scatterPlot <- renderPlotly({
    df_for_plot <- filtered_data()
    
    if (is.null(df_for_plot)) {
      return(NULL)
    }
    
    plot <- ggplot(df_for_plot, aes(x = CategoryNumber, y = DataValue_cldm, text = LocationDesc)) +
      geom_point(size = 1) +
      labs(title = paste("Chronic Liver Disease Mortality by Category in", input$year),
           x = "Category",
           y = "Chronic liver disease mortality rate (%)") +
      scale_x_continuous(breaks = 1:6) +
      theme_minimal()
    
    # Add trendline if checkbox is selected
    if (input$trendline) {
      plot <- plot + geom_smooth(method = "lm", color = "red", se = FALSE)
    }
    
    # Convert ggplot to plotly
    ggplotly(plot) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white", 
          font = list(color = "black")
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
