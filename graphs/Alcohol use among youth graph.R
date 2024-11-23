# Load necessary libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(plotly)


# Load the dataset
df_alcohol <- read.csv("alcohol_cdi.csv")

# Define available State/Territories 
available_states <- sort(unique(df_alcohol[df_alcohol$Question == "Alcohol use among youth", ]$LocationDesc))

# Define available Stratification Category
available_stratification_category <- sort(unique(df_alcohol[df_alcohol$Question == "Alcohol use among youth", ]$StratificationCategory1))

# Define the Shiny app UI
ui <- fluidPage(
  # Application title
  titlePanel("Alcohol use among youth over the years by state"),
  
  # Sidebar with inputs for filtering and customizing the plot
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select State", choices = available_states),
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
    df_state <- df_alcohol[df_alcohol$LocationDesc == input$state, ]
    
    # Filter for Chronic Liver Disease Mortality data
    df_state <- df_state[df_state$Question == "Alcohol use among youth" &
                         df_state$StratificationCategory1 == input$stratification_category &
                         df_state$Stratification1 == input$stratification, ]
    
    return(df_state)
  })
  
  # Render the interactive plot using plotly
  output$scatterPlot <- renderPlotly({
    df_for_plot <- filtered_data()
    
    if (is.null(df_for_plot)) {
      return(NULL)
    }
    
    # Find the max of DataValueAlt for scaling
    max_val <- max(df_for_plot$DataValueAlt, na.rm = TRUE)
    
    plot <- ggplot(df_for_plot, aes(x = YearStart, y = DataValueAlt)) +
      geom_point(size = 1) +
      labs(title = paste("Alcohol usage among youth by year in", input$state),
           x = "Year",
           y = "Alcohol usage among youth (Crude Prevalence)") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, max_val))
    
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
