library(tidyverse)
library(plotly)

# Old Data
# df <- read.csv("U.S._Chronic_Disease_Indicators__CDI___2023_Release.csv")
# alcohol <- df %>% 
#   filter(Topic == "Alcohol") %>% 
#   filter(!LocationDesc %in% c("District of Columbia", "Virgin Islands", "Puerto Rico", "Guam"))

x <- read.csv("alcohol_cdi.csv")

eth <- 0.0046875

filter_data <- function(data, year, state = NULL) {
  filtered <- data %>% 
    filter(YearStart == year, !is.na(DataValue)) 
  if (!is.null(state)) {
    filtered <- filtered %>% filter(LocationDesc == state)
  }
  return(filtered)
}

function(input, output, session) {
  # Code for 1st tab
  pcac_full <- x %>% 
    filter(Question == "Per capita alcohol consumption among persons aged >= 14 years") %>%
    mutate(DataValue = as.numeric(DataValue))
  
  pcac_year <- reactive({
    list(
      year_1 = filter_data(pcac_full, input$year_one_1),
      year_2 = filter_data(pcac_full, input$year_two_1)
    )
  })
  
  pcac_diff <- reactive({
    merge(pcac_year()$year_1, pcac_year()$year_2, by = "LocationDesc", suffixes = c("1", "2")) %>%
      mutate(diff = DataValue2 - DataValue1)
  })
  
  pcac_diff_reactive <- reactive({
    pcac_diff() %>%
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state_1, "highlight", "normal"))
  })
  
  nat_avg <- reactive({
    pcac_diff() %>% filter(LocationDesc == "United States") %>% pull(diff)
  })
  
  generate_bar_plot <- function(full_bar, year_data, year, state, nat_avg_1, nat_avg_2, focus_color) {
    val <- round(filter(year_data, LocationDesc == state)$DataValue, 4)
    
    ggplot(data = full_bar, mapping = aes(x = reorder(LocationDesc, DataValue), y = DataValue, fill = bar_color)) +
      scale_fill_manual(values = c("highlight" = focus_color, "normal" = "gray")) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = nat_avg_1, color = "red", linetype = "dashed") +
      geom_hline(yintercept = nat_avg_2, color = "blue", linetype = "dashed") +
      geom_vline(xintercept = state, color = "black", linetype = "dotted") +
      labs(title = paste0(year, " Annual Per Capita Alcohol Consumption Among Persons Aged >= 14 Years"),
           x = "State",
           y = "Alcohol Consumption (gallons / person)") +
      annotate(geom = "text", x = state, y = val + 0.1, label = paste0(val), color = "black", size = 10/.pt) +
      scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0, 5)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 10), 
            axis.text.y = element_text(size = 8),
            axis.title.x = element_text(size = 12), 
            axis.title.y = element_text(size = 12),
            plot.title = element_text(size = 12),
            legend.position = "none")
  }
  
  output$pcac_bar_1 <- renderPlot({
    year_data <- pcac_year()
    year_1 <- year_data$year_1 %>% 
      arrange(desc(DataValue))
    year_2 <- year_data$year_2 %>% 
      arrange(desc(DataValue))
    nat_avg_1 <- filter(year_1, LocationDesc == "United States")$DataValue
    nat_avg_2 <- filter(year_2, LocationDesc == "United States")$DataValue
    intercept <- year_1$LocationDesc[35]
    full_bar <- pcac_full %>%
      filter(YearStart == input$year_one_1) %>% 
      filter(!is.na(DataValue)) %>% 
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state_1, "highlight", "normal"))
    
    generate_bar_plot(full_bar, year_1, input$year_one_1, input$state_1, nat_avg_1, nat_avg_2, "red") +
      annotate(geom = "text", x = intercept, y = nat_avg_1,
               label = paste0(input$year_one_1, " National Average = ", nat_avg_1, " gallons / person"),
               color = "red", vjust = ifelse(nat_avg_1 >= nat_avg_2, -0.5, +1.5), size = 10/.pt)
  })
  
  output$pcac_bar_2 <- renderPlot({
    year_data <- pcac_year()
    year_1 <- year_data$year_1 %>% 
      arrange(desc(DataValue))
    year_2 <- year_data$year_2 %>% 
      arrange(desc(DataValue))
    nat_avg_1 <- filter(year_1, LocationDesc == "United States")$DataValue
    nat_avg_2 <- filter(year_2, LocationDesc == "United States")$DataValue
    intercept <- year_2$LocationDesc[35]
    full_bar <- pcac_full %>%
      filter(YearStart == input$year_two_1) %>% 
      filter(!is.na(DataValue)) %>% 
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state_1, "highlight", "normal"))
    
    generate_bar_plot(full_bar, year_2, input$year_two_1, input$state_1, nat_avg_1, nat_avg_2, "blue") +
      annotate(geom = "text", x = intercept, y = nat_avg_2,
               label = paste0(input$year_two_1, " National Average = ", nat_avg_2, " gallons / person"),
               color = "blue", vjust = ifelse(nat_avg_2 > nat_avg_1, -0.5, +1.5), size = 10/.pt)
  })
  
  output$pcac_comparison_bar <- renderPlot({
    diff_data <- pcac_diff_reactive() %>% 
      arrange(desc(diff))
    intercept <- diff_data$LocationDesc[40]
    val <- round(filter(diff_data, LocationDesc == input$state_1)$diff, 4)
    
    ggplot(data = diff_data, mapping = aes(x = reorder(LocationDesc, diff), y = diff, fill = bar_color)) +
      scale_fill_manual(values = c("highlight" = "black", "normal" = "gray")) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = nat_avg(), color = "purple", linetype = "dashed") +
      geom_vline(xintercept = input$state_1, color = "black", linetype = "dotted") +
      labs(title = paste0("Change in Annual Per Capita Alcohol Consumption from ", 
                          input$year_one_1, " to ", input$year_two_1, " Among Persons Aged >= 14 Years"),
           x = "State", 
           y = "Change in Alcohol Consumption (gallons / person)") +
      annotate(geom = "text", x = intercept, y = nat_avg(),
               label = paste0("Change in National Average = ", nat_avg(), " gallons / person"),
               color = "purple", vjust = -1, size = 12/.pt) +
      annotate(geom = "text", x = input$state_1, y = max(0.05, val + 0.05), label = paste0(val), color = "black", size = 12/.pt) +
      scale_y_continuous(breaks = seq(-1, 1, by = 0.2), limits = c(-1, 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 12), axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 12),
            plot.title = element_text(size = 16),
            legend.position = "none")
  })
  
  output$state_norm_summary <- renderText({
    specific_value_1 <- filter_data(pcac_full, input$year_one_1, input$state_1) %>% pull(DataValue)
    specific_value_2 <- filter_data(pcac_full, input$year_two_1, input$state_1) %>% pull(DataValue)
    paste0("In ", input$state_1, ", per capita alcohol consumption was ",
           specific_value_1, " gallons of ethanol in ", input$year_one_1, 
           " and ", specific_value_2, " gallons of ethanol in ", input$year_two_1, 
           ", equivalent to approximately ", round(specific_value_1 / eth, 2), 
           " and ", round(specific_value_2 / eth, 2), " standard drinks, respectively.")
  })
  
  output$state_change_summary <- renderText({
    diff_value <- pcac_diff_reactive() %>% filter(LocationDesc == input$state_1) %>% pull(diff)
    paste0("From ", input$year_one_1, " to ", input$year_two_1, ", per capita alcohol consumption among persons aged 14 years and older in ",
           input$state_1, " increased by ", round(diff_value, 4), " gallons of pure ethanol. This is equivalent to ",
           round(diff_value / eth, 2), " additional standard drinks consumed per person in ", input$year_two_1, " compared to ", input$year_one_1, ".")
  })
  
  # Code for 2nd tab
  stratification_values_2 <- reactive({
    filtered_values <- x[x$StratificationCategory1 == input$stratification_category_2, ]
    return(unique(filtered_values$Stratification1))
  })
  
  observe({
    updateSelectInput(session, "stratification_2", choices = stratification_values_2())
  })
  
  filtered_data_1 <- reactive({
    df_state <- x[x$LocationDesc == input$state_2, ]
    df_state <- df_state[df_state$Question == "Alcohol use among youth" &
                         df_state$StratificationCategory1 == input$stratification_category_2 &
                         df_state$Stratification1 == input$stratification_2 &
                         !is.na(df_state$DataValueAlt), ]
    return(df_state)
  })
  
  output$auay_scatterPlot <- renderPlotly({
    df_for_plot <- filtered_data_1()
    
    if (is.null(df_for_plot)) {
      return(NULL)
    }
    
    max_val <- max(df_for_plot$DataValueAlt, na.rm = TRUE)
    
    plot <- ggplot(df_for_plot, aes(x = YearStart, y = DataValueAlt)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Alcohol usage among youth by year in", input$state_2),
           x = "Year",
           y = "Alcohol usage among youth (Crude Prevalence)") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, max_val))
    
    if (input$trendline_2) {
      plot <- plot + geom_smooth(method = "lm", color = "red", se = FALSE)
    }
    
    ggplotly(plot) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white", 
          font = list(color = "black")
        )
      )
  })
  
  # Code for 3rd tab
  stratification_values_3 <- reactive({
    filtered_values <- x[x$StratificationCategory1 == input$stratification_category_3, ]
    unique(filtered_values$Stratification1)
  })
  
  observe({
    updateSelectInput(session, "stratification_3", choices = stratification_values_3())
  })
  
  filtered_data <- reactive({
    df_year <- x[x$YearStart == input$year_3, ]
    
    df_year_cldm <- df_year[df_year$Question == "Chronic liver disease mortality" & 
                              df_year$DataValueType == input$data_type_3 & 
                              df_year$StratificationCategory1 == input$stratification_category_3 &
                              df_year$Stratification1 == input$stratification_3, ]
    
    df_regulation <- x %>%
      filter(YearStart == input$year_3,
             Question == "Local authority to regulate alcohol outlet density", 
             DataValue != "")
    
    result_df <- df_regulation %>%
      select(YearStart, LocationDesc, DataValue) %>%
      left_join(df_year_cldm %>% select(YearStart, LocationDesc, DataValueAlt), 
                by = c("LocationDesc")) %>%
      rename(DataValue_regulation = DataValue, 
             DataValue_cldm = DataValueAlt)
    
    if (nrow(result_df) == 0) {
      return(NULL)
    }
    
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
  
  output$cldm_scatterPlot <- renderPlotly({
    df_for_plot <- filtered_data()
    
    if (is.null(df_for_plot)) {
      return(NULL)
    }
    
    plot <- ggplot(df_for_plot, aes(x = CategoryNumber, y = DataValue_cldm, text = LocationDesc)) +
      geom_point(size = 1) +
      labs(title = paste("Chronic Liver Disease Mortality by Category in", input$year_3),
           x = "Category of State Alcohol Regulation",
           y = paste(input$data_type_3, "for Chronic Liver Disease Mortality (cases per 100000)")) +
      scale_y_continuous(limits = c(0, max(df_for_plot$DataValue_cldm))) +
      scale_x_continuous(breaks = 1:6) +
      theme(axis.title.y = element_text(size = 8))
    
    if (input$trendline_3) {
      mean_data <- df_for_plot %>%
        group_by(CategoryNumber) %>%
        summarize(mean = mean(DataValue_cldm, na.rm = TRUE), LocDesc = LocationDesc)
      
      plot <- plot + geom_line(data = mean_data, aes(x = CategoryNumber, y = mean), inherit.aes = FALSE, color = "red", size = 1)
    }
    
    ggplotly(plot) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white", 
          font = list(color = "black")
        )
      )
  })
  
  output$cat <- renderUI({
    if (nrow(x %>% filter(DataValueType == input$data_type_3,
                     StratificationCategory1 == input$stratification_category_3,
                     Stratification1 == input$stratification_3)) == 0) {
      return("No data values meet the restrictions! Empty graph!")
    }
    
    df_for_plot <- filtered_data()
    mean_data <- df_for_plot %>%
      group_by(CategoryNumber) %>%
      summarize(mean_cldm = mean(DataValue_cldm, na.rm = TRUE))
    
    if (sum(is.na(mean_data$mean_cldm)) > 0) {
      return("There is no state data for certain categories, resulting in discontinuous trendlines.")
    }
    
    summary_message <- "Summary of Chronic Liver Disease Mortality by Category:<br>"
    
    for (i in 1:nrow(mean_data)) {
      cat <- if (i == 1) {
        "Category 1 - State had exclusive local alcohol retail licensing"
      } else if (i == 2) {
        "Category 2 - State had joint local and state alcohol retail licensing"
      } else if (i == 3) {
        "Category 3 - State had exclusive state alcohol retail licensing but with local zoning authority"
      } else if (i == 4) {
        "Category 4 - State had mixed alcohol retail licensing policies"
      } else if (i == 5) {
        "Category 5 - State had nearly exclusive state alcohol retail licensing"
      } else if (i == 6) {
        "Category 6 - State had exclusive state alcohol retail licensing"
      }
      
      mean_value <- mean_data$mean_cldm[i]
      
      interpretation <- if (mean_value < 10) {
        "This category has a relatively low mortality rate."
      } else if (mean_value < 20) {
        "This category has a moderate mortality rate."
      } else {
        "This category has a high mortality rate."
      }
      
      summary_message <- paste0(summary_message, "<br>", cat, ".<br>Mean Mortality Rate = ",
                                round(mean_value, 2), "% - ", interpretation, "<br>")
    }
    
    HTML(summary_message)
  })
}