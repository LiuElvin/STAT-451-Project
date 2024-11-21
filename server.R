library(tidyverse)

# Old Data
# df <- read.csv("U.S._Chronic_Disease_Indicators__CDI___2023_Release.csv")
# alcohol <- df %>% 
#   filter(Topic == "Alcohol") %>% 
#   filter(!LocationDesc %in% c("District of Columbia", "Virgin Islands", "Puerto Rico", "Guam"))

x <- read.csv("alcohol_cdi.csv")

eth <- 0.0046875

validate_years <- function(input) {
  invalid_years <- c(2011, 2012, 2013, 2015, 2017, 2021)
  validate(
    need(!(input$year_one %in% invalid_years), paste("Error:", input$year_one, "is not a valid selection for Year 1!")),
    need(!(input$year_two %in% invalid_years), paste("Error:", input$year_two, "is not a valid selection for Year 2!"))
  )
}

filter_data <- function(data, year, state = NULL) {
  filtered <- data %>% 
    filter(YearStart == year, !is.na(DataValue)) 
  if (!is.null(state)) {
    filtered <- filtered %>% filter(LocationDesc == state)
  }
  return(filtered)
}

function(input, output) {
  pcac_full <- x %>% 
    filter(Question == "Per capita alcohol consumption among persons aged >= 14 years") %>%
    mutate(DataValue = as.numeric(DataValue))
  
  pcac_year <- reactive({
    list(
      year_1 = filter_data(pcac_full, input$year_one),
      year_2 = filter_data(pcac_full, input$year_two)
    )
  })
  
  pcac_diff <- reactive({
    merge(pcac_year()$year_1, pcac_year()$year_2, by = "LocationDesc", suffixes = c("1", "2")) %>%
      mutate(diff = DataValue2 - DataValue1)
  })
  
  pcac_diff_reactive <- reactive({
    pcac_diff() %>%
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state, "highlight", "normal"))
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
  
  # Bar plots
  output$pcac_bar_1 <- renderPlot({
    validate_years(input)
    year_data <- pcac_year()
    year_1 <- year_data$year_1 %>% 
      arrange(desc(DataValue))
    year_2 <- year_data$year_2 %>% 
      arrange(desc(DataValue))
    nat_avg_1 <- filter(year_1, LocationDesc == "United States")$DataValue
    nat_avg_2 <- filter(year_2, LocationDesc == "United States")$DataValue
    intercept <- year_1$LocationDesc[35]
    
    full_bar <- pcac_full %>%
      filter(YearStart == input$year_one) %>% 
      filter(!is.na(DataValue)) %>% 
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state, "highlight", "normal"))
    
    generate_bar_plot(full_bar, year_1, input$year_one, input$state, nat_avg_1, nat_avg_2, "red") +
      annotate(geom = "text", x = intercept, y = nat_avg_1,
               label = paste0(input$year_one, " National Average = ", nat_avg_1, " gallons / person"),
               color = "red", vjust = ifelse(nat_avg_1 >= nat_avg_2, -0.5, +1.5), size = 10/.pt)
  })
  
  output$pcac_bar_2 <- renderPlot({
    validate_years(input)
    year_data <- pcac_year()
    year_1 <- year_data$year_1 %>% 
      arrange(desc(DataValue))
    year_2 <- year_data$year_2 %>% 
      arrange(desc(DataValue))
    nat_avg_1 <- filter(year_1, LocationDesc == "United States")$DataValue
    nat_avg_2 <- filter(year_2, LocationDesc == "United States")$DataValue
    intercept <- year_2$LocationDesc[35]
    
    full_bar <- pcac_full %>%
      filter(YearStart == input$year_two) %>% 
      filter(!is.na(DataValue)) %>% 
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state, "highlight", "normal"))
    
    generate_bar_plot(full_bar, year_2, input$year_two, input$state, nat_avg_1, nat_avg_2, "blue") +
      annotate(geom = "text", x = intercept, y = nat_avg_2,
               label = paste0(input$year_two, " National Average = ", nat_avg_2, " gallons / person"),
               color = "blue", vjust = ifelse(nat_avg_2 > nat_avg_1, -0.5, +1.5), size = 10/.pt)
  })
  
  # Comparison bar plot
  output$pcac_comparison_bar <- renderPlot({
    validate_years(input)
    diff_data <- pcac_diff_reactive() %>% 
      arrange(desc(diff))
    intercept <- diff_data$LocationDesc[40]
    val <- round(filter(diff_data, LocationDesc == input$state)$diff, 4)
    
    ggplot(data = diff_data, mapping = aes(x = reorder(LocationDesc, diff), y = diff, fill = bar_color)) +
      scale_fill_manual(values = c("highlight" = "black", "normal" = "gray")) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = nat_avg(), color = "purple", linetype = "dashed") +
      geom_vline(xintercept = input$state, color = "black", linetype = "dotted") +
      labs(title = paste0("Change in Annual Per Capita Alcohol Consumption from ", 
                          input$year_one, " to ", input$year_two, " Among Persons Aged >= 14 Years"),
           x = "State", 
           y = "Change in Alcohol Consumption (gallons / person)") +
      annotate(geom = "text", x = intercept, y = nat_avg(),
               label = paste0("Change in National Average = ", nat_avg(), " gallons / person"),
               color = "purple", vjust = -1, size = 12/.pt) +
      annotate(geom = "text", x = input$state, y = max(0.05, val + 0.05), label = paste0(val), color = "black", size = 12/.pt) +
      scale_y_continuous(breaks = seq(-1, 1, by = 0.2), limits = c(-1, 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size = 12), axis.text.y = element_text(size = 10),
            axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 12),
            plot.title = element_text(size = 16),
            legend.position = "none")
  })
  
  # Summary outputs
  output$state_norm_summary <- renderText({
    specific_value_1 <- filter_data(pcac_full, input$year_one, input$state) %>% pull(DataValue)
    specific_value_2 <- filter_data(pcac_full, input$year_two, input$state) %>% pull(DataValue)
    paste0("In ", input$state, ", per capita alcohol consumption was ",
           specific_value_1, " gallons of ethanol in ", input$year_one, 
           " and ", specific_value_2, " gallons of ethanol in ", input$year_two, 
           ", equivalent to approximately ", round(specific_value_1 / eth, 2), 
           " and ", round(specific_value_2 / eth, 2), " standard drinks, respectively.")
  })
  
  output$state_change_summary <- renderText({
    diff_value <- pcac_diff_reactive() %>% filter(LocationDesc == input$state) %>% pull(diff)
    paste0("From ", input$year_one, " to ", input$year_two, ", per capita alcohol consumption among persons aged 14 years and older in ",
           input$state, " increased by ", round(diff_value, 4), " gallons of pure ethanol. This is equivalent to ",
           round(diff_value / eth, 2), " additional standard drinks consumed per person in ", input$year_two, " compared to ", input$year_one, ".")
  })
}