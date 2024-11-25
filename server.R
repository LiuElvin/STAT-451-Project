library(tidyverse)
library(viridis)
library(tigris)
library(plotly)
library(sf)

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
  # Code for pcac tab
  pcac_full <- x %>% 
    filter(Question == "Per capita alcohol consumption among persons aged >= 14 years") %>%
    mutate(DataValue = as.numeric(DataValue))
  
  pcac_year <- reactive({
    list(year_1 = filter_data(pcac_full, input$year_one_1), year_2 = filter_data(pcac_full, input$year_two_1))
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
  
  # Code for auay tab
  stratification_values_2 <- reactive({
    filtered_values <- x[x$StratificationCategory1 == input$stratification_category_2, ]
    unique(filtered_values$Stratification1)
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
  })
  
  output$auay_linePlot <- renderPlotly({
    df_selected_state <- filtered_data_1()
    
    df_all_states <- x[x$Question == "Alcohol use among youth" &
                         x$StratificationCategory1 == input$stratification_category_2 &
                         x$Stratification1 == input$stratification_2 &
                         !is.na(x$DataValueAlt), ]
    
    if (nrow(df_all_states) == 0) {
      return(NULL)
    }
    
    max_val <- max(df_all_states$DataValueAlt, na.rm = TRUE)
    
    plot <- ggplot() +
      geom_line(data = df_all_states,
                aes(x = YearStart, y = DataValueAlt, group = LocationDesc,
                    text = paste0("Region: ", LocationDesc, " <br>Usage: ", DataValueAlt, "% <br>Year: ", YearStart, " ")),
                color = "grey", alpha = 0.2) +
      geom_point(data = df_all_states, 
                 aes(x = YearStart, y = DataValueAlt, group = LocationDesc,
                     text = paste0("Region: ", LocationDesc, " <br>Usage: ", DataValueAlt, "% <br>Year: ", YearStart, " ")),
                 color = "grey", alpha = 0.2) +
      labs(title = paste("Alcohol usage among youth by (odd) years for", input$stratification_2),
           x = "Year",
           y = "Alcohol usage among youth (Crude Prevalence %)") +
      theme_minimal() +
      scale_x_continuous(breaks = c(2013, 2015, 2017, 2019)) +
      scale_y_continuous(limits = c(0, max_val))
    
    if (!is.null(df_selected_state) & nrow(df_selected_state) > 0) {
      plot <- plot +
        geom_line(data = df_selected_state,
                  aes(x = YearStart, y = DataValueAlt),
                  color = "blue", size = 1) +
        geom_point(data = df_selected_state,
                   aes(x = YearStart, y = DataValueAlt,
                       text = paste0("Region: ", LocationDesc, " <br>Usage: ", DataValueAlt, "% <br>Year: ", YearStart, " ")),
                   color = "blue", size = 3) +
        labs(title = paste("Alcohol usage among youth by (odd) years in", input$state_2, "for", input$stratification_2))
    }
    
    if (input$trendline_2) {
      plot <- plot +
        geom_line(data = filter(df_all_states, LocationDesc == "United States"),
                  aes(x = YearStart, y = DataValueAlt, color = "National Average"), size = 1) +
        
        geom_point(data = filter(df_all_states, LocationDesc == "United States"),
                   aes(x = YearStart, y = DataValueAlt,
                       text = paste0("Region: United States <br>Usage: ", DataValueAlt, "% <br>Year: ", YearStart, " ")),
                   color = "red", size = 3) +
        
        scale_color_manual(name = "", values = c("National Average" = "red")) 
    }
    
    ggplotly(plot, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
  })
  
  output$auay_summary <- renderText({
    if (sum(is.na(filter(x, Question == "Alcohol use among youth",
                         StratificationCategory1 == input$stratification_category_2,
                         Stratification1 == input$stratification_2,
                         LocationDesc == input$state_2)$DataValueAlt)) == nrow(filter(x, Question == "Alcohol use among youth",
                                                                                      StratificationCategory1 == input$stratification_category_2,
                                                                                      Stratification1 == input$stratification_2,
                                                                                      LocationDesc == input$state_2))) {
      
      return("Unfortunately no data values were in the original dataset for the specified state, data type, and stratification.")
    }
    
    df_for_plot <- filtered_data_1()
    
    df_for_plot <- df_for_plot %>% 
      mutate(DataValue = as.numeric(DataValue)) %>% 
      arrange(YearStart)
    
    summary_message <- "Note: The only years recorded are 2013, 2015, 2017, and 2019, with some states containing only a subset of these years depending on the stratification."
    
    for (i in 1:nrow(df_for_plot)) {
      val <- df_for_plot$DataValue[i]
      
      summary_message <- paste0(summary_message, "<br><br>Year (", df_for_plot$YearStart[i], "):<br> Crude Prevalence of Alcohol Usage Among Youth = ", round(val, 4), "%")
    }
    
    summary_message <- paste0(summary_message, "<br><br>", "Any years that are not listed were not provided by the original dataset.")
    HTML(summary_message)
  })
  
  output$us_summary <- renderText({
    us_data <- filter(x, Question == "Alcohol use among youth",
                      StratificationCategory1 == input$stratification_category_2,
                      Stratification1 == input$stratification_2,
                      LocationDesc == "United States")
    
    if (sum(is.na(us_data$DataValueAlt)) == nrow(us_data)) {
      return("Unfortunately, no data values are available for the United States. Here is an empty graph!")
    }
    
    us_data <- us_data %>%
      mutate(DataValue = as.numeric(DataValue)) %>%
      arrange(YearStart)
    
    summary_message <- "Note: The only years recorded for the United States are 2013, 2015, 2017, and 2019, with some years potentially missing depending on the stratification."
    
    for (i in 1:nrow(us_data)) {
      val <- us_data$DataValue[i]
      
      summary_message <- paste0(summary_message, "<br><br>Year (", us_data$YearStart[i], "):<br> Crude Prevalence of Alcohol Usage Among Youth = ", round(val, 4), "%")
    }
    
    summary_message <- paste0(summary_message, "<br><br>", "Any years that are not listed were not provided by the original dataset.")
    HTML(summary_message)
  })
  
  # Code for csar tab
  stratification_values_3 <- reactive({
    filtered_values <- x[x$StratificationCategory1 == input$stratification_category_3, ]
    unique(filtered_values$Stratification1)
  })
  
  observe({
    updateSelectInput(session, "stratification_3", choices = stratification_values_3())
  })
  
  filtered_data <- reactive({
    df_year <- x[x$YearStart == input$year_3, ]
    
    df_year_csar <- df_year[df_year$Question == "Chronic liver disease mortality" & 
                              df_year$DataValueType == input$data_type_3 & 
                              df_year$StratificationCategory1 == input$stratification_category_3 &
                              df_year$Stratification1 == input$stratification_3, ]
    
    df_regulation <- x %>%
      filter(YearStart == input$year_3,
             Question == "Local authority to regulate alcohol outlet density", 
             DataValue != "")
    
    result_df <- df_regulation %>%
      select(YearStart, LocationDesc, DataValue) %>%
      left_join(df_year_csar %>% select(YearStart, LocationDesc, DataValueAlt), 
                by = c("LocationDesc")) %>%
      rename(DataValue_regulation = DataValue, 
             Mortality = DataValueAlt)
    
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
      mutate(Category = category_mapping[DataValue_regulation])
  })
  
  output$csar_plot <- renderPlotly({
    df_for_plot <- filtered_data()
    
    if (nrow(x %>% filter(DataValueType == input$data_type_3,
                          StratificationCategory1 == input$stratification_category_3,
                          Stratification1 == input$stratification_3)) == 0) {
      return(NULL)
    }
    
    if (input$plot_3 == "Scatterplot") {
      plot <- ggplot(df_for_plot, aes(x = Category, y = Mortality, text = paste0("State: ", LocationDesc))) +
        geom_point(size = 1) +
        labs(title = paste("Scatterplot of Chronic Liver Disease Mortality by Category in", input$year_3, "for", input$stratification_3),
             x = "Category of State Alcohol Regulation",
             y = paste(input$data_type_3, "for Chronic Liver Disease Mortality (cases per 100000)")) +
        scale_y_continuous(limits = c(0, max(df_for_plot$Mortality))) +
        scale_x_continuous(breaks = 1:6) +
        theme(axis.title.y = element_text(size = 8))
      
      if (input$trendline_3) {
        mean_data <- df_for_plot %>%
          group_by(Category) %>%
          summarize(mean = mean(Mortality, na.rm = TRUE), LocDesc = LocationDesc)
        
        plot <- plot + geom_line(data = mean_data, aes(x = Category, y = mean), inherit.aes = FALSE, color = "red", size = 1)
      }
      
      ggplotly(plot) %>%
        layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
    }
    else {
        num_categories <- length(unique(df_for_plot$Category))
        
        colors <- colorRampPalette(c("yellow", "red"))(num_categories)
        
        plot <- ggplot(df_for_plot, aes(x = as.factor(Category), y = Mortality, fill = as.factor(Category))) +
          geom_boxplot() +
          labs(title = paste("Boxplot of Chronic Liver Disease Mortality by Category in", input$year_3, "for", input$stratification_3),
               x = "Category of State Alcohol Regulation",
               y = paste(input$data_type_3, "for Chronic Liver Disease Mortality (cases per 100000)")) +
          scale_y_continuous(limits = c(0, max(df_for_plot$Mortality, na.rm = TRUE))) +
          scale_fill_manual(values = colors) +
          theme(axis.title.y = element_text(size = 8), legend.position = "none")
        
        if (input$trendline_3) {
          median_data <- df_for_plot %>%
            group_by(Category) %>%
            summarize(median = median(Mortality, na.rm = TRUE), LocDesc = LocationDesc)
          
          plot <- plot + geom_line(data = median_data, aes(x = Category, y = median), inherit.aes = FALSE, color = "black", size = 1, linetype = "dashed")
        }
        
        ggplotly(plot) %>%
          layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
      }
  })
  
  output$cat_1 <- renderUI({
    mssg <- paste0("Category 1 - State had exclusive local alcohol retail licensing", "<br><br>",
                   "Category 2 - State had joint local and state alcohol retail licensing", "<br><br>",
                   "Category 3 - State had exclusive state alcohol retail licensing but with local zoning authority", "<br><br>",
                   "Category 4 - State had mixed alcohol retail licensing policies", "<br><br>",
                   "Category 5 - State had nearly exclusive state alcohol retail licensing", "<br><br>",
                   "Category 6 - State had exclusive state alcohol retail licensing")
    HTML(mssg)
  })
  
  output$cat_2 <- renderUI({
    if (nrow(x %>% filter(DataValueType == input$data_type_3,
                     StratificationCategory1 == input$stratification_category_3,
                     Stratification1 == input$stratification_3)) == 0) {
      return("Unfortunately no data values were in the original dataset for the specified year, data type, and stratification. Here is an empty graph!")
    }
    
    df_for_plot <- filtered_data()
    
    mean_data <- df_for_plot %>%
      group_by(Category) %>%
      summarize(mean_csar = mean(Mortality, na.rm = TRUE))
    
    if (sum(is.na(mean_data$mean_csar)) > 0) {
      return("There is no state data for certain categories, resulting in discontinuous trendlines.")
    }
    
    summary_message <- ""
    
    for (i in 1:nrow(mean_data)) {
      mean_value <- mean_data$mean_csar[i]
      
      if (i != 1) {
        summary_message <- paste0(summary_message, "<br><br>")
      }
      
      interpretation <- if (mean_value < 20) {
        "This category has a relatively low mortality rate."
      } else if (mean_value < 30) {
        "This category has a moderate mortality rate."
      } else {
        "This category has a high mortality rate."
      }
      
      summary_message <- paste0(summary_message, "Category ", i, ":<br> Mean Mortality Rate = ", round(mean_value, 2), " - ", interpretation)
    }
    
    HTML(summary_message)
  })
  
  # Code for cldm tab
  filter_1 <- function(y) {
    df_year <- x[x$YearStart == y, ]
    
    df_cldm <- df_year[df_year$Question == "Chronic liver disease mortality" & 
                         df_year$DataValueType == input$data_type_4 & 
                         df_year$StratificationCategory1 == "Overall" &
                         df_year$Stratification1 == "Overall", ]
  }
  
  prep_plot <- function(y) {
    us_states <- states(cb = TRUE)
    
    df_for_plot <- filter_1(y)
    
    df_states <- filter(df_for_plot, LocationDesc != "United States")
    
    reposition_states <- function(geometry) {
      alaska <- geometry %>% filter(NAME == "Alaska")
      hawaii <- geometry %>% filter(NAME == "Hawaii")
      alaska <- st_geometry(alaska) * 0.35
      alaska <- alaska + c(-65, 7)
      hawaii <- st_geometry(hawaii) + c(50, 7)
      mainland <- geometry %>% filter(!NAME %in% c("Alaska", "Hawaii"))
      st_geometry(geometry[geometry$NAME == "Alaska", ]) <- alaska
      st_geometry(geometry[geometry$NAME == "Hawaii", ]) <- hawaii
      geometry
    }
    
    us_states_repositioned <- reposition_states(us_states)
    
    us_states_update <- us_states_repositioned %>%
      right_join(df_states, by = c("NAME" = "LocationDesc"))
    
    us_states_update$DataValue <- as.numeric(us_states_update$DataValue)
    
    us_states_update <- us_states_update %>% 
      rename(Mortality = DataValue)
    
    selected_state <- us_states_update %>% filter(NAME == input$state_4)
    
    word <- ""
    
    if (input$data_type_4 == "Number") {
      word <- "Total Deaths"
    }
    else {
      word <- "Deaths per 100,000"
    }
    
    plot <- ggplot(us_states_update) +
      geom_sf(aes(fill = Mortality, text = paste0("State: ", NAME, " <br>Mortality: ", Mortality, " <br>Year: ", y, " ")), color = "white") +
      scale_fill_gradient(low = "yellow", high = "red2", name = word) +
      geom_sf(data = selected_state, aes(fill = Mortality, text = paste0("State: ", NAME, " <br>Mortality: ", Mortality, " <br>Year: ", y, " ")),
              color = "black") +
      labs(title = paste0("U.S. States Heatmap of Chronic Liver Disease Mortality in ", y)) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank()) +
      coord_sf(xlim = c(-125, -65), ylim = c(25, 49))
      
    ggplotly(plot, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(color = "black")))
  }
  
  output$cldm_plot_1 <- renderPlotly({
    prep_plot(input$year_4_1)
  })
  
  output$cldm_plot_2 <- renderPlotly({
    prep_plot(input$year_4_2)
  })
  
  output$cldm_state <- renderUI({
    df_for_plot_1 <- filter_1(input$year_4_1)
    df_for_plot_2 <- filter_1(input$year_4_2)
    
    selected_state_info_1 <- filter(df_for_plot_1, LocationDesc == input$state_4)$DataValue
    selected_state_info_2 <- filter(df_for_plot_2, LocationDesc == input$state_4)$DataValue
    
    unit <- ""
    
    if (input$data_type_4 == "Number") {
      unit <- "total deaths"
    }
    else {
      unit <- "deaths per 100,000"
    }
    
    summary_message <- paste0(
      "In the year ", input$year_4_1, ", the chronic liver disease mortality rate for ", input$state_4, " was ", 
      round(sum(as.numeric(selected_state_info_1), na.rm = TRUE), 2), " ", unit, ".<br><br>",
      "In the year ", input$year_4_2, ", the chronic liver disease mortality rate for ", input$state_4, " was ", 
      round(sum(as.numeric(selected_state_info_2), na.rm = TRUE), 2), " ", unit, "."
    )
    
    HTML(summary_message)
  })
  
  output$cldm_us <- renderUI({
    df_for_plot_1 <- filter_1(input$year_4_1)
    df_for_plot_2 <- filter_1(input$year_4_2)
    
    selected_us_info_1 <- filter(df_for_plot_1, LocationDesc == "United States")$DataValue
    selected_us_info_2 <- filter(df_for_plot_2, LocationDesc == "United States")$DataValue
    
    unit <- ""
    
    if (input$data_type_4 == "Number") {
      unit <- "total deaths"
    }
    else {
      unit <- "deaths per 100,000"
    }
    
    summary_message <- paste0(
      "In the year ", input$year_4_1, ", the chronic liver disease mortality rate for the United States was ", 
      round(sum(as.numeric(selected_us_info_1), na.rm = TRUE), 2), " ", unit, ".<br><br>",
      "In the year ", input$year_4_2, ", the chronic liver disease mortality rate for the United States was ", 
      round(sum(as.numeric(selected_us_info_2), na.rm = TRUE), 2), " ", unit, "."
    )
    
    HTML(summary_message)
  })
}
