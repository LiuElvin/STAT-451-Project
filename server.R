library(tidyverse)
library(tigris)
library(viridis)
library(sf)

function(input, output) {
  # Filters
  df <- read.csv("U.S._Chronic_Disease_Indicators__CDI___2023_Release.csv")
  
  x <- df %>% 
    filter(Topic == "Alcohol") %>% 
    filter(LocationDesc != "District of Columbia") %>% 
    filter(LocationDesc != "Virgin Islands") %>% 
    filter(LocationDesc != "Puerto Rico") %>% 
    filter(LocationDesc != "Guam")
  
  # pcac
  #
  #
  pcac_full <- x %>% 
    filter(Question ==
      "Per capita alcohol consumption among persons aged >= 14 years")
  
  pcac_full$DataValue <- as.numeric(pcac_full$DataValue)
  
  pcac_year_1 <- reactive({
    pcac_full %>%
      filter(YearStart == input$year_one) %>% 
      filter(!is.na(DataValue))
  })
  
  pcac_year_2 <- reactive({
    pcac_full %>%
      filter(YearStart == input$year_two) %>% 
      filter(!is.na(DataValue))
  })
  
  pcac_diff <- reactive({
    merged_data <- merge(
      pcac_year_1(),
      pcac_year_2(),
      by = "LocationDesc",
      suffixes = c("1", "2")
    ) %>% 
      mutate(diff = DataValue2 - DataValue1)
    return(merged_data)
  })
  
  nat_avg <- reactive({
    filter(pcac_diff(), LocationDesc == "United States")$diff
  })
  
  eth <- 0.0046875
  
  pcac_diff_reactive <- reactive({
    pcac_diff() %>%
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state, "highlight", "normal"))
  })
  
  nat_avg_1 <- reactive({
    filter(pcac_year_1(), LocationDesc == "United States")$DataValue
  })
  
  nat_avg_2 <- reactive({
    filter(pcac_year_2(), LocationDesc == "United States")$DataValue
  })
  
  pcac_year_1_bar_1 <- reactive({
    pcac_full %>%
      filter(YearStart == input$year_one) %>% 
      filter(!is.na(DataValue)) %>% 
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state, "highlight", "normal"))
  })
  
  pcac_year_2_bar_2 <- reactive({
    pcac_full %>%
      filter(YearStart == input$year_two) %>% 
      filter(!is.na(DataValue)) %>% 
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state, "highlight", "normal"))
  })
  
  # pcac barplot 1
  output$pcac_bar_1 = renderPlot({
    validate(
      need(input$year_one != 2011, "Error: 2011 is not a valid selection for Year 1!"),
      need(input$year_one != 2012, "Error: 2022 is not a valid selection for Year 1!"),
      need(input$year_one != 2013, "Error: 2013 is not a valid selection for Year 1!"),
      need(input$year_one != 2015, "Error: 2015 is not a valid selection for Year 1!"),
      need(input$year_one != 2017, "Error: 2017 is not a valid selection for Year 1!"),
      need(input$year_one != 2021, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_two != 2011, "Error: 2011 is not a valid selection for Year 2!"),
      need(input$year_two != 2012, "Error: 2012 is not a valid selection for Year 2!"),
      need(input$year_two != 2013, "Error: 2013 is not a valid selection for Year 2!"),
      need(input$year_two != 2015, "Error: 2015 is not a valid selection for Year 2!"),
      need(input$year_two != 2017, "Error: 2017 is not a valid selection for Year 2!"),
      need(input$year_two != 2021, "Error: 2021 is not a valid selection for Year 2!")
    )
    
    year_1 <- pcac_year_1()
    
    selected_data_1 <- year_1 %>% 
      filter(LocationDesc == input$state)
    
    specific_data_1 <- selected_data_1$DataValue
    
    year_2 <- pcac_year_2()
    
    selected_data_2 <- year_2 %>% 
      filter(LocationDesc == input$state)
    
    specific_data_2 <- selected_data_2$DataValue
    
    ordered_data <- year_1 %>%
      arrange(desc(DataValue))
    
    axis_state <- ordered_data$LocationDesc[25]
    
    ggplot(data = pcac_year_1_bar_1(), mapping = aes(x = reorder(LocationDesc, DataValue), y = DataValue, fill = bar_color)) +
      scale_fill_manual(values = c("highlight" = "red", "normal" = "gray")) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = specific_data_1, color = "red", linetype = "dashed") +
      geom_hline(yintercept = specific_data_2, color = "blue", linetype = "dashed") +
      geom_vline(xintercept = input$state, color = "black", linetype = "dotted") +
      labs(title = paste0(input$year_one, " Annual Per Capita Alcohol Consumption Among Persons Aged >= 14 Years"),
           x = "State",
           y = "Alcohol Consumption (gallons / person)") +
      annotate(geom = "text",
               x = axis_state,
               y = specific_data_1,
               label = paste0(specific_data_1, " gallons / person"),
               color = "red",
               angle = 0,
               vjust = -1,
               size = 14/.pt) +
      scale_y_continuous(breaks = seq(0, 5, by = 1),
                         limits = c(0, 5)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5),
            legend.position = "none",
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            plot.title = element_text(size = 12))
  })
  
  # pcac barplot 2
  output$pcac_bar_2 = renderPlot({
    validate(
      need(input$year_one != 2011, "Error: 2011 is not a valid selection for Year 1!"),
      need(input$year_one != 2012, "Error: 2022 is not a valid selection for Year 1!"),
      need(input$year_one != 2013, "Error: 2013 is not a valid selection for Year 1!"),
      need(input$year_one != 2015, "Error: 2015 is not a valid selection for Year 1!"),
      need(input$year_one != 2017, "Error: 2017 is not a valid selection for Year 1!"),
      need(input$year_one != 2021, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_two != 2011, "Error: 2011 is not a valid selection for Year 2!"),
      need(input$year_two != 2012, "Error: 2012 is not a valid selection for Year 2!"),
      need(input$year_two != 2013, "Error: 2013 is not a valid selection for Year 2!"),
      need(input$year_two != 2015, "Error: 2015 is not a valid selection for Year 2!"),
      need(input$year_two != 2017, "Error: 2017 is not a valid selection for Year 2!"),
      need(input$year_two != 2021, "Error: 2021 is not a valid selection for Year 2!")
    )
    
    year_1 <- pcac_year_1()
    
    selected_data_1 <- year_1 %>% 
      filter(LocationDesc == input$state)
    
    specific_data_1 <- selected_data_1$DataValue
    
    year_2 <- pcac_year_2()
    
    selected_data_2 <- year_2 %>% 
      filter(LocationDesc == input$state)
    
    specific_data_2 <- selected_data_2$DataValue
    
    ordered_data <- year_2 %>%
      arrange(desc(DataValue))
    
    axis_state <- ordered_data$LocationDesc[25]
    
    ggplot(data = pcac_year_2_bar_2(), mapping = aes(x = reorder(LocationDesc, DataValue), y = DataValue, fill = bar_color)) +
      scale_fill_manual(values = c("highlight" = "blue", "normal" = "gray")) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = specific_data_1, color = "red", linetype = "dashed") +
      geom_hline(yintercept = specific_data_2, color = "blue", linetype = "dashed") +
      geom_vline(xintercept = input$state, color = "black", linetype = "dotted") +
      labs(title = paste0(input$year_two, " Annual Per Capita Alcohol Consumption Among Persons Aged >= 14 Years"),
           x = "State",
           y = "Alcohol Consumption (gallons / person)") +
      annotate(geom = "text",
               x = axis_state,
               y = specific_data_2,
               label = paste0(specific_data_2, " gallons / person"),
               color = "blue",
               angle = 0,
               vjust = -1,
               size = 14/.pt) +
      scale_y_continuous(breaks = seq(0, 5, by = 1),
                         limits = c(0, 5)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5),
            legend.position = "none",
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            plot.title = element_text(size = 12))
  })
  
  # pcac comparison bar plot
  output$pcac_comparison_bar = renderPlot({
    validate(
      need(input$year_one != 2011, "Error: 2011 is not a valid selection for Year 1!"),
      need(input$year_one != 2012, "Error: 2022 is not a valid selection for Year 1!"),
      need(input$year_one != 2013, "Error: 2013 is not a valid selection for Year 1!"),
      need(input$year_one != 2015, "Error: 2015 is not a valid selection for Year 1!"),
      need(input$year_one != 2017, "Error: 2017 is not a valid selection for Year 1!"),
      need(input$year_one != 2021, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_two != 2011, "Error: 2011 is not a valid selection for Year 2!"),
      need(input$year_two != 2012, "Error: 2012 is not a valid selection for Year 2!"),
      need(input$year_two != 2013, "Error: 2013 is not a valid selection for Year 2!"),
      need(input$year_two != 2015, "Error: 2015 is not a valid selection for Year 2!"),
      need(input$year_two != 2017, "Error: 2017 is not a valid selection for Year 2!"),
      need(input$year_two != 2021, "Error: 2021 is not a valid selection for Year 2!")
    )
    
    diff_reactive <- pcac_diff_reactive()
    
    selected_data <- diff_reactive %>% 
      filter(LocationDesc == input$state)
    
    ordered_data <- diff_reactive %>%
      arrange(desc(diff))
    
    axis_state <- ordered_data$LocationDesc[25]
    axis_state_diff <- ordered_data$diff[25]
    
    ggplot(data = pcac_diff_reactive(), mapping = aes(x = reorder(LocationDesc, diff),y = diff, fill = bar_color)) +
      scale_fill_manual(values = c("highlight" = "black", "normal" = "gray")) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 0, color = "black") +
      geom_hline(yintercept = nat_avg(), color = "purple", linetype = "dashed") +
      geom_vline(xintercept = input$state, color = "black", linetype = "dotted") +
      labs(title = paste0("Change in Annual Per Capita Alcohol Consumption from ",
                          input$year_one,
                          " to ",
                          input$year_two,
                          " Among Persons Aged >= 14 Years"),
           x = "State",
           y = "Change in Alcohol Consumption (gallons / person)") +
      annotate(geom = "text",
               x = axis_state,
               y = nat_avg(),
               label = paste0("Change in National Average = ", nat_avg(), " gallons / person"),
               color = "purple",
               angle = 0,
               vjust = -1,
               size = 14/.pt) +
      geom_text(data = selected_data,
                aes(label = round(diff, 4)),
                vjust = ifelse(selected_data$diff > 0, -0.5, 1.5),
                color = "black", size = 5) +
      scale_y_continuous(breaks = seq(-1, 1, by = 0.2),
                         limits = c(-1, 1)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5),
            legend.position = "none",
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 20))
  })
  
  # Summary at the top
  output$state_norm_summary <- renderText({
    year_1 <- pcac_year_1()
    
    selected_data_1 <- year_1 %>% 
      filter(LocationDesc == input$state)
    
    specific_data_1 <- selected_data_1$DataValue
    
    year_2 <- pcac_year_2()
    
    selected_data_2 <- year_2 %>% 
      filter(LocationDesc == input$state)
    
    specific_data_2 <- selected_data_2$DataValue
    
    paste0("In the state of ",
           input$state,
           ", the annual per capita consumption rate of alcohol among individuals aged 14 years and over was ",
           specific_data_1,
           " gallons of ethanol / person in ",
           input$year_one,
           " and ",
           specific_data_2,
           " gallons of ethanol / person in ",
           input$year_two,
           ". In other words, the average person from ",
           input$state,
           " drank around ",
           round(specific_data_1 / eth, 4),
           " standard drinks in ",
           input$year_one,
           " and ",
           round(specific_data_2 / eth, 4),
           " standard drinks in ",
           input$year_two,
           ".")
  })
  
  # Summary at the top
  output$state_change_summary <- renderText({
    selected_data <- pcac_diff_reactive() %>%
      filter(LocationDesc == input$state)
    
    state_diff <- selected_data$diff
    
    paste0("The per capita alcohol consumption in ",
           input$state,
           " has increased by ",
           round(state_diff, 4),
           " gallons of ethanol / persons aged 14 years and older from ", 
           input$year_one,
           " to ",
           input$year_two,
           ". On average, this is equivalent to each person in ",
           input$state,
           " drinking ",
           round(state_diff / eth, 4),
           " standard drinks more in ",
           input$year_one,
           " compared to ",
           input$year_two,
           ".")
  })
}