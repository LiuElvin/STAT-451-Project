library(tidyverse)
library(tigris)
library(viridis)
library(sf)

function(input, output) {
  
  df <- read.csv("U.S._Chronic_Disease_Indicators__CDI___2023_Release.csv")
  
  x <- df %>% 
    filter(Topic == "Alcohol") %>% 
    filter(LocationDesc != "District of Columbia") %>% 
    filter(LocationDesc != "Virgin Islands") %>% 
    filter(LocationDesc != "Puerto Rico") %>% 
    filter(LocationDesc != "Guam")
  
  pcac_full <- x %>% 
    filter(Question ==
      "Per capita alcohol consumption among persons aged >= 14 years")
  
  pcac_full$DataValue <- as.numeric(pcac_full$DataValue)
  
  pcac_year_1 <- reactive({
    pcac_full %>%
      filter(YearStart == input$year_one)
  })
  
  pcac_year_2 <- reactive({
    pcac_full %>%
      filter(YearStart == input$year_two)
  })
  
  pcac_diff <- reactive({
    merged_data <- merge(
      pcac_year_1(),
      pcac_year_2(),
      by = "LocationDesc",
      suffixes = c("1", "2")
    ) %>% 
      mutate(diff = DataValue2 - DataValue1) %>% 
      filter(!is.na(DataValue1) & !is.na(DataValue2))
    return(merged_data)
  })
  
  nat_avg <- reactive({
    filter(pcac_diff(), LocationDesc == "United States")$diff
  })
  
  # 0.0046875 gallons of ethanol / std drink
  eth <- 0.0046875
  
  pcac_diff_reactive <- reactive({
    pcac_diff() %>%
      filter(LocationDesc != "United States") %>%
      mutate(bar_color = ifelse(LocationDesc == input$state, "highlight", "normal"))
  })
  
  output$pcac_plot = renderPlot({
    validate(
      need(input$year_one != 2011, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_one != 2012, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_one != 2013, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_one != 2015, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_one != 2017, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_one != 2021, "Error: 2021 is not a valid selection for Year 1!"),
      need(input$year_two != 2011, "Error: 2021 is not a valid selection for Year 2!"),
      need(input$year_two != 2012, "Error: 2021 is not a valid selection for Year 2!"),
      need(input$year_two != 2013, "Error: 2021 is not a valid selection for Year 2!"),
      need(input$year_two != 2015, "Error: 2021 is not a valid selection for Year 2!"),
      need(input$year_two != 2017, "Error: 2021 is not a valid selection for Year 2!"),
      need(input$year_two != 2021, "Error: 2021 is not a valid selection for Year 2!")
    )
    
    selected_data <- pcac_diff_reactive() %>% 
      filter(LocationDesc == input$state)
    
    ordered_data <- pcac_diff_reactive() %>%
      arrange(desc(diff))
    
    seven_state <- ordered_data$LocationDesc[17]
    seven_state_diff <- ordered_data$diff[17]
    
    ggplot(data = pcac_diff_reactive(), 
           mapping = aes(x = reorder(LocationDesc, diff), y = diff, fill = bar_color)) +
      scale_fill_manual(values = c("highlight" = "black", "normal" = "gray")) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = 0, color = "black") +
      geom_hline(yintercept = nat_avg(), color = "red", linetype = "dashed") +
      geom_hline(yintercept = eth * 100, color = "blue", linetype = "dashed") +
      labs(title = paste0("Change in Annual Per Capita Alcohol Consumption from ", 
                          input$year_one, " to ", input$year_two, " Among Persons Aged >= 14 Years"),
           x = "State", y = "Change in Alcohol Consumption (gallons of ethanol / person)") +
      annotate("text", x = seven_state, y = seven_state_diff, 
               label = paste0("National Average Increase = ", nat_avg(), " gallons / person"),
               color = "red", angle = 0, vjust = -1, size = 12/.pt) +
      annotate("text", x = seven_state, y = eth * 100, 
               label = paste0("Difference of 100 Standard Drinks = 0.4687 gallons / person"), 
               color = "blue", angle = 0, vjust = -1, size = 12/.pt) +   
      geom_text(data = selected_data, aes(label = round(diff, 4)),
                vjust = ifelse(selected_data$diff > 0, -0.5, 1.5),
                color = "black", size = 4) +
      scale_y_continuous(breaks = seq(-1, 1, by = 0.2), limits = c(-0.9, 0.9)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5), 
            legend.position = "none",
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            plot.title = element_text(size=20))
  })
  
  output$state_change_summary <- renderText({
    selected_data <- pcac_diff_reactive() %>%
      filter(LocationDesc == input$state)
    
    state_diff <- selected_data$diff
    
    paste0("Annual alcohol consumption in ",
           input$state,
           " has increased by ",
           round(state_diff, 4),
           " gallons of ethanol per capita from ", 
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