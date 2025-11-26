# Install required packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(plotly)) install.packages("plotly")
if (!require(DT)) install.packages("DT")
if (!require(readxl)) install.packages("readxl")
if (!require(corrplot)) install.packages("corrplot")
if (!require(psych)) install.packages("psych")
if (!require(leaflet)) install.packages("leaflet")

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(readxl)
library(corrplot)
library(psych)
library(leaflet)

# Define server logic
function(input, output, session) {
  
  # Load data
  loaded_data <- reactive({
    file_path <- "Traffic flow and emission data.xlsx"
    sheets <- excel_sheets(file_path)
    data_list <- list()
    for(sheet in sheets) {
      data_list[[sheet]] <- read_excel(file_path, sheet = sheet)
    }
    return(data_list)
  })
  
  # Get current filtered data
  current_data <- reactive({
    req(loaded_data(), input$sheet_select)
    data <- loaded_data()[[input$sheet_select]]
    
    # Apply filters
    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select
    data <- data %>%
      filter(.data[[traffic_col]] >= input$flow_range[1] & 
               .data[[traffic_col]] <= input$flow_range[2]) %>%
      filter(.data[[pollutant_col]] >= input$emission_range[1] & 
               .data[[pollutant_col]] <= input$emission_range[2])
    return(data)
  })
  
  # Get all numeric data for correlation analysis
  numeric_data <- reactive({
    data <- current_data()
    req(data)
    
    # Select only numeric columns for correlation analysis
    numeric_cols <- sapply(data, is.numeric)
    data_numeric <- data[, numeric_cols]
    
    # Remove columns with zero variance
    variances <- apply(data_numeric, 2, var, na.rm = TRUE)
    data_numeric <- data_numeric[, variances > 0, drop = FALSE]
    
    return(data_numeric)
  })
  
  # Main scatter plot
  output$traffic_emission_scatter <- renderPlotly({
    data <- current_data()
    req(data)
    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select
    
    # Create base plot
    p <- ggplot(data) +
      geom_point(aes(x = .data[[traffic_col]], 
                     y = .data[[pollutant_col]],
                     text = paste("Site:", ID,
                                  "<br>Traffic:", round(.data[[traffic_col]], 1), "veh/h",
                                  "<br>Emission:", round(.data[[pollutant_col]], 2), "g/km/h")),
                 color = "steelblue", size = 3, alpha = 0.7)
    
    # Add trend line if selected
    if (input$show_trend) {
      p <- p + geom_smooth(aes(x = .data[[traffic_col]], 
                               y = .data[[pollutant_col]]), data = data, method = "loess", se = FALSE, color = "darkorange", size = 1)
    } 
    
    # Axis labels
    x_label <- gsub(" Flow (veh/h)", "", traffic_col)
    y_label <- gsub(" Emission Intensity (g/km/h)", "", pollutant_col)
    
    # Add labels and theme
    p <- p +
      labs(title = paste("Relationship between", x_label, "and", y_label),
           subtitle = paste("Time Period:", input$sheet_select),
           x = paste(traffic_col),
           y = paste(pollutant_col)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.title = element_text(size = 10))
    
    ggplotly(p, tooltip = "text", height = 600) %>% 
      layout(hoverlabel = list(bgcolor = "white", 
                               font = list(size = 10)),
             margin = list(t = 80))  
  })
  
  # Pollution statistics
  output$pollution_stats <- renderDT({
    data <- current_data()
    req(data)
    
    pollutant_col <- input$pollutant_select
    
    stats <- data %>%
      summarise(
        Mean = round(mean(.data[[pollutant_col]], na.rm = TRUE), 2),
        Median = round(median(.data[[pollutant_col]], na.rm = TRUE), 2),
        SD = round(sd(.data[[pollutant_col]], na.rm = TRUE), 2),
        Min = round(min(.data[[pollutant_col]], na.rm = TRUE), 2),
        Max = round(max(.data[[pollutant_col]], na.rm = TRUE), 2),
        IQR = round(IQR(.data[[pollutant_col]], na.rm = TRUE), 2)
      ) %>%
      t() %>% as.data.frame() %>%
      rename(Value = V1)
    
    stats$Statistic <- rownames(stats)
    stats <- stats[, c("Statistic", "Value")]
    
    datatable(stats, 
              options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE),
              rownames = FALSE)
  })
  
  # Traffic statistics
  output$traffic_stats <- renderDT({
    data <- current_data()
    req(data)
    
    traffic_col <- input$traffic_select
    
    stats <- data %>%
      summarise(
        Mean = round(mean(.data[[traffic_col]], na.rm = TRUE), 2),
        Median = round(median(.data[[traffic_col]], na.rm = TRUE), 2),
        SD = round(sd(.data[[traffic_col]], na.rm = TRUE), 2),
        Min = round(min(.data[[traffic_col]], na.rm = TRUE), 2),
        Max = round(max(.data[[traffic_col]], na.rm = TRUE), 2),
        IQR = round(IQR(.data[[traffic_col]], na.rm = TRUE), 2)
      ) %>%
      t() %>% as.data.frame() %>%
      rename(Value = V1)
    
    stats$Statistic <- rownames(stats)
    stats <- stats[, c("Statistic", "Value")]
    
    datatable(stats, 
              options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE),
              rownames = FALSE)
  })
  
  # Correlation Analysis - Using Spearman for non-normal data
  output$correlation_summary <- renderDT({
    data <- current_data()
    req(data, nrow(data) > 1)
    
    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select
    
    # Calculate Spearman's rank correlation (non-parametric)
    spearman_cor <- cor.test(data[[traffic_col]], data[[pollutant_col]], 
                             method = "spearman", use = "complete.obs", exact = FALSE)
    
    # Create correlation strength description for Spearman
    strength <- if (abs(spearman_cor$estimate) > 0.7) {
      "Strong"
    } else if (abs(spearman_cor$estimate) > 0.3) {
      "Moderate"
    } else {
      "Weak"
    }
    
    direction <- if (spearman_cor$estimate > 0) "Positive" else "Negative"
    significance <- if (spearman_cor$p.value < 0.001) "***" else 
      if (spearman_cor$p.value < 0.01) "**" else 
        if (spearman_cor$p.value < 0.05) "*" else "Not significant"
    
    summary_df <- data.frame(
      Statistic = c("Correlation Method", 
                    "Spearman's Rank Correlation (ρ)", 
                    "P-value", 
                    "Relationship Strength", 
                    "Relationship Direction",
                    "Statistical Significance", 
                    "Sample Size",
                    "Data Distribution"),
      Value = c(
        "Spearman's Rank",
        round(spearman_cor$estimate, 6),
        format.pval(spearman_cor$p.value, digits = 4),
        strength,
        direction,
        significance,
        format(nrow(data), big.mark = ","),
        "Non-normal"
      )
    )
    
    summary_df <- summary_df[, c("Statistic", "Value")]
    
    datatable(summary_df, 
              options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE),
              rownames = FALSE)
  })
  
  # Statistical Tests output
  output$statistical_tests <- renderDT({
    data <- current_data()
    req(data, nrow(data) > 2)
    
    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select
    
    tryCatch({
      # Get the data for selected columns
      traffic_data <- data[[traffic_col]]
      pollution_data <- data[[pollutant_col]]
      
      # Perform normality tests (Shapiro-Wilk test)
      normality_traffic <- shapiro.test(traffic_data)
      normality_pollution <- shapiro.test(pollution_data)
      
      # Perform Spearman's rank correlation test
      spearman_test <- cor.test(traffic_data, pollution_data, 
                                method = "spearman", exact = FALSE)
      
      # Create correlation strength description
      strength <- if (abs(spearman_test$estimate) > 0.7) {
        "Strong"
      } else if (abs(spearman_test$estimate) > 0.3) {
        "Moderate"
      } else {
        "Weak"
      }
      
      # Determine direction and significance
      direction <- if (spearman_test$estimate > 0) "Positive" else "Negative"
      significance <- if (spearman_test$p.value < 0.001) "***" else 
        if (spearman_test$p.value < 0.01) "**" else 
          if (spearman_test$p.value < 0.05) "*" else "Not significant"
      
      # Create results table
      tests_df <- data.frame(
        Test = c("Sample Size", 
                 "Complete Cases", 
                 "Normality Test (Traffic) - p-value",
                 "Normality Test (Pollution) - p-value",
                 "Data Distribution Assessment",
                 "Spearman's Rank Correlation (ρ)", 
                 "P-value", 
                 "Relationship Strength",
                 "Relationship Direction",
                 "Statistical Significance"),
        Result = c(
          format(nrow(data), big.mark = ","),
          format(length(traffic_data), big.mark = ","),
          format.pval(normality_traffic$p.value, digits = 4),
          format.pval(normality_pollution$p.value, digits = 4),
          ifelse(normality_traffic$p.value < 0.05 | normality_pollution$p.value < 0.05, 
                 "Non-normal distribution", "Normal distribution"),
          round(spearman_test$estimate, 6),
          format.pval(spearman_test$p.value, digits = 4),
          strength,
          direction,
          significance
        ),
        Interpretation = c(
          "Total observations in filtered data",
          "Observations with complete data for both variables",
          "Shapiro-Wilk test for normality (p < 0.05 indicates non-normal)",
          "Shapiro-Wilk test for normality (p < 0.05 indicates non-normal)",
          "Overall assessment of data distribution",
          "Monotonic relationship coefficient (-1 to +1)",
          "Probability of observing this relationship by chance",
          "Strength of the monotonic relationship",
          "Direction of the relationship",
          "Statistical significance level"
        )
      )
      
      # Use base R to select columns
      tests_df <- tests_df[, c("Test", "Result", "Interpretation")]
      
      datatable(tests_df, 
                options = list(
                  dom = 't', 
                  paging = FALSE, 
                  searching = FALSE, 
                  ordering = FALSE,
                  columnDefs = list(
                    list(width = '200px', targets = 0),
                    list(width = '150px', targets = 1),
                    list(width = '300px', targets = 2)
                  )
                ),
                rownames = FALSE) %>%
        formatStyle(columns = c(0, 1, 2), fontSize = '14px')
      
    })
  })
  
  # Data table
  output$data_table <- renderDT({
    data <- current_data()
    req(data)
    
    # Select relevant columns for display
    display_data <- data %>%
      select(ID, Longitude, Latitude, 
             `HDV Flow (veh/h)`, `LDV Flow (veh/h)`, `NEV Flow (veh/h)`, `Total Vehicle Flow(veh/h)`,
             `CO Emission Intensity (g/km/h)`, `HC Emission Intensity (g/km/h)`,
             `NOx Emission Intensity (g/km/h)`, `PM2.5 Emission Intensity (g/km/h)`)
    datatable(display_data, 
              colnames = c(names(display_data)[1:10], 'PM<sub>2.5</sub> Emission Intensity (g/km/h)'),
              escape = FALSE,
              options = list(scrollX = TRUE, pageLength = 10),
              caption = "Traffic Flow and Emission Data")
  })
  
  # Map visualization
  output$map_plot <- renderLeaflet({
    data <- current_data()
    req(data)
    
    traffic_col <- input$traffic_select
    pollutant_col <- input$pollutant_select
    
    # Normalize marker size based on traffic flow
    flow_values <- data[[traffic_col]]
    flow_scaled <- (flow_values - min(flow_values)) / 
      (max(flow_values) - min(flow_values) + 1e-10)
    marker_sizes <- 1 + flow_scaled * 10  
    
    # Color scale for emission
    emission_values <- data[[pollutant_col]]
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = emission_values
    )
    
    # Create popup content without using .data pronoun
    popup_content <- paste0(
      "<b>Site ID: </b>", data$ID, "<br>",
      "<b>Flow: </b>", round(data[[traffic_col]], 1), " veh/h<br>",
      "<b>Emission: </b>", round(data[[pollutant_col]], 2), " g/km/h<br>",
      "<b>Time Period: </b>", input$sheet_select
    )
    
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%  
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = marker_sizes,
        color = ~pal(emission_values),
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = popup_content
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~emission_values,
        title = paste(gsub(" Emission Intensity \\(g/km/h\\)", "", pollutant_col), 
                      "<br>Emission (g/km/h)")
      ) %>%
      setView(lng = mean(data$Longitude, na.rm = TRUE), 
              lat = mean(data$Latitude, na.rm = TRUE), 
              zoom = 10)
  })
}