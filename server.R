# Install required packages if not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(plotly)) install.packages("plotly")
if (!require(DT)) install.packages("DT")
if (!require(readxl)) install.packages("readxl")

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(readxl)

# Define server logic
function(input, output, session) {
  
# Load data from fixed Excel file
loaded_data <- reactive({
  file_path <- "Traffic flow and emission data.xlsx"
  sheets <- excel_sheets(file_path)
  data_list <- list()
  for(sheet in sheets) {
    data_list[[sheet]] <- read_excel(file_path, sheet = sheet)
  }
  return(data_list)
})
  
# Get current data
current_data <- reactive({
  req(loaded_data(), input$sheet_select)
  data <- loaded_data()[[input$sheet_select]]
  return(data)
})
    
# Main scatter plot
output$traffic_emission_scatter <- renderPlotly({
  data <- current_data()
  req(data)
  traffic_col <- input$traffic_select
  pollutant_col <- input$pollutant_select
    
# Create base plot
p <- ggplot(data, aes(x = .data[[traffic_col]], y = .data[[pollutant_col]]))
    
# Add points
p <- p + geom_point(aes(text = paste("Site:", ID,
                                     "<br>Traffic:", round(.data[[traffic_col]], 1), "veh/h",
                                     "<br>Emission:", round(.data[[pollutant_col]], 2), "g/km/h")), 
                    color = "steelblue", 
                    size = 3, 
                    alpha = 0.7)

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
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 12))
 
ggplotly(p, tooltip = "text") %>% 
  layout(height = 600,
        hoverlabel = list(bgcolor = "white", 
                          font = list(size = 12)),
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
  stats <- stats %>% select(Statistic, Value)
  
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
  stats <- stats %>% select(Statistic, Value)
  
  datatable(stats, 
            options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE),
            rownames = FALSE)
})
  
# Correlation summary
output$correlation_summary <- renderDT({
  data <- current_data()
  req(data)
  
  traffic_col <- input$traffic_select
  pollutant_col <- input$pollutant_select
  
# Calculate correlation
 correlation <- cor(data[[traffic_col]], data[[pollutant_col]], use = "complete.obs")
  
# Create correlation strength description
 strength <- if (abs(correlation) > 0.7) {
  "Strong"
  } else if (abs(correlation) > 0.3) {
    "Moderate"
  } else {
    "Weak"
  }
  
  direction <- if (correlation > 0) "Positive" else "Negative"
  
  summary_df <- data.frame(
    Statistic = c("Correlation Coefficient", "Relationship Strength", "Relationship Direction"),
    Value = c(
      round(correlation, 4),
      strength,
      direction
    )
  )
  
  datatable(summary_df, 
            options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE),
            rownames = FALSE)
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
            options = list(scrollX = TRUE, pageLength = 10),
            caption = "Traffic Flow and Emission Data")
 })
}