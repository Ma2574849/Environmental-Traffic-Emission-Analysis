# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(readxl)

fluidPage(
    titlePanel("Vehicle Traffic and Emission Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        width = 3,

# Sheet selection
selectInput("sheet_select", "Select Time Period:",
            choices = c("Overall Average", "Off-Peak Hour", "Peak Hour", 
                         "Weekday", "Weekend", "Early Morning"),
            selected = "Overall Average"),
# Pollutant selection       
selectInput("pollutant_select", "Select Pollutant:",
            choices = c("CO" = "CO Emission Intensity (g/km/h)",
                        "HC" = "HC Emission Intensity (g/km/h)",
                        "NOx" = "NOx Emission Intensity (g/km/h)", 
                        "PM2.5" = "PM2.5 Emission Intensity (g/km/h)"),
            selected = "CO Emission Intensity (g/km/h)"),
# Traffic flow selection       
selectInput("traffic_select", "Select Vehicle Type:",
            choices = c("Heavy Duty Vehicles (HDV)" = "HDV Flow (veh/h)",
                        "Light Duty Vehicles (LDV)" = "LDV Flow (veh/h)",
                        "New Energy Vehicles (NEV)" = "NEV Flow (veh/h)", 
                        "Total Vehicle Flow" = "Total Vehicle Flow(veh/h)"),
            selected = "Total Vehicle Flow(veh/h)"),

# Visualization controls
checkboxInput("show_trend",
              "Show Trend Line",
              value = TRUE),
sliderInput("point_size",
            "Point Size:",
            min = 1, max = 10,
            value = 3, step = 0.5),

# Statistics section header
h4("Data Summary"),
helpText("Statistical summaries for selected data")),
 
mainPanel(
  width = 9,
  tabsetPanel(
   tabPanel("Visualization",
    fluidRow(
     plotlyOutput("traffic_emission_scatter", height = "600px"))),
   tabPanel("Statistics",
    fluidRow(
     column(6,
      h4("Pollution Statistics"),
      DTOutput("pollution_stats")),
     column(6,
      h4("Traffic Statistics"),
      DTOutput("traffic_stats"))),
br(),
    fluidRow(
     column(6,
      h4("Correlation Analysis"),
      DTOutput("correlation_summary")))),
   tabPanel("Data Table",
    fluidRow(
      DTOutput("data_table"))),
             )
         )
     )
)