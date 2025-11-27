# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(readxl)
library(leaflet)

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
                        "PM\u2082.\u2085" = "PM2.5 Emission Intensity (g/km/h)"),
            selected = "CO Emission Intensity (g/km/h)"),
# Traffic flow selection       
selectInput("traffic_select", "Select Vehicle Type:",
            choices = c("Heavy Duty Vehicles (HDV)" = "HDV Flow (veh/h)",
                        "Light Duty Vehicles (LDV)" = "LDV Flow (veh/h)",
                        "New Energy Vehicles (NEV)" = "NEV Flow (veh/h)", 
                        "Total Vehicle Flow" = "Total Vehicle Flow(veh/h)"),
            selected = "Total Vehicle Flow(veh/h)"),

# Range slection
sliderInput("flow_range", "Traffic Flow Range (veh/h):",
            min = 0, max = 27000, value = c(0, 27000), step = 100),
sliderInput("emission_range", "Emission Intensity Range (g/km/h):",
            min = 0, max = 16000, value = c(0, 16000), step = 100),

# Visualization controls
checkboxInput("show_trend", "Show Trend Line", value = TRUE),

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
      h4("Statistical Tests"),
      DTOutput("statistical_tests")))),
   tabPanel("Map",
    fluidRow(
     leafletOutput("map_plot", height = "650px"))),
   tabPanel("Data Table",
    fluidRow(
      DTOutput("data_table")))
   
   )
  )
)
)