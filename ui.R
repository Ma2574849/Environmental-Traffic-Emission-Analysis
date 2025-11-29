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
            choices = c("Off-Peak Hour", "Peak Hour"),
            selected = "Peak Hour"),
# Pollutant selection       
selectInput("pollutant_select", "Select Pollutant:",
            choices = c("CO" = "CO Emission Intensity (g/km/h)"),
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
   
   tabPanel("Distribution",
    fluidRow(
     column(6,
      h4("Traffic Flow Distribution"),
      plotlyOutput("traffic_histogram", height = "300px"),
      plotlyOutput("traffic_boxplot", height = "300px")),
     column(6,
      h4("Emission Intensity Distribution"),
      plotlyOutput("emission_histogram", height = "300px"),
      plotlyOutput("emission_boxplot", height = "300px")))),
   
   tabPanel("Statistics",
    fluidRow(
     column(6,
      h4("CO Statistics"),
      DTOutput("pollution_stats")),
     column(6,
      h4("Traffic Statistics"),
      DTOutput("traffic_stats"))),
br(),
    fluidRow(
     column(8,
      h4("Traffic Statistical Tests"),
      DTOutput("statistical_tests")))),

   tabPanel("Map",
    fluidRow(
     leafletOutput("map_plot", height = "650px"))),

   tabPanel("Vehicle Contribution",
    fluidRow(
     column(10, 
      plotlyOutput("vehicle_contribution_plot", height = "400px"))),
br(),    
    fluidRow(
     column(6, 
      DTOutput("vehicle_contribution_table")))),

   tabPanel("Data Table",
    fluidRow(
      DTOutput("data_table")))
   
   )
  )
)
)