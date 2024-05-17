# Load necessary libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)

# Load the data (ensure this is done outside the server function to avoid reloading)
leaflet_data <- read_csv("leaflet_data.csv")

# Ensure longitude and latitude columns are numeric
leaflet_data$BEGIN_LAT <- as.numeric(leaflet_data$BEGIN_LAT)
leaflet_data$BEGIN_LON <- as.numeric(leaflet_data$BEGIN_LON)
leaflet_data$END_LAT <- as.numeric(leaflet_data$END_LAT)
leaflet_data$END_LON <- as.numeric(leaflet_data$END_LON)

# Define UI for application
ui <- fluidPage(
  titlePanel("Leaflet Map of Events in Texas"),
  
  tabsetPanel(
    tabPanel("Texas Map",
             # Sidebar layout with a sidebar and main panel
             sidebarLayout(
               sidebarPanel(
                 helpText("This app displays events in Texas from 2022 to present on a leaflet map."),
                 
                 # Input: Radio buttons for Event Type
                 radioButtons("event_type", "Event Type:",
                              choices = unique(leaflet_data$EVENT_TYPE),
                              selected = unique(leaflet_data$EVENT_TYPE)[1]),
                 
                 # Input: Select Month Name
                 selectInput("month_name", "Month:",
                             choices = unique(leaflet_data$MONTH_NAME),
                             selected = unique(leaflet_data$MONTH_NAME),
                             multiple = TRUE),
                 
                 # Input: Slider for Event Count
                 uiOutput("event_count_ui")
               ),
               
               mainPanel(
                 leafletOutput("mymap"),
                 textOutput("event_count_display")
               )
             )
    ),
    tabPanel("Model",
             fluidRow(
               column(6, img(src = "observed_vs_predicted_total_events.png", height = "300px", width = "100%")),
               column(6, img(src = "fit2.png", height = "300px", width = "100%"))
             )
    ),
    tabPanel("Interesting Visuals",
             fluidRow(
               column(4, img(src = "pp.png", height = "300px", width = "100%")),
               column(4, img(src = "tornado_time1.png", height = "300px", width = "100%")),
               column(4, img(src = "damage.png", height = "300px", width = "100%"))
             )
    ),
    tabPanel("Future Work",
             fluidRow(
               column(12, 
                      h3("Future Work"),
                      p("We will continue to work on this dashboard and improve it.")
               )
             )
    ),
    tabPanel("About Us",
             fluidRow(
               column(12, 
                      h3("About Us"),
                      p("Created by:"),
                      p("Md Tahidul Islam"),
                      p("Reyhaneh Bolouri"),
                      p("Sai Varsha Sreeperumbudur"),
                      wellPanel(
                        p("We would like to thank Dr. Majumder for the opportunity to learn and develop our Data Visual skills!")
                      )
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    leaflet_data %>%
      filter(EVENT_TYPE == input$event_type,
             MONTH_NAME %in% input$month_name,
             !is.na(BEGIN_LAT) & !is.na(BEGIN_LON))
  })
  
  # Update sliderInput for event count based on the filtered data
  output$event_count_ui <- renderUI({
    max_events <- nrow(filtered_data())
    sliderInput("event_count", "Number of Events to Display:",
                min = 1, max = max_events, value = min(100, max_events))
  })
  
  # Create a reactive leaflet map
  output$mymap <- renderLeaflet({
    leaflet(filtered_data() %>% head(input$event_count)) %>%
      setView(lng = -99.9018, lat = 31.9686, zoom = 6) %>%  # Center on Texas
      addTiles() %>%
      addMarkers(
        lng = ~BEGIN_LON,
        lat = ~BEGIN_LAT,
        popup = ~paste("Category:", EVENT_TYPE, "<br>",
                       "Month:", MONTH_NAME, "<br>",
                       "Start Lat:", BEGIN_LAT, "<br>",
                       "Start Lon:", BEGIN_LON, "<br>",
                       "End Lat:", END_LAT, "<br>",
                       "End Lon:", END_LON)
      )
  })
  
  # Display the current number of events
  output$event_count_display <- renderText({
    event_count <- nrow(filtered_data() %>% head(input$event_count))
    paste("Displaying", event_count, "events.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
