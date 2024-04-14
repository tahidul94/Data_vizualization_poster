library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmap)
library(dplyr)
# To create and save omaha map data from goole
# omaps <-  get_map(location = 'Omaha', maptype = 'roadmap', zoom = 11, color='bw')
# save(omaps, file = "omaps.RData") # get_map(location = 'Omaha', source = 'stamen', maptype = 'toner')
# Once saved, we don't need to connect google, we can just load
# I have done this step already. Just get the omaps.RData from canvas

load("omaps.RData") #obtained using get_map(). you can download it from canvas
crimes <- read.csv("omaha-crimes.csv") # download this data from canvas
crimeDat <- readRDS("usaCrimeDat.rds")


ui <- dashboardPage(
  dashboardHeader(title = "My Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("My control Center", tabName = "control", 
               icon = icon("dashboard")),
      menuItem("My city crime", tabName = "oCrime", 
               icon = icon("th")),
      menuItem("My states crime", tabName = "statesCrime", 
               icon = icon("mask"))
      
    )    
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "control",
              fluidRow(
                box(plotOutput("myPlot", height = 250)),
                box(
                  title = "Controls",
                  sliderInput("slider", 
                              "Number of observations:", 1, 100, 50)
                )
              )
      ),
      # Second tab content
      tabItem(tabName = "oCrime",
              h2("Omaha crime map goes here"),
              fluidRow(
                column(6,box(plotOutput("myMap"), height = 450, width=400)),
                column(6, box(title = "Please select a crime",
                              checkboxGroupInput("crimeType", label="Crime Type",
                                                 choices = unique(crimes$type))
                ))
                
              )
      ),
      #hw4 instruction
      tabItem(tabName = "statesCrime",
              h2("My states crime map goes here"),
              fluidRow(
                column(5,
                       wellPanel(
                         selectInput("selectedCrimeState", "Choose a crime to display",
                                     choices = unique(crimeDat$Crime), selected = unique(crimeDat$Crime)[1]),
                         sliderInput("crimeYearState", "Crime Year", 
                                     min = min(crimeDat$Year), max = max(crimeDat$Year), value = round(mean(range(crimeDat$Year))),step=1)
                       )
                ),
                column(6,
                       # Output for the crime map will be placed here
                       plotOutput("statesCrimeMap")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$myPlot <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$myMap <- renderPlot({
    crimes_sub <- subset(crimes, crimes$type %in% input$crimeType)
    ggmap(omaps) +
      geom_point(size=5, alpha = 1/2, aes(lon,lat, color=type), 
                 data = crimes_sub)
  })
  
  mdat <- map_data("state")
  
  output$statesCrimeMap <- renderPlot({
    # Subset the data based on the selected crime and year
    selectedData <- crimeDat %>%
      filter(crimeDat$Crime == input$selectedCrimeState & crimeDat$Year == input$crimeYearState)
    
    # Merge the map data with the selected crime data
    mergedData <- merge(mdat, selectedData, by.x = c('region'), by.y = c('state'), all.x = TRUE)
    mergedData <- mergedData[order(mergedData$order),]
    # Plot the map
    ggplot(mergedData, aes(x = long, y = lat, group = group, fill = rate)) +
      geom_polygon(color = "white") +
      coord_fixed(1.3) + 
      theme_void() + 
      labs(fill = "Crime Rate (%)") +
      scale_fill_gradient(low = "lightblue", high = "red",na.value = "grey50")
  })
  
  
}

shinyApp(ui, server)
