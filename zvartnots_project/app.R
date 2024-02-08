library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(shinyjs)
library(rsconnect)

# Read data from CSV
data <- read.csv("merged_data.csv", stringsAsFactors = FALSE)
data$OnTime <- data$Status %in% c('Onetime', 'Ontime')
data$FlightStatus <- ifelse(data$OnTime, 'On-time', 'Delayed')

# Convert 'Date' to Date type with the correct format
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Flight Data Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Flight Status Distribution", tabName = "tab_plot1"),
      menuItem("Number of Flights by Country", tabName = "tab_plot2"),
      menuItem("Airlines", tabName = "tab_plot3"),
      menuItem("Number of Flights Over Time", tabName = "tab_plot4"),  # Existing menu item
      menuItem("Armenian vs. Non-Armenian Flights", tabName = "tab_plot6")
    ),
    useShinyjs()  # Initialize shinyjs
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "tab_plot1",
        fluidPage(
          sliderInput("startDate1", "Start Date:", min = min(data$Date), max = max(data$Date), value = min(data$Date), step = 1),
          sliderInput("endDate1", "End Date:", min = min(data$Date), max = max(data$Date), value = max(data$Date), step = 1),
          selectizeInput("countries1", "Countries:", choices = unique(data$country), selected = unique(data$country)[1], multiple = TRUE),
          selectizeInput("airlines1", "Airlines:", choices = unique(data$Airline), selected = unique(data$Airline)[1], multiple = TRUE),
          plotlyOutput("plot1")
        )
      ),
      tabItem(
        tabName = "tab_plot2",
        fluidPage(
          sliderInput("startDate2", "Start Date:", min = min(data$Date), max = max(data$Date), value = min(data$Date), step = 1),
          sliderInput("endDate2", "End Date:", min = min(data$Date), max = max(data$Date), value = max(data$Date), step = 1),
          selectizeInput("countries2", "Countries:", choices = unique(data$country), selected = unique(data$country)[1], multiple = TRUE),
          plotlyOutput("plot2")
        )
      ),
      tabItem(
        tabName = "tab_plot3",  # New tab for the market share plot
        fluidPage(
          sliderInput("startDate3", "Start Date:", min = min(data$Date), max = max(data$Date), value = min(data$Date), step = 1),
          sliderInput("endDate3", "End Date:", min = min(data$Date), max = max(data$Date), value = max(data$Date), step = 1),
          plotlyOutput("marketSharePlot")  # New plot for market share
        )
      ),
      tabItem(
        tabName = "tab_plot4",  # New tab for the time series plot
        fluidPage(
          sliderInput("startDate4", "Start Date:", min = min(data$Date), max = max(data$Date), value = min(data$Date), step = 1),
          sliderInput("endDate4", "End Date:", min = min(data$Date), max = max(data$Date), value = max(data$Date), step = 1),
          selectizeInput("airlines1", "Airlines:", choices = unique(data$Airline), selected = unique(data$Airline)[1], multiple = TRUE),
          plotlyOutput("timeSeriesPlot")
        )
      ),
      tabItem(
        tabName = "tab_plot6",  # New tab for Armenian vs. Non-Armenian flights
        fluidPage(
          sliderInput("startDate6", "Start Date:", min = min(data$Date), max = max(data$Date), value = min(data$Date), step = 1),
          sliderInput("endDate6", "End Date:", min = min(data$Date), max = max(data$Date), value = max(data$Date), step = 1),
          selectInput("flightType6", "Flight Type:", choices = c("Armenian", "Non-Armenian"), selected = "Armenian"),
          plotlyOutput("dynamicFlightCountPlot")
        )
      )
    )
  )
)



# Define server logic
server <- function(input, output, session) {
  
  
  
  # Filtered data for Plot 1
  filteredDataPlot1 <- reactive({
    req(input$startDate1, input$endDate1, input$countries1, input$airlines1)
    
    data %>%
      filter(!is.na(Date), !is.na(country), !is.na(Airline),
             Date >= min(data$Date, na.rm = TRUE) & Date <= max(data$Date, na.rm = TRUE) &
               Date >= input$startDate1 & Date <= input$endDate1 &
               country %in% input$countries1 &
               Airline %in% input$airlines1 &
               Status %in% c('Ontime', 'Cancelled', 'Earlier', 'Delay')) %>%
      group_by(Status) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
  })
  
  # Filtered data for Plot 2
  filteredDataPlot2 <- reactive({
    req(input$startDate2, input$endDate2, input$countries2)
    
    data %>%
      filter(!is.na(Date), !is.na(country),
             Date >= min(data$Date, na.rm = TRUE) & Date <= max(data$Date, na.rm = TRUE) &
               Date >= input$startDate2 & Date <= input$endDate2 &
               country %in% input$countries2) %>%
      group_by(country) %>%
      summarise(NumFlights = n()) %>%
      arrange(desc(NumFlights))
  })
  
  # Plot 1: Flight Status Distribution
  output$plot1 <- renderPlotly({
    gg <- ggplot(filteredDataPlot1(), aes(x = Status, y = Count)) +
      geom_col() +
      labs(title = "Flight Status Distribution",
           x = "Status", y = "Count") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))  # Center the title
    
    ggplotly(gg, tooltip = "y") %>%
      layout(hovermode = "x")
  })
  
  # Plot 2: Number of Flights by Country
  output$plot2 <- renderPlotly({
    gg <- ggplot(filteredDataPlot2(), aes(x = NumFlights, y = country)) +
      geom_col() +
      labs(title = "Number of Flights by Country",
           x = "Number of Flights", y = "Country") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5))  # Center the title
    
    ggplotly(gg, tooltip = "x") %>%
      layout(hovermode = "x")
  })
  filteredDataMarketShare <- reactive({
    req(input$startDate3, input$endDate3)
    
    data %>%
      filter(!is.na(Date),
             Date >= min(data$Date, na.rm = TRUE) & Date <= max(data$Date, na.rm = TRUE) &
               Date >= input$startDate3 & Date <= input$endDate3) %>%
      group_by(Airline) %>%
      summarise(NumFlights = n())
  })
  
  # Market Share Plot
  
  
  output$marketSharePlot <- renderPlotly({
    marketShare <- filteredDataMarketShare() %>%
      arrange(desc(NumFlights))
    
    # Adjust the width as needed
    
    # Create a ggplot object with vertical bars and a box plot
    gg <- ggplot(marketShare, aes(x = Airline, y = NumFlights)) +
      geom_bar(stat = 'identity') +
      theme_classic() +
      theme(axis.text.x = element_text()) +
      coord_flip() 
    # Convert ggplot object to plotly
    p <- ggplotly(gg, width = 800, height = 550, tooltip = "y")
    
    # Customize layout
    p <- p %>% layout(title = 'Airlines',
                      xaxis = list(title = 'Number of Flights'),
                      yaxis = list(title = 'Airline'),
                      margin = list(l = 50, r = 50, b = 50, t = 50))
    
    p
  })
  filteredDataTimeSeries <- reactive({
    req(input$startDate4, input$endDate4)
    
    data %>%
      filter(!is.na(Date),
             Date >= input$startDate4 & Date <= input$endDate4) %>%
      group_by(Date) %>%
      summarise(NumFlights = n())
  })
  
  # Time Series Plot
  output$timeSeriesPlot <- renderPlotly({
    gg <- ggplot(filteredDataTimeSeries(), aes(x = Date, y = NumFlights)) +
      geom_line() +
      labs(title = "Number of Flights Over Time",
           x = "Date", y = "Number of Flights") +
      theme_minimal()
    
    ggplotly(gg, tooltip = "y")
  })
  filteredDataFlightPaths <- reactive({
    req(input$startDate5, input$endDate5, input$countries5, input$airlines5)
    
    data %>%
      filter(!is.na(Date), !is.na(country), !is.na(Airline),
             Date >= min(data$Date, na.rm = TRUE) & Date <= max(data$Date, na.rm = TRUE) &
               Date >= input$startDate5 & Date <= input$endDate5 &
               country %in% input$countries5 &
               Airline %in% input$airlines5 &
               Status %in% c('Ontime', 'Cancelled', 'Earlier', 'Delay')) %>%
      group_by(Status) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))
  })
  output$flightMap <- renderPlotly({
    geo <- list(
      scope = 'world',
      projection = list(type = 'mercator'),  # Change projection to 'mercator'
      showland = TRUE,
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray80")
    )
    
    # Calculate the number of flights for each destination
    flights_count <- data %>% group_by(Destination) %>% summarise(num_flights = n())
    
    # Merge the counts back into the original data
    data_with_counts <- left_join(data, flights_count, by = "Destination")
    
    # Create the plot with circles sized based on the number of flights
    fig <- plot_geo(locationmode = 'ISO-3')
    fig <- fig %>% add_markers(
      data = data_with_counts, x = ~lng, y = ~lat, text = ~paste(Destination, "<br>Count: ", num_flights),
      size = ~num_flights,  # Size circles based on the number of flights
      hoverinfo = "text", alpha = 0.5, color = I("red")  # Set the color of markers to red
    )
    fig <- fig %>% add_segments(
      data = data_with_counts,
      x = ~lng, xend = 44.3993,
      y = ~lat, yend = 40.1520,
      alpha = 0.5, size = I(0.5), hoverinfo = "none", line = list(alpha = 0.5)  # Set the width of joining lines to 0.5
    )
    fig <- fig %>% layout(
      title = 'Flight Paths Map',
      geo = geo, showlegend = FALSE, height = 800
    )
    
    fig
    
  })
  filteredDataArmenianVsNonArmenian <- reactive({
    data %>%
      filter(Date >= input$startDate6, Date <= input$endDate6) %>%
      filter(if (input$flightType6 == "Armenian") Armenian == 1 else Armenian == 0) %>%
      group_by(Airline) %>%
      summarise(Count = n())
  })
  
  # Render the bar chart for Armenian vs. Non-Armenian Flight Count
  output$dynamicFlightCountPlot <- renderPlotly({
    gg <- ggplot(filteredDataArmenianVsNonArmenian(), aes(x = Airline, y = Count)) +
      geom_bar(stat = 'identity') +
      labs(title = "Flight Count by Airline",
           x = "Airline", y = "Count") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
    
    ggplotly(gg)  # Convert ggplot object to plotly
  })
  filteredDataWeekdayAnalysis <- reactive({
    req(input$startDate7, input$endDate7, input$airlines7, input$statusFilter)
    
    data %>%
      filter(!is.na(Date), !is.na(Airline),
             Date >= input$startDate7 & Date <= input$endDate7 &
               Airline %in% input$airlines7 &
               case_when(
                 input$statusFilter == 0 ~ TRUE,  # All
                 input$statusFilter == 1 ~ Status == "Delay",
                 input$statusFilter == 2 ~ Status == "Ontime",
                 input$statusFilter == 3 ~ Status == "Cancelled",
                 input$statusFilter == 4 ~ Status == "Earlier"
               )) %>%
      mutate(Weekday = factor(weekdays(Date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
      group_by(Weekday) %>%
      summarise(NumFlights = n()) %>%
      arrange(Weekday)  # Arrange the weekdays in the specified order
  })
  # Render the weekday analysis plot
  output$weekdayPlot <- renderPlotly({
    gg <- ggplot(filteredDataWeekdayAnalysis(), aes(x = Weekday, y = NumFlights)) +
      geom_col() +
      labs(title = "Number of Flights by Weekday",
           x = "Weekday", y = "Number of Flights") +
      theme_minimal()
    
    ggplotly(gg, tooltip = "y")
  })
  data$IsDelayed <- data$Status == 'Delay'
  ordered_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  filteredData <- reactive({
    temp_data <- data
    if(input$airlineInput != "All") {
      temp_data <- temp_data %>% filter(Airline == input$airlineInput)
    }
    temp_data %>% 
      filter(DayOfWeek %in% input$daysOfWeek) %>%
      mutate(DayOfWeek = factor(DayOfWeek, levels = ordered_days)) # Set the order for days
  })
  output$delayPlot <- renderPlotly({
    delay_by_day <- filteredData()
    if(nrow(delay_by_day) > 0) {
      delay_by_day <- delay_by_day %>%
        group_by(DayOfWeek) %>%
        summarise(DelayedFlights = sum(IsDelayed, na.rm = TRUE),
                  TotalFlights = n(),
                  DelayProportion = DelayedFlights / TotalFlights * 100) %>%
        ungroup()
      p <- ggplot(delay_by_day, aes(x = DayOfWeek, y = DelayProportion, group = 1)) +
        geom_bar(stat = "identity") +
        labs(x = "Day of the Week", y = "Proportion of Delayed Flights (%)",
             title = "Proportion of Delayed Flights by Day of the Week") +
        theme_classic()
      
      
      ggplotly(p)
    } else {
      ggplotly(ggplot())
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)




