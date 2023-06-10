library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Time Series Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Select a Plot:"),
      actionButton("plotTimeSeries", "Time Series Plot"),
      actionButton("plotScatter", "Scatter Plot"),
      actionButton("plotBox", "Box Plot"),
      actionButton("plotDensity", "Density Plot"),
      actionButton("plotSeasonal", "Seasonal Plot"),
      br(),
      h3("Plot Explanations:"),
      verbatimTextOutput("plotExplanation")
    ),
    
    mainPanel(
      plotOutput("selectedPlot")
    )
  )
)

# Define server
server <- function(input, output) {
  # Generate random time series data
  set.seed(123)
  time <- seq(as.Date("2020-01-01"), by = "month", length.out = 12)
  values <- rnorm(12)
  data <- data.frame(time, values)
  
  # Define plot explanations
  plotExplanations <- list(
    "Time Series Plot" = "This plot displays the time series data as a line chart.",
    "Scatter Plot" = "This plot shows the values on both the x-axis and y-axis.",
    "Box Plot" = "This plot provides a visual representation of the value distribution.",
    "Density Plot" = "This plot illustrates the estimated probability density function.",
    "Seasonal Plot" = "This plot displays the values across different months."
  )
  
  # Render selected plot
  observeEvent(input$plotTimeSeries, {
    output$selectedPlot <- renderPlot({
      plotTimeSeries()
    })
  })
  
  observeEvent(input$plotScatter, {
    output$selectedPlot <- renderPlot({
      plotScatter()
    })
  })
  
  observeEvent(input$plotBox, {
    output$selectedPlot <- renderPlot({
      plotBox()
    })
  })
  
  observeEvent(input$plotDensity, {
    output$selectedPlot <- renderPlot({
      plotDensity()
    })
  })
  
  observeEvent(input$plotSeasonal, {
    output$selectedPlot <- renderPlot({
      plotSeasonal()
    })
  })
  
  # Time series plot
  plotTimeSeries <- function() {
    ggplot(data, aes(x = time, y = values, color = "Time Series")) +
      geom_line() +
      labs(x = "Time", y = "Values", color = "") +
      theme_minimal() +
      scale_color_manual(values = c("blue"))
  }
  
  # Scatter plot
  plotScatter <- function() {
    ggplot(data, aes(x = values, y = values, color = "Scatter Plot")) +
      geom_point() +
      labs(x = "Values", y = "Values", color = "") +
      theme_minimal() +
      scale_color_manual(values = c("red"))
  }
  
  # Box plot
  plotBox <- function() {
    ggplot(data, aes(x = "", y = values, fill = "Box Plot")) +
      geom_boxplot() +
      labs(x = "", y = "Values", fill = "") +
      theme_minimal() +
      scale_fill_manual(values = c("green"))
  }
  
  # Density plot
  plotDensity <- function() {
    ggplot(data, aes(x = values, fill = "Density Plot")) +
      geom_density(alpha = 0.5) +
      labs(x = "Values", y = "Density", fill = "") +
      theme_minimal() +
      scale_fill_manual(values = c("purple"))
  }
  
  # Seasonal plot
  plotSeasonal <- function() {
    data$month <- format(data$time, "%b")
    ggplot(data, aes(x = month, y = values, group = 1, color = "Seasonal Plot")) +
      geom_line() +
      labs(x = "Month", y = "Values", color = "") +
      theme_minimal() +
      scale_color_manual(values = c("orange"))
  }
  
  # Render plot explanation
  output$plotExplanation <- renderText({
    plotExplanations[input$selectedButton]
  })
}

# Run the app
shinyApp(ui = ui, server = server)
