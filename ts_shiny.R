library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Time Series Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      # No sidebar inputs in this example
    ),
    
    mainPanel(
      plotOutput("timeSeriesPlot"),
      plotOutput("scatterPlot"),
      plotOutput("boxPlot"),
      plotOutput("densityPlot"),
      plotOutput("seasonalPlot")
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
  
  # Render the time series plot
  output$timeSeriesPlot <- renderPlot({
    ggplot(data, aes(x = time, y = values, color = "Time Series")) +
      geom_line() +
      labs(x = "Time", y = "Values", color = "") +
      theme_minimal() +
      scale_color_manual(values = c("red"))
  })
  
  # Render the scatter plot
  output$scatterPlot <- renderPlot({
    ggplot(data, aes(x = values, y = values, color = "Scatter Plot")) +
      geom_point() +
      labs(x = "Values", y = "Values", color = "") +
      theme_minimal() +
      scale_color_manual(values = c("blue"))
  })
  
  # Render the box plot
  output$boxPlot <- renderPlot({
    ggplot(data, aes(x = "", y = values, fill = "Box Plot")) +
      geom_boxplot() +
      labs(x = "", y = "Values", fill = "") +
      theme_minimal() +
      scale_fill_manual(values = c("green"))
  })
  
  # Render the density plot
  output$densityPlot <- renderPlot({
    ggplot(data, aes(x = values, fill = "Density Plot")) +
      geom_density(alpha = 0.5) +
      labs(x = "Values", y = "Density", fill = "") +
      theme_minimal() +
      scale_fill_manual(values = c("purple"))
  })
  
  # Render the seasonal plot
  output$seasonalPlot <- renderPlot({
    data$month <- format(data$time, "%b")
    ggplot(data, aes(x = month, y = values, group = 1, color = "Seasonal Plot")) +
      geom_line() +
      labs(x = "Month", y = "Values", color = "") +
      theme_minimal() +
      scale_color_manual(values = c("orange"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
