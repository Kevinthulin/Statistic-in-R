# app.R - Time Series Analysis & Forecasting Shiny App

# Load required libraries
library(shiny)
library(shinydashboard)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Time Series Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("arrow-trend-up"))
    ),
    
    # Data selection inputs
    conditionalPanel(
      condition = "input.sidebar == 'data'",
      selectInput("dataSource", "Select Data Source:",
                  choices = c("Built-in Dataset" = "builtin",
                             "Upload CSV" = "upload",
                             "Synthetic Data" = "synthetic")),
      
      # Conditional inputs based on data source selection
      conditionalPanel(
        condition = "input.dataSource == 'builtin'",
        selectInput("builtinData", "Dataset:",
                   choices = c("AirPassengers", "sunspot.year", "UKgas", "WWWusage"))
      ),
      
      conditionalPanel(
        condition = "input.dataSource == 'upload'",
        fileInput("uploadFile", "Choose CSV File",
                  accept = c("text/csv", "text/comma-separated-values", ".csv")),
        numericInput("timeColumn", "Time Column Index (0 if none):", 0),
        numericInput("valueColumn", "Value Column Index:", 1),
        numericInput("frequency", "Data Frequency (12=monthly, 4=quarterly):", 12),
        numericInput("startYear", "Start Year:", 2010),
        numericInput("startPeriod", "Start Period:", 1)
      ),
      
      conditionalPanel(
        condition = "input.dataSource == 'synthetic'",
        numericInput("syntheticLength", "Series Length:", 120),
        numericInput("syntheticTrendStart", "Trend Start Value:", 100),
        numericInput("syntheticTrendEnd", "Trend End Value:", 200),
        numericInput("syntheticSeasonality", "Seasonality Amplitude:", 15),
        numericInput("syntheticNoise", "Noise Standard Deviation:", 10),
        numericInput("syntheticFrequency", "Frequency:", 12),
        numericInput("syntheticStartYear", "Start Year:", 2010)
      )
    ),
    
    # Analysis inputs
    conditionalPanel(
      condition = "input.sidebar == 'analysis'",
      checkboxInput("showDiff", "Show Differenced Series", TRUE),
      checkboxInput("showDecomposition", "Show Decomposition", TRUE),
      checkboxInput("showACFPACF", "Show ACF/PACF", TRUE),
      numericInput("diffOrder", "Differencing Order:", 1, min = 0, max = 2)
    ),
    
    # Forecasting inputs
    conditionalPanel(
      condition = "input.sidebar == 'forecasting'",
      numericInput("forecastPeriods", "Forecast Periods:", 24, min = 1),
      selectInput("forecastMethod", "Forecast Method:",
                 choices = c("Auto ARIMA" = "auto.arima",
                            "ETS" = "ets",
                            "Seasonal Naive" = "snaive",
                            "STL + ETS" = "stlf")),
      checkboxInput("showPredictionIntervals", "Show Prediction Intervals", TRUE)
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data tab
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Time Series Data", status = "primary", solidHeader = TRUE,
                    plotOutput("dataPlot", height = 300)),
                box(title = "Data Summary", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("dataSummary"))
              ),
              fluidRow(
                box(title = "Data Preview", width = 12,
                    tableOutput("dataTable"))
              )
      ),
      
      # Analysis tab
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Original vs. Differenced Series", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    plotOutput("diffPlot", height = 250),
                    conditionalPanel(
                      condition = "input.showDiff == true",
                      verbatimTextOutput("diffSummary")
                    ))
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.showDecomposition == true",
                  box(title = "Time Series Decomposition", status = "info", 
                      solidHeader = TRUE, width = 12,
                      plotOutput("decompPlot", height = 400))
                )
              ),
              fluidRow(
                conditionalPanel(
                  condition = "input.showACFPACF == true",
                  box(title = "ACF and PACF", status = "warning", 
                      solidHeader = TRUE, width = 12,
                      plotOutput("acfpacfPlot", height = 350))
                )
              )
      ),
      
      # Forecasting tab
      tabItem(tabName = "forecasting",
              fluidRow(
                box(title = "Forecast Plot", status = "primary", 
                    solidHeader = TRUE, width = 12,
                    plotOutput("forecastPlot", height = 350))
              ),
              fluidRow(
                box(title = "Model Summary", status = "info", 
                    solidHeader = TRUE, width = 6,
                    verbatimTextOutput("modelSummary")),
                box(title = "Forecast Values", status = "warning", 
                    solidHeader = TRUE, width = 6,
                    tableOutput("forecastTable"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: Get time series data based on user input
  ts_data <- reactive({
    if (input$dataSource == "builtin") {
      # Load built-in dataset
      dataset_name <- input$builtinData
      if (dataset_name == "AirPassengers") {
        return(AirPassengers)
      } else if (dataset_name == "sunspot.year") {
        return(sunspot.year)
      } else if (dataset_name == "UKgas") {
        return(UKgas)
      } else if (dataset_name == "WWWusage") {
        return(WWWusage)
      }
    } else if (input$dataSource == "upload" && !is.null(input$uploadFile)) {
      # Process uploaded file
      df <- read.csv(input$uploadFile$datapath)
      
      # Convert to time series
      if (input$timeColumn > 0) {
        time_col <- df[, input$timeColumn]
        value_col <- df[, input$valueColumn]
        # If time column exists, use it for conversion logic (simplified here)
        return(ts(value_col, frequency = input$frequency))
      } else {
        value_col <- df[, input$valueColumn]
        return(ts(value_col, start = c(input$startYear, input$startPeriod), 
                  frequency = input$frequency))
      }
    } else if (input$dataSource == "synthetic") {
      # Generate synthetic data
      set.seed(123)
      trend <- seq(from = input$syntheticTrendStart, 
                  to = input$syntheticTrendEnd, 
                  length.out = input$syntheticLength)
      
      seasonal <- input$syntheticSeasonality * 
        sin(2 * pi * seq(1:input$syntheticLength)/input$syntheticFrequency)
      
      irregular <- rnorm(input$syntheticLength, mean = 0, sd = input$syntheticNoise)
      
      synthetic_ts <- trend + seasonal + irregular
      return(ts(synthetic_ts, 
                start = c(input$syntheticStartYear, 1), 
                frequency = input$syntheticFrequency))
    }
    
    # Default return if no valid selection
    return(AirPassengers)
  })
  
  # Data tab outputs
  output$dataPlot <- renderPlot({
    plot(ts_data(), main = "Time Series Plot", xlab = "Time", ylab = "Value",
         col = "steelblue", lwd = 2)
  })
  
  output$dataSummary <- renderPrint({
    my_ts <- ts_data()
    cat("Time Series Summary:\n")
    cat("Start time: ", start(my_ts), "\n")
    cat("End time: ", end(my_ts), "\n")
    cat("Frequency: ", frequency(my_ts), "\n")
    cat("Number of observations: ", length(my_ts), "\n\n")
    cat("Statistical Summary:\n")
    summary(my_ts)
  })
  
  output$dataTable <- renderTable({
    my_ts <- ts_data()
    df <- data.frame(
      Time = time(my_ts),
      Value = as.numeric(my_ts)
    )
    head(df, 10)
  })
  
  # Analysis tab outputs
  output$diffPlot <- renderPlot({
    my_ts <- ts_data()
    
    # Apply differencing as specified
    if (input$diffOrder > 0) {
      diff_ts <- diff(my_ts, differences = input$diffOrder)
      
      # Plot both original and differenced series
      par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
      plot(my_ts, main = "Original Series", xlab = "", 
           ylab = "Value", col = "steelblue", lwd = 2)
      plot(diff_ts, main = paste("Differenced Series (order =", input$diffOrder, ")"), 
           xlab = "Time", ylab = "Difference", col = "tomato", lwd = 2)
      par(mfrow = c(1, 1))
    } else {
      # Just plot the original series
      plot(my_ts, main = "Original Series", 
           xlab = "Time", ylab = "Value", col = "steelblue", lwd = 2)
    }
  })
  
  output$diffSummary <- renderPrint({
    my_ts <- ts_data()
    
    if (input$diffOrder > 0) {
      diff_ts <- diff(my_ts, differences = input$diffOrder)
      
      # Test for stationarity
      adf_result <- tryCatch(
        adf.test(diff_ts),
        error = function(e) return(NULL)
      )
      
      cat("Differenced Series Summary:\n")
      cat("Differencing order: ", input$diffOrder, "\n")
      cat("Number of observations after differencing: ", length(diff_ts), "\n\n")
      
      if (!is.null(adf_result)) {
        cat("Augmented Dickey-Fuller Test for Stationarity:\n")
        cat("ADF Test p-value: ", adf_result$p.value, "\n")
        if (adf_result$p.value < 0.05) {
          cat("Interpretation: Series appears to be stationary (p < 0.05)\n")
        } else {
          cat("Interpretation: Series may not be stationary (p >= 0.05)\n")
        }
      }
    }
  })
  
  output$decompPlot <- renderPlot({
    my_ts <- ts_data()
    
    # Check if decomposition is possible
    if (frequency(my_ts) > 1) {
      tryCatch({
        decomp <- decompose(my_ts)
        plot(decomp, col = "steelblue")
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(1, 1, "Decomposition not possible for this time series",
             cex = 1.5, col = "red")
      })
    } else {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Decomposition requires seasonal data (frequency > 1)",
           cex = 1.5, col = "red")
    }
  })
  
  output$acfpacfPlot <- renderPlot({
    my_ts <- ts_data()
    
    # Apply differencing if specified
    if (input$diffOrder > 0) {
      diff_ts <- diff(my_ts, differences = input$diffOrder)
    } else {
      diff_ts <- my_ts
    }
    
    # Generate ACF and PACF plots
    par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
    acf(diff_ts, main = paste("ACF of", ifelse(input$diffOrder > 0, "Differenced", "Original"), "Series"))
    pacf(diff_ts, main = paste("PACF of", ifelse(input$diffOrder > 0, "Differenced", "Original"), "Series"))
    par(mfrow = c(1, 1))
  })
  
  # Forecasting tab outputs
  # Reactive: Generate forecast based on user inputs
  forecast_output <- reactive({
    my_ts <- ts_data()
    
    # Fit model based on selected method
    if (input$forecastMethod == "auto.arima") {
      model <- auto.arima(my_ts)
    } else if (input$forecastMethod == "ets") {
      model <- ets(my_ts)
    } else if (input$forecastMethod == "snaive") {
      model <- snaive(my_ts)
    } else if (input$forecastMethod == "stlf") {
      # Check if frequency is sufficient for STL
      if (frequency(my_ts) > 1) {
        model <- stlf(my_ts)
      } else {
        # Fallback to ETS if not seasonal
        model <- ets(my_ts)
      }
    }
    
    # Generate forecast
    forecast(model, h = input$forecastPeriods)
  })
  
  output$forecastPlot <- renderPlot({
    fc <- forecast_output()
    
    if (!input$showPredictionIntervals) {
      # Plot without prediction intervals
      plot(fc, main = paste("Forecast using", input$forecastMethod),
           xlab = "Time", ylab = "Value",
           fcol = "red", PI = FALSE)
    } else {
      # Plot with prediction intervals
      plot(fc, main = paste("Forecast using", input$forecastMethod),
           xlab = "Time", ylab = "Value",
           fcol = "red")
    }
  })
  
  output$modelSummary <- renderPrint({
    fc <- forecast_output()
    summary(fc$model)
  })
  
  output$forecastTable <- renderTable({
    fc <- forecast_output()
    
    # Convert forecast to data frame
    forecast_df <- data.frame(
      Time = time(fc$mean),
      Forecast = as.numeric(fc$mean),
      Lower_80 = as.numeric(fc$lower[, 1]),
      Upper_80 = as.numeric(fc$upper[, 1]),
      Lower_95 = as.numeric(fc$lower[, 2]),
      Upper_95 = as.numeric(fc$upper[, 2])
    )
    
    forecast_df
  })
  
  # Update sidebar based on tab selection
  observe({
    updateTabItems(session, "sidebar", input$sidebar)
  })
}

# Run the application
shinyApp(ui = ui, server = server)