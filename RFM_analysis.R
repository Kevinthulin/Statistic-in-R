# RFM Analysis Shiny App for Online Retail Dataset

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)
library(tidyr)
library(plotly)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "RFM Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Import", tabName = "data", icon = icon("file-import")),
      menuItem("RFM Analysis", tabName = "rfm", icon = icon("chart-pie")),
      menuItem("Customer Segments", tabName = "segments", icon = icon("users")),
      menuItem("Recommendations", tabName = "recommendations", icon = icon("lightbulb"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Data Import Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Import Data", status = "primary", solidHeader = TRUE,
                  fileInput("file", "Upload Online Retail Excel File",
                            accept = c(".xlsx", ".xls")),
                  actionButton("process", "Process Data", icon = icon("cogs")),
                  hr(),
                  verbatimTextOutput("dataSummary")
                ),
                box(
                  title = "Data Preview", status = "info", solidHeader = TRUE,
                  DTOutput("dataTable")
                )
              )
      ),
      
      # RFM Analysis Tab
      tabItem(tabName = "rfm",
              fluidRow(
                box(
                  title = "RFM Parameters", status = "primary", solidHeader = TRUE, width = 3,
                  dateInput("analysisDate", "Analysis Date:", value = Sys.Date()),
                  # Fixed slider inputs with explicit numeric values
                  sliderInput("recencyBreaks", "Recency Quantiles:", 
                              min = 0.05, max = 0.95, value = c(0.2, 0.4, 0.6, 0.8), step = 0.05),
                  sliderInput("frequencyBreaks", "Frequency Quantiles:", 
                              min = 0.05, max = 0.95, value = c(0.2, 0.4, 0.6, 0.8), step = 0.05),
                  sliderInput("monetaryBreaks", "Monetary Quantiles:", 
                              min = 0.05, max = 0.95, value = c(0.2, 0.4, 0.6, 0.8), step = 0.05),
                  actionButton("calculate", "Calculate RFM", icon = icon("calculator"))
                ),
                tabBox(
                  title = "RFM Distributions", width = 9,
                  tabPanel("Recency", plotlyOutput("recencyPlot")),
                  tabPanel("Frequency", plotlyOutput("frequencyPlot")),
                  tabPanel("Monetary", plotlyOutput("monetaryPlot")),
                  tabPanel("RFM Score", plotlyOutput("rfmScorePlot"))
                )
              ),
              fluidRow(
                box(
                  title = "RFM Summary Statistics", status = "info", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("rfmSummary")
                )
              ),
              fluidRow(
                box(
                  title = "Top Customers by RFM Score", status = "success", solidHeader = TRUE, width = 12,
                  DTOutput("topCustomers"),
                  downloadButton("downloadRFM", "Download RFM Data")
                )
              )
      ),
      
      # Customer Segments Tab
      tabItem(tabName = "segments",
              fluidRow(
                box(
                  title = "Segment Parameters", status = "primary", solidHeader = TRUE, width = 3,
                  numericInput("championThreshold", "Champion Threshold (RFM Score):", 13, min = 3, max = 15),
                  checkboxInput("customSegments", "Use Custom Segment Rules", FALSE),
                  actionButton("updateSegments", "Update Segments", icon = icon("sync"))
                ),
                box(
                  title = "Customer Segments Distribution", status = "info", solidHeader = TRUE, width = 9,
                  plotlyOutput("segmentPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Segment Metrics", status = "warning", solidHeader = TRUE, width = 12,
                  plotlyOutput("segmentMetrics")
                )
              ),
              fluidRow(
                box(
                  title = "Segment Details", status = "success", solidHeader = TRUE, width = 12,
                  DTOutput("segmentTable"),
                  downloadButton("downloadSegments", "Download Segment Data")
                )
              )
      ),
      
      # Recommendations Tab
      tabItem(tabName = "recommendations",
              fluidRow(
                box(
                  title = "Marketing Recommendations", status = "primary", solidHeader = TRUE, width = 12,
                  uiOutput("recommendations")
                )
              ),
              fluidRow(
                valueBoxOutput("championsBox", width = 3),
                valueBoxOutput("loyalBox", width = 3),
                valueBoxOutput("atriskBox", width = 3),
                valueBoxOutput("hibernatingBox", width = 3)
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = NULL,
    cleanData = NULL,
    rfmData = NULL,
    segmentData = NULL
  )
  
  # Process the uploaded file
  observeEvent(input$process, {
    req(input$file)
    
    # Read the data with error handling
    tryCatch({
      values$data <- read_excel(input$file$datapath)
      
      # Clean the data
      values$cleanData <- values$data %>%
        filter(!is.na(CustomerID)) %>%
        filter(Quantity > 0) %>%
        filter(UnitPrice > 0)
      
      # Update analysis date
      last_date <- max(values$cleanData$InvoiceDate)
      updateDateInput(session, "analysisDate", value = last_date + days(1))
    }, error = function(e) {
      showNotification(paste("Error processing file:", e$message), type = "error")
    })
  })
  
  # Display data summary
  output$dataSummary <- renderPrint({
    req(values$cleanData)
    
    cat("Data Summary:\n")
    cat("Total Records:", nrow(values$data), "\n")
    cat("Clean Records:", nrow(values$cleanData), "\n")
    cat("Date Range:", as.character(min(values$cleanData$InvoiceDate)), "to", 
        as.character(max(values$cleanData$InvoiceDate)), "\n")
    cat("Number of Unique Customers:", length(unique(values$cleanData$CustomerID)), "\n")
    cat("Number of Unique Products:", length(unique(values$cleanData$StockCode)), "\n")
    cat("Number of Countries:", length(unique(values$cleanData$Country)), "\n")
  })
  
  # Display data preview
  output$dataTable <- renderDT({
    req(values$cleanData)
    datatable(head(values$cleanData, 1000), 
              options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Calculate RFM
  observeEvent(input$calculate, {
    req(values$cleanData)
    
    # Get analysis date
    analysis_date <- input$analysisDate
    
    # Use tryCatch for error handling
    tryCatch({
      # Calculate RFM metrics
      values$rfmData <- values$cleanData %>%
        mutate(TotalPrice = Quantity * UnitPrice) %>%
        group_by(CustomerID) %>%
        summarize(
          Recency = as.numeric(difftime(analysis_date, max(InvoiceDate), units = "days")),
          Frequency = n_distinct(InvoiceNo),
          Monetary = sum(TotalPrice),
          First_Purchase = min(InvoiceDate),
          Last_Purchase = max(InvoiceDate)
        )
      
      # Define quantiles - with error handling
      quantile_recency <- quantile(values$rfmData$Recency, 
                                  probs = input$recencyBreaks)
      quantile_frequency <- quantile(values$rfmData$Frequency, 
                                    probs = input$frequencyBreaks)
      quantile_monetary <- quantile(values$rfmData$Monetary, 
                                   probs = input$monetaryBreaks)
      
      # Score RFM
      values$rfmData <- values$rfmData %>%
        mutate(
          R_Score = case_when(
            Recency <= quantile_recency[1] ~ 5,
            Recency <= quantile_recency[2] ~ 4,
            Recency <= quantile_recency[3] ~ 3,
            Recency <= quantile_recency[4] ~ 2,
            TRUE ~ 1
          ),
          F_Score = case_when(
            Frequency >= quantile_frequency[4] ~ 5,
            Frequency >= quantile_frequency[3] ~ 4,
            Frequency >= quantile_frequency[2] ~ 3,
            Frequency >= quantile_frequency[1] ~ 2,
            TRUE ~ 1
          ),
          M_Score = case_when(
            Monetary >= quantile_monetary[4] ~ 5,
            Monetary >= quantile_monetary[3] ~ 4,
            Monetary >= quantile_monetary[2] ~ 3,
            Monetary >= quantile_monetary[1] ~ 2,
            TRUE ~ 1
          )
        ) %>%
        mutate(
          RFM_Score = R_Score + F_Score + M_Score,
          RFM_Category = paste0(R_Score, F_Score, M_Score)
        )
      
      # Update segments
      updateSegments()
      
    }, error = function(e) {
      showNotification(paste("Error calculating RFM:", e$message), type = "error")
    })
  })
  
  # Update customer segments
  updateSegments <- function() {
    req(values$rfmData)
    
    tryCatch({
      champion_threshold <- input$championThreshold
      
      if (input$customSegments) {
        # Custom segmentation rules
        values$rfmData <- values$rfmData %>%
          mutate(
            Segment = case_when(
              RFM_Score >= champion_threshold ~ "Champions",
              R_Score >= 4 & F_Score >= 3 & M_Score >= 3 ~ "Loyal Customers",
              R_Score >= 3 & F_Score >= 1 & M_Score >= 2 ~ "Potential Loyalists",
              R_Score >= 3 & F_Score >= 3 & M_Score >= 3 ~ "Recent Customers",
              R_Score <= 2 & F_Score >= 3 & M_Score >= 3 ~ "Needing Attention",
              R_Score <= 2 & F_Score >= 2 & M_Score >= 2 ~ "At Risk",
              R_Score <= 2 & F_Score <= 2 & M_Score <= 2 ~ "Hibernating",
              TRUE ~ "Others"
            )
          )
      } else {
        # Simplified segments based on total RFM score
        values$rfmData <- values$rfmData %>%
          mutate(
            Segment = case_when(
              RFM_Score >= champion_threshold ~ "Champions",
              RFM_Score >= 10 & RFM_Score < champion_threshold ~ "Loyal Customers",
              RFM_Score >= 8 & RFM_Score < 10 ~ "Potential Loyalists",
              RFM_Score >= 6 & RFM_Score < 8 ~ "Needing Attention",
              RFM_Score >= 4 & RFM_Score < 6 ~ "At Risk",
              RFM_Score < 4 ~ "Hibernating",
              TRUE ~ "Others"
            )
          )
      }
      
      # Calculate segment summary
      values$segmentData <- values$rfmData %>%
        group_by(Segment) %>%
        summarize(
          Count = n(),
          Count_Percent = round(n() / nrow(values$rfmData) * 100, 2),
          Avg_Recency = round(mean(Recency), 2),
          Avg_Frequency = round(mean(Frequency), 2),
          Avg_Monetary = round(mean(Monetary), 2),
          Avg_RFM_Score = round(mean(RFM_Score), 2),
          Total_Revenue = sum(Monetary)
        ) %>%
        arrange(desc(Count))
    }, error = function(e) {
      showNotification(paste("Error updating segments:", e$message), type = "error")
    })
  }
  
  # Observe segment update button
  observeEvent(input$updateSegments, {
    updateSegments()
  })
  
  # RFM Summary output
  output$rfmSummary <- renderPrint({
    req(values$rfmData)
    
    cat("RFM Analysis Summary:\n")
    cat("Analysis Date:", as.character(input$analysisDate), "\n")
    cat("Total Customers Analyzed:", nrow(values$rfmData), "\n\n")
    
    cat("RFM Metrics Range:\n")
    cat("Recency (days):", round(min(values$rfmData$Recency), 2), "to", 
        round(max(values$rfmData$Recency), 2), "\n")
    cat("Frequency (transactions):", min(values$rfmData$Frequency), "to", 
        max(values$rfmData$Frequency), "\n")
    cat("Monetary (total spent):", round(min(values$rfmData$Monetary), 2), "to", 
        round(max(values$rfmData$Monetary), 2), "\n\n")
    
    cat("RFM Score Distribution:\n")
    print(table(values$rfmData$RFM_Score))
  })
  
  # Top customers table
  output$topCustomers <- renderDT({
    req(values$rfmData)
    
    top_customers <- values$rfmData %>%
      arrange(desc(RFM_Score), desc(Monetary)) %>%
      select(CustomerID, RFM_Score, R_Score, F_Score, M_Score, Recency, 
             Frequency, Monetary, Segment) %>%
      head(20)
    
    datatable(top_customers, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Recency distribution plot
  output$recencyPlot <- renderPlotly({
    req(values$rfmData)
    
    p <- ggplot(values$rfmData, aes(x = Recency)) +
      geom_histogram(bins = 30, fill = "cornflowerblue", alpha = 0.7) +
      labs(title = "Recency Distribution (Days Since Last Purchase)",
           x = "Days", y = "Number of Customers") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Frequency distribution plot
  output$frequencyPlot <- renderPlotly({
    req(values$rfmData)
    
    p <- ggplot(values$rfmData, aes(x = Frequency)) +
      geom_histogram(bins = 30, fill = "coral", alpha = 0.7) +
      labs(title = "Frequency Distribution (Number of Transactions)",
           x = "Transactions", y = "Number of Customers") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Monetary distribution plot
  output$monetaryPlot <- renderPlotly({
    req(values$rfmData)
    
    p <- ggplot(values$rfmData, aes(x = Monetary)) +
      geom_histogram(bins = 30, fill = "forestgreen", alpha = 0.7) +
      labs(title = "Monetary Distribution (Total Spend)",
           x = "Total Spend", y = "Number of Customers") +
      theme_minimal() +
      scale_x_log10()
    
    ggplotly(p)
  })
  
  # RFM Score distribution plot
  output$rfmScorePlot <- renderPlotly({
    req(values$rfmData)
    
    p <- ggplot(values$rfmData, aes(x = factor(RFM_Score))) +
      geom_bar(fill = "darkblue", alpha = 0.7) +
      labs(title = "RFM Score Distribution",
           x = "RFM Score", y = "Number of Customers") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Segment distribution plot
  output$segmentPlot <- renderPlotly({
    req(values$segmentData)
    
    p <- ggplot(values$segmentData, aes(x = reorder(Segment, -Count), y = Count, fill = Segment)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(Count_Percent, "%")), vjust = -0.5) +
      labs(title = "Customer Segments Distribution",
           x = "", y = "Number of Customers") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Segment metrics plot
  output$segmentMetrics <- renderPlotly({
    req(values$segmentData)
    
    segment_metrics <- values$segmentData %>%
      select(Segment, Avg_Recency, Avg_Frequency, Avg_Monetary) %>%
      gather(key = "Metric", value = "Value", -Segment)
    
    p <- ggplot(segment_metrics, aes(x = reorder(Segment, Value), y = Value, fill = Metric)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Metric, scales = "free_y") +
      coord_flip() +
      labs(title = "Average RFM Metrics by Customer Segment",
           x = "", y = "Average Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Segment details table
  output$segmentTable <- renderDT({
    req(values$segmentData)
    
    datatable(values$segmentData, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Marketing recommendations
  output$recommendations <- renderUI({
    req(values$segmentData)
    
    HTML(
      "<h4>Recommended Marketing Strategies by Segment:</h4>
      
      <strong>Champions:</strong>
      <ul>
        <li>Reward them with loyalty programs</li>
        <li>Make them brand advocates</li>
        <li>Invite to exclusive events and promotions</li>
        <li>Cross-sell higher-end products</li>
      </ul>
      
      <strong>Loyal Customers:</strong>
      <ul>
        <li>Upsell to higher margin products</li>
        <li>Engage with regular communication</li>
        <li>Offer premium membership with benefits</li>
        <li>Gather feedback for product improvements</li>
      </ul>
      
      <strong>Potential Loyalists:</strong>
      <ul>
        <li>Offer membership programs</li>
        <li>Recommend additional products</li>
        <li>Provide special customer onboarding</li>
        <li>Build relationship with personalized communications</li>
      </ul>
      
      <strong>Recent Customers:</strong>
      <ul>
        <li>Provide excellent service to ensure satisfaction</li>
        <li>Follow up on their experience</li>
        <li>Offer a welcoming promotion for next purchase</li>
      </ul>
      
      <strong>Needing Attention:</strong>
      <ul>
        <li>Reactivation offers and limited-time promotions</li>
        <li>Request feedback on satisfaction</li>
        <li>Recommend products based on past purchases</li>
      </ul>
      
      <strong>At Risk:</strong>
      <ul>
        <li>Send service recovery emails</li>
        <li>Offer renewal incentives</li>
        <li>Conduct satisfaction surveys</li>
        <li>Reengagement campaigns with added value</li>
      </ul>
      
      <strong>Hibernating:</strong>
      <ul>
        <li>Create 'We miss you' campaigns</li>
        <li>Offer significant incentives to reconnect</li>
        <li>Present new products and improvements since last purchase</li>
      </ul>"
    )
  })
  
  # Value boxes
  output$championsBox <- renderValueBox({
    req(values$segmentData)
    champion_data <- values$segmentData %>% filter(Segment == "Champions")
    if(nrow(champion_data) > 0) {
      valueBox(
        paste0(champion_data$Count, " (", champion_data$Count_Percent, "%)"),
        "Champions",
        icon = icon("crown"),
        color = "yellow"
      )
    } else {
      valueBox("N/A", "Champions", icon = icon("crown"), color = "yellow")
    }
  })
  
  output$loyalBox <- renderValueBox({
    req(values$segmentData)
    loyal_data <- values$segmentData %>% filter(Segment == "Loyal Customers")
    if(nrow(loyal_data) > 0) {
      valueBox(
        paste0(loyal_data$Count, " (", loyal_data$Count_Percent, "%)"),
        "Loyal Customers",
        icon = icon("heart"),
        color = "green"
      )
    } else {
      valueBox("N/A", "Loyal Customers", icon = icon("heart"), color = "green")
    }
  })
  
  output$atriskBox <- renderValueBox({
    req(values$segmentData)
    atrisk_data <- values$segmentData %>% filter(Segment == "At Risk")
    if(nrow(atrisk_data) > 0) {
      valueBox(
        paste0(atrisk_data$Count, " (", atrisk_data$Count_Percent, "%)"),
        "At Risk Customers",
        icon = icon("exclamation-triangle"),
        color = "orange"
      )
    } else {
      valueBox("N/A", "At Risk Customers", icon = icon("exclamation-triangle"), color = "orange")
    }
  })
  
  output$hibernatingBox <- renderValueBox({
    req(values$segmentData)
    hibernating_data <- values$segmentData %>% filter(Segment == "Hibernating")
    if(nrow(hibernating_data) > 0) {
      valueBox(
        paste0(hibernating_data$Count, " (", hibernating_data$Count_Percent, "%)"),
        "Hibernating Customers",
        icon = icon("snowflake"),
        color = "red"
      )
    } else {
      valueBox("N/A", "Hibernating Customers", icon = icon("snowflake"), color = "red")
    }
  })
  
  # Download handler for RFM data
  output$downloadRFM <- downloadHandler(
    filename = function() {
      paste("rfm_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$rfmData, file, row.names = FALSE)
    }
  )
  
  # Download handler for segment data
  output$downloadSegments <- downloadHandler(
    filename = function() {
      paste("segment_summary_", format(Sys.Date(), "%Y%m%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$segmentData, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)