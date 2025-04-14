# Random Walk Shiny App
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive 2D Random Walk Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("steps", "Number of Steps:", value = 500, min = 10, max = 10000),
      
      # Probabilities for each direction
      sliderInput("prob_north", "Probability of North:", 
                  min = 0, max = 1, value = 0.25, step = 0.01),
      sliderInput("prob_east", "Probability of East:", 
                  min = 0, max = 1, value = 0.25, step = 0.01),
      sliderInput("prob_south", "Probability of South:", 
                  min = 0, max = 1, value = 0.25, step = 0.01),
      sliderInput("prob_west", "Probability of West:", 
                  min = 0, max = 1, value = 0.25, step = 0.01),
      
      # Seed for reproducibility
      numericInput("seed", "Random Seed (optional):", value = NULL),
      
      # Action button to run simulation
      actionButton("simulate", "Run Simulation"),
      
      # Checkbox to show advanced stats
      checkboxInput("show_stats", "Show Advanced Statistics", value = FALSE),
      
      # Information panel
      helpText("Note: The probabilities must sum to 1. They will be normalized automatically if they don't."),
      
      # Download button for the results
      downloadButton("downloadData", "Download Walk Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Path Visualization", 
                 plotOutput("walkPlot", height = "600px"),
                 verbatimTextOutput("summary")),
        tabPanel("Multiple Simulations", 
                 numericInput("num_sims", "Number of Simulations:", value = 100, min = 10, max = 1000),
                 actionButton("runMultiple", "Run Multiple Simulations"),
                 plotOutput("distPlot", height = "400px"),
                 verbatimTextOutput("multiSummary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Normalize probabilities to ensure they sum to 1
  normalize_probs <- function() {
    probs <- c(input$prob_north, input$prob_east, input$prob_south, input$prob_west)
    probs_sum <- sum(probs)
    
    if (probs_sum == 0) {
      return(c(0.25, 0.25, 0.25, 0.25))  # Default to equal probabilities
    }
    
    return(probs / probs_sum)
  }
  
  # Function to simulate a 2D random walk with custom probabilities
  simulate_random_walk <- function(steps, probs, seed = NULL) {
    # Set seed for reproducibility if provided
    if (!is.null(seed) && !is.na(seed)) {
      set.seed(seed)
    }
    
    # Initialize position vectors
    x <- numeric(steps + 1)
    y <- numeric(steps + 1)
    
    # Starting position is (0,0)
    x[1] <- 0
    y[1] <- 0
    
    # Generate random directions based on probabilities
    # 1 = North, 2 = East, 3 = South, 4 = West
    directions <- sample(1:4, steps, replace = TRUE, prob = probs)
    
    # Perform the random walk
    for (i in 1:steps) {
      direction <- directions[i]
      
      # Update position based on direction
      if (direction == 1) {  # North
        y[i + 1] <- y[i] + 1
        x[i + 1] <- x[i]
      } else if (direction == 2) {  # East
        x[i + 1] <- x[i] + 1
        y[i + 1] <- y[i]
      } else if (direction == 3) {  # South
        y[i + 1] <- y[i] - 1
        x[i + 1] <- x[i]
      } else if (direction == 4) {  # West
        x[i + 1] <- x[i] - 1
        y[i + 1] <- y[i]
      }
    }
    
    # Create a data frame with the results
    results <- data.frame(
      step = 0:steps,
      x = x,
      y = y
    )
    
    return(results)
  }
  
  # Reactive value to store the current walk data
  walk_data <- reactiveVal(NULL)
  
  # Run simulation when button is clicked
  observeEvent(input$simulate, {
    probs <- normalize_probs()
    steps <- input$steps
    seed <- input$seed
    
    # Update the walk data
    walk_data(simulate_random_walk(steps, probs, seed))
  })
  
  # Create the walk plot
  output$walkPlot <- renderPlot({
    req(walk_data())
    
    # Get the walk data
    data <- walk_data()
    
    # Calculate displacement at each step
    data$displacement <- sqrt(data$x^2 + data$y^2)
    
    # Get final position for labeling
    final_pos <- data[nrow(data), ]
    
    # Create the plot
    ggplot(data, aes(x = x, y = y)) +
      geom_path(aes(color = step), linewidth = 0.5) +
      geom_point(data = data[1, ], aes(x = x, y = y), color = "green", size = 3) +
      geom_point(data = final_pos, aes(x = x, y = y), color = "red", size = 3) +
      scale_color_gradient(low = "blue", high = "red") +
      labs(
        title = paste("2D Random Walk (", nrow(data) - 1, " steps)", sep = ""),
        subtitle = paste("Final displacement:", round(final_pos$displacement, 2)),
        x = "X Position",
        y = "Y Position",
        color = "Step"
      ) +
      annotate("text", x = data[1, "x"], y = data[1, "y"] + 0.5, 
               label = "Start", color = "green4") +
      annotate("text", x = final_pos$x, y = final_pos$y + 0.5, 
               label = "End", color = "red4") +
      theme_minimal() +
      coord_equal()
  })
  
  # Output summary statistics
  output$summary <- renderText({
    req(walk_data())
    
    data <- walk_data()
    final_position <- tail(data, 1)
    
    # Calculate displacement
    displacement <- sqrt(final_position$x^2 + final_position$y^2)
    
    # Calculate theoretical expected displacement (approx. sqrt(steps) for equal probabilities)
    steps <- nrow(data) - 1
    probs <- normalize_probs()
    
    summary_text <- paste(
      "Final position: (", final_position$x, ", ", final_position$y, ")\n",
      "Final displacement: ", round(displacement, 2), "\n",
      sep = ""
    )
    
    if (input$show_stats) {
      # Only show these details if advanced stats are enabled
      bias_factor <- 1 - 2 * (probs[1] * probs[3] + probs[2] * probs[4])
      expected_displacement <- sqrt(steps * bias_factor)
      
      summary_text <- paste(
        summary_text,
        "Direction probabilities: North=", round(probs[1], 2), 
        ", East=", round(probs[2], 2),
        ", South=", round(probs[3], 2), 
        ", West=", round(probs[4], 2), "\n",
        "Theoretical bias factor: ", round(bias_factor, 3), "\n",
        "Expected displacement (theoretical): ", round(expected_displacement, 2), "\n",
        sep = ""
      )
    }
    
    return(summary_text)
  })
  
  # Multiple simulations results
  multi_sim_results <- reactiveVal(NULL)
  
  # Run multiple simulations
  observeEvent(input$runMultiple, {
    probs <- normalize_probs()
    steps <- input$steps
    num_sims <- input$num_sims
    
    # Create a progress bar
    progress <- shiny::Progress$new()
    progress$set(message = "Running simulations...", value = 0)
    on.exit(progress$close())
    
    # Run the simulations
    displacements <- numeric(num_sims)
    
    for (i in 1:num_sims) {
      # Update progress bar
      progress$set(value = i / num_sims)
      
      # Run a single simulation
      walk <- simulate_random_walk(steps, probs)
      final_pos <- tail(walk, 1)
      displacements[i] <- sqrt(final_pos$x^2 + final_pos$y^2)
    }
    
    # Store the results
    multi_sim_results(displacements)
  })
  
  # Plot the distribution of displacements
  output$distPlot <- renderPlot({
    req(multi_sim_results())
    
    displacements <- multi_sim_results()
    steps <- input$steps
    probs <- normalize_probs()
    
    # Calculate theoretical expected displacement
    bias_factor <- 1 - 2 * (probs[1] * probs[3] + probs[2] * probs[4])
    expected_displacement <- sqrt(steps * bias_factor)
    
    # Create a data frame for plotting
    df <- data.frame(displacement = displacements)
    
    # Plot the distribution
    ggplot(df, aes(x = displacement)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
      geom_density(color = "darkred", linewidth = 1) +
      geom_vline(xintercept = mean(displacements), color = "red", linetype = "dashed") +
      geom_vline(xintercept = expected_displacement, color = "green", linetype = "dashed") +
      labs(
        title = "Distribution of Final Displacements",
        subtitle = paste("Red: Mean from simulations (", round(mean(displacements), 2), 
                         "), Green: Theoretical (", round(expected_displacement, 2), ")", sep = ""),
        x = "Displacement",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  # Summary of multiple simulations
  output$multiSummary <- renderText({
    req(multi_sim_results())
    
    displacements <- multi_sim_results()
    steps <- input$steps
    probs <- normalize_probs()
    
    # Calculate theoretical expected displacement
    bias_factor <- 1 - 2 * (probs[1] * probs[3] + probs[2] * probs[4])
    expected_displacement <- sqrt(steps * bias_factor)
    
    # Create summary text
    paste(
      "Number of simulations: ", length(displacements), "\n",
      "Mean displacement: ", round(mean(displacements), 2), "\n",
      "Median displacement: ", round(median(displacements), 2), "\n",
      "Standard deviation: ", round(sd(displacements), 2), "\n",
      "Theoretical expected displacement: ", round(expected_displacement, 2), "\n",
      "Ratio of observed to expected: ", round(mean(displacements) / expected_displacement, 2), "\n",
      sep = ""
    )
  })
  
  # Download handler for the walk data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("random_walk_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(walk_data())
      write.csv(walk_data(), file, row.names = FALSE)
    }
  )
  
  # Initialize with a default simulation
  observe({
    # Only run this once when the app starts
    isolate({
      if (is.null(walk_data())) {
        probs <- c(0.25, 0.25, 0.25, 0.25)  # Equal probabilities
        steps <- input$steps
        walk_data(simulate_random_walk(steps, probs))
      }
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)