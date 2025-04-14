# Load required libraries
library(ggplot2)
library(dplyr)

# Function to simulate a 2D random walk
simulate_random_walk <- function(steps = 1000, seed = NULL) {
  # Set seed for reproducibility if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Initialize position vectors
  x <- numeric(steps + 1)
  y <- numeric(steps + 1)
  
  # Starting position is (0,0)
  x[1] <- 0
  y[1] <- 0
  
  # Generate random directions
  # 1 = North, 2 = East, 3 = South, 4 = West
  directions <- sample(1:4, steps, replace = TRUE)
  
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

# Function to plot a random walk
plot_random_walk <- function(walk_data) {
  # Calculate displacement at each step
  walk_data <- walk_data %>%
    mutate(displacement = sqrt(x^2 + y^2))
  
  # Get final position for labeling
  final_pos <- walk_data[nrow(walk_data), ]
  
  # Create the plot
  p <- ggplot(walk_data, aes(x = x, y = y)) +
    geom_path(aes(color = step), linewidth = 0.5) +
    geom_point(data = walk_data[1, ], aes(x = x, y = y), color = "green", size = 3) +
    geom_point(data = final_pos, aes(x = x, y = y), color = "red", size = 3) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(
      title = paste("2D Random Walk (", nrow(walk_data) - 1, " steps)", sep = ""),
      subtitle = paste("Final displacement:", round(final_pos$displacement, 2)),
      x = "X Position",
      y = "Y Position",
      color = "Step"
    ) +
    annotate("text", x = walk_data[1, "x"], y = walk_data[1, "y"] + 0.5, 
             label = "Start", color = "green4") +
    annotate("text", x = final_pos$x, y = final_pos$y + 0.5, 
             label = "End", color = "red4") +
    theme_minimal() +
    coord_equal()
  
  return(p)
}

# Simulate a random walk with 500 steps
set.seed(123)  # For reproducibility
walk_data <- simulate_random_walk(steps = 500)

# Plot the random walk
plot_random_walk(walk_data)

# Calculate and print some statistics
final_position <- tail(walk_data, 1)
cat("Final position:", final_position$x, ",", final_position$y, "\n")
cat("Final displacement:", sqrt(final_position$x^2 + final_position$y^2), "\n")

# The expected displacement for a 2D random walk is approximately sqrt(steps)
expected_displacement <- sqrt(500)
cat("Expected displacement (theoretical):", expected_displacement, "\n")

# Simulate multiple walks to verify the expected displacement
num_simulations <- 1000
final_displacements <- numeric(num_simulations)

for (i in 1:num_simulations) {
  walk <- simulate_random_walk(steps = 500)
  final_pos <- tail(walk, 1)
  final_displacements[i] <- sqrt(final_pos$x^2 + final_pos$y^2)
}

cat("Average final displacement from", num_simulations, "simulations:", 
    mean(final_displacements), "\n")
cat("Standard deviation of displacement:", sd(final_displacements), "\n")

# Plot the distribution of final displacements
displacement_df <- data.frame(displacement = final_displacements)
ggplot(displacement_df, aes(x = displacement)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  geom_vline(xintercept = mean(final_displacements), color = "red", linetype = "dashed") +
  geom_vline(xintercept = expected_displacement, color = "green", linetype = "dashed") +
  labs(
    title = "Distribution of Final Displacements",
    subtitle = paste("Red: Mean from simulations (", round(mean(final_displacements), 2), 
                     "), Green: Theoretical (", round(expected_displacement, 2), ")", sep = ""),
    x = "Displacement",
    y = "Count"
  ) +
  theme_minimal()