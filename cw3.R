# Function to generate simulations of a person's walk home
generate <- function(steps, sims) {
  # Check if steps and sims are numeric, integer, and within the specified range
  if (is.numeric(steps) && is.numeric(sims) && steps <= 200 && sims <= 1000 && steps == round(steps) && sims == round(sims)) {
    # Reading CSV file
    probabilities <- read.csv("/Users/rae/Downloads/probabilities.csv", header = FALSE, row.names = 1)
    
    # Empty list to store simulation results
    results <- list()
    
    # Loop the number of simulations
    for (i in 1:sims) {
      # Initialize a matrix to represent the person's position in the lanes
      sim_matrix <- matrix(0, nrow = steps, ncol = 9, dimnames = list(paste0("Step ", 1:steps), paste0("Lane ", 1:9)))
      # Start the person at lane 5
      sim_matrix[1, 5] <- 1
      
      # Loop through each step of the simulation
      for (step in 2:steps) {
        # Get the current lane of the person
        current_lane <- which(sim_matrix[step - 1, ] == 1)
        
        # Movement probabilities based on the current lane
        movement_probs <- probabilities[current_lane, ]
        movement_probs <- movement_probs[!is.na(movement_probs) & movement_probs > 0]
        
        # If there are valid movement probabilities
        if (length(movement_probs) > 0) {
          # Sample a movement based on probabilities
          movement <- sample.int(length(movement_probs), size = 1, replace = TRUE, prob = movement_probs)
          
          # Calculate the new lane based on the sampled movement
          new_lane <- switch(movement,
                             "1" = max(current_lane - 3, 1),
                             "2" = max(current_lane - 2, 1),
                             "3" = max(current_lane - 1, 1),
                             "4" = max(current_lane - 1, 1),
                             "5" = current_lane,
                             "6" = min(current_lane + 1, 9),
                             "7" = min(current_lane + 2, 9),
                             "8" = min(current_lane + 3, 9))
          
          # Update the person's position in the matrix
          sim_matrix[step, new_lane] <- 1
        }
        sim_matrix[step, new_lane] <- 1
      }
      # Store the simulation result in the list
      results[[paste0("Simulation.", i)]] <- sim_matrix
    }
    
    # Return the list of simulation results
    return(results)
    
  } else {
    # If steps or sims are not within the specified range, throw an error
    stop("Error: steps must be an integer between 1 and 200, and sims must be an integer between 1 and 1000.")
    return(NULL)
  }
}

# Function to analyze the simulation results
sim.analysis <- function(results) {
  # Total observed times in each lane for each step
  total_observed <- array(0, dim = c(nrow(results[[1]]), ncol(results[[1]])))
  
  # Loop each simulation and aggregate the results
  for (i in seq_along(results)) {
    total_observed <- total_observed + results[[i]]
  }
  
  # Calculate the mean and standard deviation of observations in each lane
  lane_means <- rowMeans(total_observed)
  lane_stdevs <- apply(total_observed, 2, sd)
  
  # Add mean and standard deviation rows to total_observed
  total_observed <- rbind(total_observed, lane_means)
  total_observed <- rbind(total_observed, lane_stdevs)
  
  # Calculate mean and standard deviation for every 10th step and the final step
  step_stats <- matrix(NA, nrow = 2, ncol = 9)
  step_stats_labels <- c("Mean", "Standard Deviation")
  
  # Loop each step and calculate the statistics
  for (i in seq(1, nrow(total_observed), by = 10)) {
    if (i <= nrow(total_observed)) {
      step <- total_observed[i, ]
      step_stats <- rbind(step_stats, step)
    }
  }
  
  # Calculate mean and standard deviation for the final step
  final_step <- total_observed[nrow(total_observed), ]
  step_stats <- rbind(step_stats, final_step)
  
  # Save total_observed to outcomes.csv
  write.csv(total_observed, file = "outcomes.csv")
  
  # Save step_stats to summary.csv
  step_stats_df <- as.data.frame(step_stats)
  colnames(step_stats_df) <- paste0("Lane ", 1:9)
  rownames(step_stats_df) <- c(step_stats_labels, seq(10, min(nrow(total_observed) - 2, nrow(total_observed)), by = 10), "Final Step")
  write.csv(step_stats_df, file = "summary.csv")
}
# Generating steps = 100 and sims = 200
results <- generate(100,200)
results

