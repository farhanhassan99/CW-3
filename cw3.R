generate <- function(steps, sims) {
  # Check if steps and sims are integers
  if (is.integer(steps) || is.integer(sims)) {
    cat("Both steps and sims must be integers.\n")
    return(NULL)
  }
  
  # Check if steps is within range
  if (steps < 1 || steps > 200) {
    cat("Number of steps must be between 1 and 200.\n")
    return(NULL)
  }
  
  # Check if sims is within range
  if (sims < 1 || sims > 1000) {
    cat("Number of simulations must be between 1 and 1000.\n")
    return(NULL)
  }
  
  # Read probabilities from CSV file (used file.choose() to select the file)
  probabilities <- read.csv("/Users/rae/Downloads/probabilities.csv", header = TRUE, row.names = 1)
  
  
  # Define function to calculate next lane based on current lane
  next_lane <- function(current_lane) {
    movements <- c("L3", "L2", "L1", "SF", "R1", "R2", "R3") # Defining the vector
    probabilities_for_lane <- probabilities[current_lane, ]
    movement_probabilities <- as.numeric(probabilities_for_lane) # Convert to numeric vectr 
    movement <- sample(movements, size = 1, prob = movement_probabilities)
    
    if (movement == "L3") {
      return(current_lane - 3)
    } else if (movement == "L2") {
      return(current_lane - 2)
    } else if (movement == "L1") {
      return(current_lane - 1)
    } else if (movement == "R1") {
      return(current_lane + 1)
    } else if (movement == "R2") {
      return(current_lane + 2)
    } else if (movement == "R3") {
      return(current_lane + 3)
    } else {
      return(current_lane) # Movement is not recognized, return to current lane
    }
  }
  
  # Store simulations
  results <- list()
  
  # Performing simulations (loops)
  for (i in 1:sims) {
    current_lane <- 5
    # matrix to store each single simulation
    simulation_matrix <- matrix(0, nrow = steps, ncol = 9,
                                dimnames = list(paste0("Step ", 1:steps), paste0("Lane", 1:9)))
    
    # each simulation loop:
    for (step in 1:steps) {
      simulation_matrix[step, current_lane] <- 1  # set the current lane to 1 (person's position)
      current_lane <- next_lane(current_lane)    # calculate the next lane based on the current lane
      if (current_lane < 1) current_lane <- 1    
      if (current_lane > 9) current_lane <- 9
    }
    
    results[[paste0("simulation.", i)]] <- simulation_matrix
  }
  
  return(results)
}
probabilities #check the probabilites file
#results <‐ generate(100, 200)  
results <- generate(steps = 100, sims = 200)
