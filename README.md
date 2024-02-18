# Question 2 Part 1
ann.loan <- function(type, n, x, i, p) {
  # Check if type is 'ann' or 'loan'
  if (type %in% c('ann', 'loan')) {
    # Continue with the function if condition is met
  } else {
    stop("Invalid type. Use 'ann' for annuity calculations or 'loan' for loan repayment calculations.")
  }
  
  # Check conditions for n
  if (is.numeric(n) && n >= 1 && n <= 25 && n == round(n)) {
    # Continue with the function if condition is met
  } else {
    stop("Invalid. 'n' must be an integer between 1 and 25.")
  }
  
  # Check conditions for x
  if (is.numeric(x) && x > 0) {
    # Continue with the function if condition is met
  } else {
    stop("Invalid. 'x' must be a positive number.")
  }
  
  # Check conditions for i
  if (is.numeric(i) && i >= 0 && i <= 1) {
    # Continue with the function if condition is met
  } else {
    stop("Invalid. 'i' must be a number between 0 and 1.")
  }
  
  # Check conditions for p
  if (p %in% c(1, 2, 3, 4, 6, 12)) {
    # Continue with the function if condition is met
  } else {
    stop("Invalid. 'p' can only take the values 1, 2, 3, 4, 6, and 12.")
  }
  
  # Annuity or loan repayment calculation based on type
  if (type == 'ann') {
    present_value <- x * ((1 - (1 + i)^(-n)) / i)
    return(present_value)
  } else {
    total_repayment <- x * (i / (1 - (1 + i)^(-n)))
    return(total_repayment)
  }
}

# Test the function with some values
# For annuity calculation
annuity_pv <- ann.loan(type = 'ann', n = 10, x = 100, i = 0.05, p = 12)
print(paste("Present value of annuity:", round(annuity_pv, 2)))

# For loan repayment calculation
loan_repayment <- ann.loan(type = 'loan', n = 4, x = 120, i = 0.06, p = 3)
print(paste("Total annual loan repayment:", round(loan_repayment, 2)))

#Question 2 part 2

# Function to value policies
value_policies <- function(data_path) {
  # Read data from CSV file
  data <- read.csv("R coursework/policyholders.csv")
  

  colnames(data) <- c("First name", "Surname", "Gender", "Type", "n", "x", "i", "p")
  
  # Convert type to character and validate
  data$Type <- factor(data$Type, levels = c(1, 2), labels = c("ann", "loan"))
  invalid_types <- data[!(data$Type %in% c("ann", "loan")), ]
  if (nrow(invalid_types) > 0) {
    stop(paste0("Invalid policy types found in rows: ", paste(rownames(invalid_types), collapse = ", ")))
  }
  
  # Calculate present values/repayments using ann.loan function
  data$Result <- mapply(ann.loan, type = data$Type, n = data$n, x = data$x, i = data$i, p = data$p, SIMPLIFY = FALSE)
  
  # Separate annuities and loans with errors
  annuities <- data[data$Type == "ann", ]
  loans <- data[data$Type == "loan", ]
  errors <- data[is.na(data$Result), ]
  
  # Sort by surname
  annuities <- annuities[order(annuities$Surname), ]
  loans <- loans[order(loans$Surname), ]
  errors <- errors[order(errors$Surname), ]
  
  # Write annuities and loans to CSV files with results
  write.csv(annuities, "annuities_valued.csv", row.names = FALSE)
  write.csv(loans, "loans_valued.csv", row.names = FALSE)
  
  # Write errors to CSV file
  if (nrow(errors) > 0) {
    write.csv(errors, "errors.csv", row.names = FALSE)
  }
  
  # Calculate summary statistics by gender for both annuities and loans
  summary_stats <- by(data$Result, data$Gender, FUN = function(x) c(mean = mean(x), sd = sd(x)))
  summary_stats <- do.call(rbind, summary_stats)
  write.csv(summary_stats, "summary.csv", row.names = TRUE)
}

# Example 
data_path <- ("R coursework/policyholders.csv")
value_policies(data_path)
value_policies()
