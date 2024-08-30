### MAT 373, Day 2
### Simulate left, right, center

# What is the chance of losing different chips in a turn?
n_dice <- 3

# Define the different moves, and the probabilities associated with each:
outcomes <- data.frame(rolls = as.factor(c("left","right","center","star")),
                       probabilities = c(1/6,1/6,1/6,3/6) )



# Do a sample roll:
sample(outcomes$rolls,
       size = n_dice, 
       replace = TRUE,
       prob = outcomes$probabilities) |>
  table()


# Time to replicate the results
n_times <- 10000

replicate(n_times, {
  
  # Define a sample roll
  roll <- sample(outcomes$rolls,
                 size = n_dice, 
                 replace = TRUE,
                 prob = outcomes$probabilities) |>
    table()
  
  # Add up the number of chips lost - we only count the left, right and center
  sum(roll[c("left","right","center")]) 
  }
  ) |> 
  table() / n_times



