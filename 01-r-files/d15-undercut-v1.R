### MAT 373, Day 15
### Undercut in R --> basic game


# Undercut is a game that looks at the difference between two players to allocate points
undercut_round <- function() {
  
  # The two players
  p1_play <- sample(1:5,size = 1, replace = TRUE)
  p2_play <- sample(1:5,size = 1, replace = TRUE)
  
  # Determine how much the difference between the two are
  net_change = p1_play - p2_play
  
  # net_change > 0 --> points go to player1
  # net_change < 0 --> points go to player2
  # net_change == 1 --> player 1 is undercut, so all points to player2
  # net_change == -1 --> player 2 is undercut, so all points to player1
  
  # We can use abs(net_change) to do a single line of code with sign(net_change)
  # sign(4) = 1; sign(-4) = -1
  
  if(abs(net_change) == 1) {  # Undercut occurs!
    net_points <- -sign(net_change)*(p1_play+p2_play)  # Notice the negative sign. If sign(net_change) > 0 sign is positive all net_points to player2, so should be negative
  } else {
    net_points <- net_change
  }
  

  return(net_points)
  
}

### YOUR TURN: Try out the game against yourself, keeping score
undercut_round()

# Next, here is an easier way to keep score:
sample_play <- c(0,replicate(n_times, {
  
  # Play your fingers
  undercut_round()
}
)) |> cumsum()

# See the results
sample_play


# Since this is a running vector, we need to determine when either player first pulls greater than 11 (player 1 wins) or less than -11 (player 2 wins).  Absolute value to the rescue!

# Try to see where this occurs - this will print out the indices
which(abs(sample_play) >= 11)

round_length <- min(which(abs(sample_play) >= 11)) # Tells us the first time a player goes past the target

# Just show the scores until a winner
sample_play[1:round_length]

# Now we can set up a function that plays through undercut
play_undercut <- function(max_rounds = 100) {
  
  # Play undercut to the maximum number of rounds
  sample_play <- c(0,replicate(max_rounds, {
    
    # Define a sample roll
    undercut_round()
  }
  )) |> cumsum()
  
  round_length <- min(which(abs(sample_play) >= 11)) # Tells us the first vector (length of the round)
  
  return(sample_play[1:round_length])
  
  
}

# Try running this code several times to see the different games:
play_undercut()


# Now let's simulate several undercut games!
# Simulate the output values (replace play_undercut() with your actual function)
out_values <- lapply(1:1000, function(i) play_undercut())

# To determine who is the winner, we look at the last entry in each game.  This will require two passes through using sapply
winners <- sapply(out_values,tail,n=1) |>  # Get the last entry in each round
  sapply(sign)   # Get the winners  1 = player 1, -1 = player 2

# See the frequency distribution of the winners
table(winners) / length(winners)

# What is the distribution of the different rounds?
round_length <- sapply(out_values,length)

# Report out stats
hist(round_length)
summary(round_length)


#### Plotting the different rounds will take a little work.  I used chatGPT 
"plot a list of vectors with different lengths in baseR"

# Find the maximum length among the vectors
max_length <- max(sapply(out_values, length))

# Pad each vector with NA to make them all the same length
padded_list <- lapply(out_values, function(x) {
  length(x) <- max_length
  return(x)
})

# Convert the padded list into a matrix
matrix_data <- do.call(cbind, padded_list)

# Plot the data using matplot()
matplot(matrix_data, type = "l", lty = 1, col = rainbow(length(vector_list)),
        xlab = "Index", ylab = "Values", main = "Undercut simulations")

