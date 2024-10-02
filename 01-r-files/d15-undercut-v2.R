### MAT 373, Day 15
### Undercut in R --> advanced game


# Undercut is a game that looks at the difference between two players to allocate points.
# In this code we can allocate in a given round strategy for how player 1 or player 2 will select their fingers, using discrete probability densities.

undercut_round <- function(
    p1_prob = c(0.2, 0.2, 0.2, 0.2, 0.2), # Random strategy for player 1
    p2_prob = c(0.2, 0.2, 0.2, 0.2, 0.2)  # Random strategy for player 2
    ) {
  
  # The two players
  p1_play <- sample(1:5, size = 1, replace = TRUE, prob = p1_prob)
  p2_play <- sample(1:5, size = 1, replace = TRUE, prob = p2_prob)

  # Determine how much the difference between the two are
  net_change <- p1_play - p2_play

  # net_change > 0 --> points go to player1
  # net_change < 0 --> points go to player2
  # net_change == 1 --> player 1 is undercut, so all points to player2
  # net_change == -1 --> player 2 is undercut, so all points to player1

  # We can use abs(net_change) to do a single line of code with sign(net_change)
  # sign(4) = 1; sign(-4) = -1

  if (abs(net_change) == 1) { # Undercut occurs!
    net_points <- -sign(net_change) * (p1_play + p2_play) # Notice the negative sign. If sign(net_change) > 0 sign is positive all net_points to player2, so should be negative
  } else {
    net_points <- net_change
  }


  return(net_points)
}

### Now we make a sample

# Time to play undercut!

p1_strategy <- c(0,0,0,0,1)  # Always play a 5
p2_strategy <- c(1,0,0,0,0)  # Always play a 1

sample_play <- c(0,replicate(n_times, {
  
  # Define a sample roll
  undercut_round(p1_prob = p1_strategy,
                 p2_prob = p2_strategy)
}
)) |> cumsum()

# See the results
sample_play


# You try to see how this happens depending on 
which(abs(sample_play) >= 11)

round_length <- min(which(abs(sample_play) >= 11)) # Tells us the first vector (length of the round)

sample_play[1:round_length]
# Do the results make sense?


# Now let's play undercut where either player 1 and player 2 can change up the strategy
play_undercut <- function(p1_prob = c(0.2,0.2,0.2,0.2,0.2), # The base strategy for player 1
                          p2_prob = c(0.2,0.2,0.2,0.2,0.2), # The base strategy for player 2
                          p1_diff = NA,  # threshold value if NA, player 1 doesn't play a strategy
                          p2_diff = NA,  # threshold value - if NA, player 2 doesn't play a strategy
                          p1_strategy_in = NA,  # The probability when the diff exceeds a value
                          p2_strategy_in = NA,  # The probability when the diff goes below a value
                          max_rounds = 100) {
  
  
  # Here we keep track of the score between them, depending on the strategy
  
  running_score <- array(NA,dim = max_rounds+1)
  running_score[1] <- 0
  
  for(i in 2: (max_rounds+1)) {
    
    p1_strategy <- p1_prob  # The null case
    p1_strategy <- if((running_score[i-1] >= p1_diff) & !is.na(p1_diff) & !anyNA(p1_strategy_in)) {
      p1_strategy <- p1_strategy_in
    }
    
    p2_strategy <- p2_prob  # The null case
    p2_strategy <- if((running_score[i-1] <= p2_diff)  & !is.na(p2_diff) & !anyNA(p2_strategy_in) ) {
      p2_strategy <- p2_strategy_in
    }
    
    running_score[i] <- running_score[i-1] + undercut_round(p1_prob = p1_strategy,
                   p2_prob = p2_strategy)
    
    if(abs(running_score[i])>=11) {break}  # Game is over once it is exceeded
    
  }
  
  return(running_score[!is.na(running_score)])  # Return the running score
}
  
# Player 1 strategy: if the difference is more than 7, then play lower numbers to undercut (safe)
# Player 2 strategy: if the difference is less than -3, then play higher numbers (risky)
play_undercut(p1_diff = 7,
              p1_strategy_in  = c(0.5,0.5,0.0,0,0),
              p2_diff = -3,
              p2_strategy_in = c(0,0,0,0.5,0.5)
              )
# Now let's simulate several undercut games!



# Use lapply to run play_undercut 10 times, passing the same p1_prob value each time
out_values <- lapply(1:1000, function(x) play_undercut(p1_diff = 7,
                                                  p1_strategy_in  = c(0.5,0.5,0.0,0,0),
                                                  p2_diff = -3,
                                                  p2_strategy_in = c(0,0,0,0.5,0.5)
)
)



# To determine who is the winner, we look at the last entry in each game.  This will require two passes through using sapply
winners <- sapply(out_values,tail,n=1) |>  # Get the last entry in each round
  sapply(sign)   # Get the winners  1 = player 1, -1 = player 2

# See the frequency distribution of the winners
table(winners) / length(winners)

# What is the distribution of the different rounds?
round_length <- sapply(out_values,length)
hist(round_length)
summary(round_length)

