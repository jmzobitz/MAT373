### MAT 373, Day 2
### Simulate left, right, center - part 2

## Part 1: Setup - define how many chips, players, and dice we will have
chips <- 6
players <- 3
n_dice <- 3

# Define the different moves, and the probabilities associated with each:
outcomes <- data.frame(rolls = as.factor(c("left","right","star","center")),
                       probabilities = c(1/6,1/6,1/6,3/6) )

## Part 2: Let's play a game!

# Define a function that can simulate a turn (all players roll)
turn <- function( dice_probabilities,  # Dice probabilities
                  player_chips, # Number of chips held by each player
                  in_dice = 3   # Number of dice rolled
) {
  
  num_players <- length(player_chips)
  
  # This vector stores the input totals
  new_chips <- player_chips

  # Loop through all the different players
  for (i in 1:num_players) {
    curr_chips <- new_chips[i]
    n_dice_roll <- min(curr_chips, in_dice) # Can't roll more than the number of dice in hand
    roll <- sample(dice_probabilities$rolls,
                   size = n_dice_roll, # Note: if n_dice = 0, then there will be nothing
                   replace = TRUE,
                   prob = dice_probabilities$probabilities) |>
      table()
    
    # Calculate how many chips were lost for this player
    new_chips[i] <- curr_chips - sum(roll[c("left", "right", "center")]) # Curr chips in the hand we 
    
    # Now pass the chips to the left or the right depending on the roll
    if (i == 1) {
      left_index <- num_players  # If index is 1, then it goes to the end of the list
    } else {
      left_index <- i - 1
    }
    if (i == num_players) {   # If we are at the end, then it goes to the beginning
      right_index <- 1
    } else {
      right_index <- i + 1
    }
    
    # Allocate chips to the players
    new_chips[left_index] <- new_chips[left_index] + roll["left"]
    new_chips[right_index] <- new_chips[right_index] + roll["right"]
  }
  
  # Return the output
  return(new_chips)
}



# Initialize some chips and players
game_chips <- array(chips,dim=players)

# Copy and paste the following block of code multiple times to see the game in progress
{ 
  game_chips <- turn(dice_probabilities = outcomes,
                     player_chips = game_chips,
                     in_dice = n_dice)
  print(game_chips)
}
### 

## Part 3: Keep track of a game in progress

left_right_center_game <- function( dice_probabilities,  # Dice probabilities
                                    in_players = 3,  # Number of players
                                   in_chips = 6, # Number of chips
                                   in_dice = 3, # Number of dice rolled
                                   max_turns = 200 # Max turns before terminating
                                   ) {
  
  chips_start <- array(in_chips,dim=in_players)
  
  current_chips <- vector(mode="list",length=max_turns+1)  # We add one more because of the start
  current_chips[[1]] <- chips_start
  
  # R counts loops starting at 1 (not 0 ;-) )
  curr_index <- 2
  in_play <- (sum(current_chips[[1]] > 0) > 1) & (curr_index <= max_turns)
  
  # Use a while loop for playing the game
  while(in_play) {
    my_turn <- turn(dice_probabilities = dice_probabilities,
                    player_chips = current_chips[[curr_index-1]],
                    in_dice = in_dice)

    
    
    current_chips[[curr_index]] <- my_turn
    
    in_play <- (sum(my_turn > 0) > 1) & (curr_index <= max_turns)
    curr_index <- curr_index + 1
  }
  # Remove all the null entries that weren't assigned and organize this into an array
  
  
  game_progress  <- matrix(unlist(current_chips[lengths(current_chips) != 0]), ncol = in_players, byrow = TRUE)
  
  return(game_progress)
  
  
  
}

# This runs a game as a function - yay!
left_right_center_game(dice_probabilities = outcomes)

## Part 4 - Exploration of games

# How long do games last?
n_times <- 1000
replicate(n_times, {
  
  left_right_center_game(dice_probabilities = outcomes) |>
    nrow()
  
}
) |> 
  table() / n_times


# Is there an advantage to roll?
n_times <- 1000
replicate(n_times, {
  
  game_progress <- left_right_center_game(dice_probabilities = outcomes)
  # Winner is the player who has chips at the end - we take the average if there is a tie
  winner <- which(tail(game_progress,1)>0) |> mean()
   
}
) |> 
  table() / n_times




