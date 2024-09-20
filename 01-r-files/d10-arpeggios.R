# MAT 373, Day 10
# Arpeggions in R

# STEP 0: define the possible outcomes for rolling two dice.
dice <- c(11,12,13,14,15,16,
          21,22,23,24,25,26,
          31,32,33,34,35,36,
          41,42,43,44,45,46,
          51,52,53,54,55,56,
          61,62,63,64,65,66)

### STEP 1: Solo Arpeggios - random dice rolling
# We can sample from this dice, and treat it like a sequence of rolls.  How long is each run?

n_rolls <- 5000 

# All the different dice rolls
rolls <- sample(dice,size = n_rolls,replace=TRUE)

# See the first 20 rolls
rolls[1:20]

# We add up how many times the roll is increasing - the resent
consecutive_runs <- unname(tapply(rolls, cumsum(c(TRUE, diff(rolls) <= 0)), length))

# See what the consecutive sums are - notice how it compares to the rolls - is it consistent?
consecutive_runs[1:20]

# Report summary statistics:
summary(consecutive_runs)


# Prompt to chatGPT: "Make a histogram in base R using discrete values on the x axis"
hist(consecutive_runs, 
     breaks = seq(min(consecutive_runs) - 0.5, max(consecutive_runs) + 0.5, by = 1), 
     right = FALSE, 
     main = "Arpeggio Runs", 
     xlab = "Values", 
     ylab = "Frequency",
     col = rgb(1, 0, 0, 0.5),  # Red with transparency
     border = "black")

# Now implement the mid-game reset. Prompt to chatGPT: "sum two consecutive values of a vector in R using indexing"
reset_runs <- consecutive_runs[-length(consecutive_runs)] + consecutive_runs[-1]

# See the different lengths of games when we reset
reset_runs[1:20]   # Investigate: how does this compare to consecutive_runs?

summary(reset_runs)

# Plot both for a comparison
hist(consecutive_runs, 
     breaks = seq(min(consecutive_runs) - 0.5, max(consecutive_runs) + 0.5, by = 1), 
     right = FALSE, 
     main = "Arpeggio Runs", 
     xlab = "Values", 
     ylab = "Frequency",
     col = rgb(1, 0, 0, 0.5),  # Red with transparency
     border = "black")

hist(reset_runs, 
     breaks = seq(min(reset_runs) - 0.5, max(reset_runs) + 0.5, by = 1), 
     right = FALSE, 
     main = "Arpeggio Runs", 
     xlab = "Values", 
     ylab = "Frequency",
     col = rgb(0, 0, 1, 0.5),  # Blue with transparency
     border = "black",
     add = TRUE)





### STEP 2: Dice rolls 
# Function written and then annotated with chatGPT (prompt: "annotate the following code")
# Define a function named 'dice_roll' with a default argument 'sides' as 1:6
dice_roll <- function(sides = 1:6) {
  
  # Simulate rolling two dice by sampling two values from 'sides'
  # The 'replace = TRUE' allows the same value to be rolled more than once
  roll <- sample(sides, 2, replace = TRUE)
  
  # Create a vector 'options' with two possible numbers:
  # 1. Concatenate the two dice rolls into a single number (e.g., 2 and 5 become 25)
  # 2. Concatenate the reversed order of the dice rolls into a number (e.g., 2 and 5 become 52)
  options <- c(
    as.numeric(paste(roll, collapse = "")),      # Combine roll in normal order (e.g., 25)
    as.numeric(paste(rev(roll), collapse = ""))  # Combine roll in reverse order (e.g., 52)
  )
  
  # Return the vector of both possible combinations
  return(options)
  
}

# See the result:
dice_roll()


### STEP 3: Play a solo game
solo_arpeggios_game <- function(sides = 1:6) {
  
  # Vector of where we are keeping track of our rolls
  arpeggios <- array(data = NA,dim=10) 
  
  arpeggios[1] <- min(dice_roll(sides = sides)) # Pick the smallest of the dice rolls
  curr_dice_roll <- arpeggios[1]  # Just keep track of where we currently are
  reset_vec <- FALSE  # Did we do a mid-game reset yet?
  
  for (i in 2:10) {
    
    dice_option <- dice_roll(sides = sides) # Roll the dice
    next_dice <- curr_dice_roll < dice_option  # Determine if there is a preferred roll
    
    if(sum(next_dice) == 0) {   # We will need to reset here: curr dice 31, options 12 or 21
      if(!reset_vec) {
        curr_dice_roll <- min(dice_option)  # Reset to the smaller of the two options
        arpeggios[i] <- curr_dice_roll  # Keep moving forward
        reset_vec <- TRUE  # Keep track if we have reset
      } else { break }
    }
    
    if(sum(next_dice) == 1) {  # Take the next option: curr dice 31, options 42 or 24 
      curr_dice_roll <- dice_option[next_dice]
      arpeggios[i] <- curr_dice_roll
    }
    
    if(sum(next_dice == 2)) {  # If there is a choice between two, take the smallest: curr dice 31, options 45 or 54
      curr_dice_roll <- sample(dice_option,size=1)
      arpeggios[i] <- curr_dice_roll
    }
  }
  
  return(arpeggios[!is.na(arpeggios)])  # Return the run before busting
  
  
}

# Play a game
solo_arpeggios_game()

# What is the length of a solo arpeggio game?
n_games <- 1000
out_games <- replicate(n_games, {
  solo_arpeggios_game() |> length()
})


hist(out_games_no_reset,
  breaks = seq(min(out_games) - 0.5, max(out_games) + 0.5, by = 1),
  right = FALSE,
  main = "Arpeggio Runs",
  xlab = "Values",
  ylab = "Frequency",
  col = rgb(0, 0, 1, 0.5),  # Blue with transparency
  border = "black")



### STEP 4: Two player arpeggios game.
arpeggios_game <- function() {

  ascender_reset <- FALSE  # Has a mid-game reset occurred?
  descender_reset <- FALSE # Has a mid-game reset occurred?
  
  # Initialize
  ascender_dice <- 10  # Always one less than the smallest
  descender_dice <- 67 # Start out one larger than the biggest roll
  
  # Keep track of scored rolls
  ascender_arpgeggios <- array(NA,dim = 10)  
  descender_arpgeggios <- array(NA, dim = 10)
  
  ascender_score <- 0
  descender_score <- 0
  
  curr_round <- 1
  max_rounds <- 10000  # End the rolls after this length
  
  while(max(ascender_score,descender_score) < 10 & curr_round < max_rounds ) {
    
    # Ascender goes first
    dice_option <- dice_roll()
    next_dice <- ascender_dice <= dice_option
    
    if(sum(next_dice) == 0) { # We will need to reset here: curr dice 31, options 12 or 21
      if(!ascender_reset) {
        ascender_dice <- min(dice_option) # Reset to the smaller of the two options
        ascender_reset <- TRUE  # Keep track if we reset
        
        ascender_score <- ascender_score + 1  # Increment one to the score
        ascender_arpgeggios[ascender_score] <- ascender_dice  # Record the roll
      }
    }
    
    if(sum(next_dice) == 1) { # Take the next option: curr dice 31, options 42 or 24 
      ascender_dice <- dice_option[next_dice]
      ascender_score <- ascender_score + 1 # Increment one to the score
      ascender_arpgeggios[ascender_score] <- ascender_dice # Record the roll
    }
    
    if(sum(next_dice) == 2) { # If there is a choice between two, take the smallest: curr dice 31, options 45 or 54
      ascender_dice <- min(dice_option)
      ascender_score <- ascender_score + 1 # Increment one to the score
      ascender_arpgeggios[ascender_score] <- ascender_dice # Record the roll
    }
    
    # descender goes
    dice_option <- dice_roll()
    next_dice <- descender_dice >= dice_option
    
    if(sum(next_dice) == 0) { # We will need to reset here: curr dice 21, options 34 or 43
      if(!descender_reset) {
        descender_dice <- max(dice_option) # Reset to the large of the two options
        descender_reset <- TRUE # Keep track if we reset
        descender_score <- descender_score + 1 # Increment one to the score
        descender_arpgeggios[descender_score] <- descender_dice # Record the roll
        
      }
    }
    
    if(sum(next_dice) == 1) { # Take the next option: curr dice 42, options 25 or 52 
      descender_dice <- dice_option[next_dice]
      descender_score <- descender_score + 1 # Increment one to the score
      descender_arpgeggios[descender_score] <- descender_dice # Record the roll
    }
    
    if(sum(next_dice) == 2) { # If there is a choice between two, take the largest: curr dice 31, options 21 or 12
      descender_dice <- max(dice_option)
      descender_score <- descender_score + 1 # Increment one to the score
      descender_arpgeggios[descender_score] <- descender_dice # Record the roll
    }
    
    curr_round <- curr_round + 1
    
  }
  
  # Determine who is the winner.  Note: if both acheive a score of 10, then they are considered a winner.
  
  if(ascender_score == 10 & descender_score < 10) {winner = "ascender"}
  if(ascender_score < 10 & descender_score == 10) {winner = "descender"}
  if(ascender_score == 10 & descender_score == 10) {winner = "both"}
  
  # Make of list of stats we can keep track of.
  out_data <- list(winner = winner,
                   descender = descender_arpgeggios,
                   ascender = ascender_arpgeggios,
                   rounds = curr_round)
  
  
  return(out_data)
  
}

# Play the game
arpeggios_game()


# Replicate!

n_games <- 1000
out_rounds <- replicate(n_games, {
  arpeggios_game()$rounds
  
})

hist(out_rounds,
     breaks = seq(min(out_rounds) - 10, max(out_rounds) + 10, by = 10),
     right = FALSE,
     main = "Arpeggio Rounds",
     xlab = "Values",
     ylab = "Frequency",
     col = rgb(0, 0, 1, 0.5),  # Blue with transparency
     border = "black")

summary(out_rounds)

# Is there a winner more often than the other?
replicate(n_games, {
  arpeggios_game()$winner
  
}) |> table()/n_games

  