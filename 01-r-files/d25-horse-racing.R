# MAT 373: Day 25, Horse/Wind Racing in R

# You will need the tidyverse library for some of the data wrangling
library(tidyverse)

# Define game board you will play 
game_board <- data.frame(outcomes = c("twoEight","threeSeven","fourSix","five"),
                    prob = c(2/16, 4/16, 6/16, 4/16),
                    stopping_point = c(6,6,6,6)
                    )

# Input number of trials
trials <- 1000

# A list to store results
results <- vector(mode="list",length = trials)

for (k in 1:trials) {
  
  # Set up this trial's score
  end <- 0  # Indicator variable if we are at stopping point or not
  n_rolls <- 0  # Current number of rolls
  scores <- data.frame(outcomes = game_board$outcomes,
                   tally = 0,
                   stopping_point = game_board$stopping_point
  )
  
  while (end == 0) {
    # Determine a roll
    roll <- sample(game_board$outcomes, size = 1, prob = game_board$prob,replace=TRUE)
    
    # Increment the roll's tally by 1
    scores$tally[which(roll==scores$outcomes)] <- scores$tally[which(roll==scores$outcomes)]+1
    n_rolls <- n_rolls + 1
    
    if (any(scores$tally == scores$stopping_point) ) { # We get to the stopping point
      end <- 1
      
      # Determine the winner
      curr_winner <- scores$outcomes[which(scores$tally == scores$stopping_point)]
      results[[k]] <- tibble(winner = curr_winner, rolls = n_rolls)

    }
  }
}
    
final_results <- tibble(trial = 1:trials,results=results) |> unnest(cols=c(results))


# Just do the final tally
final_results |> 
  group_by(winner) |> 
  summarize(freq = n()/trials)

### YOUR TURN TO EXPLORE!


###### PART 2: Exploring the distribution of rolls

# Define game board you will play 
game_board <- data.frame(outcomes = c("twoEight","threeSeven","fourSix","five"),
                         prob = c(2/16, 4/16, 6/16, 4/16),
                         stopping_point = c(6,6,6,6)
)

roll_trials <- 1000
roll_results <- vector(mode="numeric",length = roll_trials)

# Identify which trial winner and their target
trial_winner = "twoEight"
track_target <- game_board$stopping_point[game_board$outcomes==trial_winner]

for (k in 1:roll_trials) {
  
  # Set up this trial's score
  end <- 0  # Indicator variable if we are at stopping point or not
  n_rolls <- 0
  scores <- tibble(outcomes = game_board$outcomes,
                   tally = 0,
                   stopping_point = game_board$stopping_point
  )
  
  
  while (end == 0) {
    
    # Determine a roll
    roll <- sample(game_board$outcomes, size = 1, prob = game_board$prob)
    
    # Increment the trial_winner's tally by 1 (everything else remains 0)
    if(roll == trial_winner) {
      scores$tally[scores$outcomes==trial_winner] <- scores$tally[scores$outcomes==trial_winner] + 1
    }
    
    n_rolls <- n_rolls + 1
    
    # Determine the winner (this will always be trial_winner)
    if (any(scores$tally == scores$stopping_point) ) {
      
      end <- 1
      roll_results[[k]] <- n_rolls

    }
  }
  
}


# Compare the simulated roll length with the theoretical probability
x <- seq(track_target, max(roll_results), by=1)
roll_stopping_point <- game_board$stopping_point[game_board$outcomes==trial_winner]
roll_success_prob <- game_board$prob[game_board$outcomes==trial_winner]
y <- dnbinom(x-track_target,size=track_target,prob=roll_success_prob)


# Now plot the histogram of roll results
hist(roll_results, 
     main="Histogram of Rolls w/ Theoretical Probability", 
     xlab="Value", 
     ylab="Frequency",
     col="skyblue", 
     border="black",
     probability = TRUE,
     ylim = c(0,max(y)),
     breaks = seq(min(roll_results) - 0.5, max(roll_results) + 0.5, by = 1))


# Create the first PMF plot
lines(x, y, type = "h", lwd = 2, col = "blue")


