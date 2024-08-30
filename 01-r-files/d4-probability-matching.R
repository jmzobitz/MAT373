### MAT 373, Day 4
### Simulate probability matching problem

number_of_cards <- 5

# Test out a turn
turn <- sample(1:number_of_cards)  # The cards I flip
turn  # Print out to see

# Check to see if any card is in the correct slot
turn == 1:number_of_cards

# Are we a winner?  We can sum this (FALSE = 0 and TRUE = 1), so of the sum is greater than 0, we are a winner

sum(turn == 1:number_of_cards)

# And can make this a simple test
sum(turn == 1:number_of_cards) > 0

# We can also make this run in one line:
sum(sample(1:number_of_cards) == 1:number_of_cards) > 0
# Now let's replicate

# Time to replicate the results
n_times <- 1000

replicate(n_times, {
  
  # Define a sample roll
  sum(sample(1:number_of_cards) == 1:number_of_cards) > 0
  

}
) |> 
  table() / n_times


