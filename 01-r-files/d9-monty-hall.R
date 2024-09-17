# MAT 373
# Day 9: Monty Hall Simulation


# Monty hall is like sampling the integers 1:3, and figuring out the frequency the car is behind door 1.
n <- 10^5
cardoor <- sample(3,n,replace=TRUE)
sum(cardoor==1)/n

# You can also create an interactive function to play Monty Hall
monty <- function() {
  
  doors <- 1:3
  # randomly pick where the car is
  cardoor <- sample(doors,1)
  
  # prompt player
  print("Monty Hall says 'Pick a door, any door!'")
  
  # receive the player's choice of door (should be 1,2, or 3)
  chosen <- scan(what = integer(), nlines = 1, quiet = TRUE)
  
  # pick Monty's door (can't be the player's door or the car door)
  if (chosen != cardoor) montydoor <- doors[-c(chosen,cardoor)]
  else montydoor <- sample(doors[-chosen],1)
  
  # find out whether the player wants to switch doors
  print(paste("Monty opens door ", montydoor, "!", sep=""))
  print("Would you like to switch (y/n)?")
  reply <- scan(what = character(), nlines = 1, quiet = TRUE)
  
  # interpret what player wrote as "yes" if it starts with "y"
  if (substr(reply,1,1) == "y") {
    chosen <- doors[-c(chosen,montydoor)]
  }
  
  # announce the result of the game!
  if (chosen == cardoor) print("You won!")
  else print("You lost!")
}
