# MAT 373, Day 28

# Example 1: The broken stick

# Determine a breakpoint for a stick.
x <- runif(1)

# Break y again
y <- runif(1,min = 0,max=x)

### Do this several times
n_times <- 10000
Y <- replicate(n_times, {

  x <- runif(1)
  
  runif(1,min = 0,max=x)
}
)
hist(Y)
mean(Y)

# Example 2: Marbles in a jar
# Created with chatGPT prompt: "get a random sample of three integers that sum to 100 in R
marble_colors <- 3        # Number of marble colors

# Generate two random points to split the sum of 100 into three parts
random_points <- sort(sample(1:99, marble_colors - 1))
random_colors <- diff(c(0, random_points, 100))

# The first value is the number of red balls
random_colors[1]



### Do this several times
n_times <- 10000
red_marbles <- replicate(n_times, {
  
  marble_colors <- 3       # Number of marble colors
  
  # Generate two random points to split the sum of 100 into three parts
  random_points <- sort(sample(1:99, marble_colors - 1))
  random_colors <- diff(c(0, random_points, 100))
  
  # The first value is the number of red marbles
  random_colors[1]
  
  
}
)
hist(red_marbles)
mean(red_marbles)

