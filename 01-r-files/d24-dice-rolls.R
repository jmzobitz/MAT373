# Day 24: Is this dice fair?

# Read in the sequence of dice rolls into your workspace:
dice_roll_file <- "https://raw.githubusercontent.com/jmzobitz/MAT373/refs/heads/main/01-r-files/d24-dice-rolls.csv"
rolls <- read.csv(dice_roll_file,header=TRUE)$x

# One thing we can do is count the different instances:
table(rolls)

# Or the proportion:
table(rolls)/length(rolls)

# Create a data table for the running sample mean:

sample_means <- data.frame(n_roll=1:length(rolls),
                           mean = cumsum(rolls)/1:length(rolls))

# Now plot the sample mean as the number of rolls increase:
plot(sample_means$n_roll, sample_means$mean, type = "o", col = "blue", 
     xlab = "Roll", ylab = "Sample mean", main = "Is this dice fair?",
     ylim=c(0,6))
