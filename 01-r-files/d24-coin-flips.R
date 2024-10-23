# Day 24: Is this coin fair?

# Read in the sequence of coin flips into your workspace:
coin_flip_file <- "https://raw.githubusercontent.com/jmzobitz/MAT373/refs/heads/main/01-r-files/d24-coin-flips.csv"
flips <- read.csv(coin_flip_file,header=TRUE)$x

# One thing we can do is count the different instances:
table(flips)

# Or the proportion:
table(flips)/length(flips)

# Change the coin flip into TRUE if "H"
head_flip <- flips == "H"

# Create a data table for the running sample mean:

sample_means <- data.frame(n_flip=1:length(head_flip),
                           mean = cumsum(head_flip)/1:length(head_flip))

# Now plot the sample mean as the number of rolls increase:
plot(sample_means$n_flip, sample_means$mean, type = "o", col = "red", 
     xlab = "Flip", ylab = "Sample mean", main = "Is this coin fair?",
     ylim=c(0,1))
