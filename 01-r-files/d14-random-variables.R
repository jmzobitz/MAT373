# MAT 373, Day 14
# Working with hypergeometric and binomial distribution
# Example: The prevalence of some disease in a given country is p. A sample of n<N people is selected from a city with N inhabitants (in that same country). What is the probability that exactly k people in this sample have the disease?

# Let N = 100,000, n =500, k = 10, p = 0.02
dhyper(10,0.02*100000,.98*100000,500)
dbinom(10,500,.02)


# Remixed from chatGPT: "plot two discrete density functions in R"
#####
hyper_binom_plot <- function(N) {
  
  # N: size of population (must be as big as 500)
  # p: probalility of success
  # n: number of drawn successes
  p <- 0.02
  n <- 500
  
  
  
  # x values (number of successes) from 0 to 20
  x <- 0:20
  
  y_hyper <- dhyper(x,round(p*N),round((1-p)*N),n)
  y_binom <- dbinom(x,n,p)
  
  # Binomial PMFs for the two distributions
  #y1 <- dbinom(x, size = n1, prob = p1)
  #y2 <- dbinom(x, size = n2, prob = p2)
  
  # Create the first PMF plot
  plot(x, y_hyper, type = "h", lwd = 2, col = "blue", 
       xlab = "Number of Successes", ylab = "Probability", 
       main = "Comparison of Two PMFs", ylim = c(0, max(y_hyper, y_binom)))
  
  # Add points for the first PMF
  points(x, y_hyper, pch = 19, col = "blue")
  
  # Add the second PMF as a line plot
  lines(x, y_binom, type = "h", lwd = 2, col = "red")
  
  # Add points for the second PMF
  points(x, y_binom, pch = 19, col = "red")
  
  # Add a legend
  legend("topright", legend = c("Hypergeometric", "Binomial"), 
         col = c("blue", "red"), lty = 1, pch = 19, lwd = 2)
  
 
  
}


# Compare when N = 100 000 and N = 1000
hyper_binom_plot(10000)
hyper_binom_plot(1000)

# Lesson learned: when N is large, p = w / (w+b) fixed, and n / N < 0.1, then the 
# hypergeometric can be approximated with the binomial

### Example 2: Additivity of binomial distributions
# If X ~ Bin(n,p) and Y~Bin(m,p) and X is independent of Y, then X+Y ~ Bin(n+m,p)


# ChatGPT prompt: "add line plot to histogram base R"
# Other example:

# Generate random samples --> notice the use of rbinom
x <- rbinom(100000,size=30,prob = 0.3)
y <- rbinom(100000,size=20,prob = 0.3)
data <- x+y


### What follows is plotting the data
# Define the range of the data and create bins of width 1
min_value <- floor(min(data))  # The minimum value rounded down
max_value <- ceiling(max(data))  # The maximum value rounded up
# Create a histogram


# Define the bin centers and shift by 0.5 to center the bins
center <- seq(min_value-1,max_value+1, by = 1) - 0.5

hist_data <- hist(data, breaks =center, col = "lightblue", 
                  xlab = "Value", ylab = "Frequency", 
                  main = "Histogram with Bin(n+m,p)", freq = FALSE, ylim = c(0,0.3))


# Add the line plot on top of the histogram
points(0:50,dbinom(0:50,size=50,prob = 0.3), col = "red", pch=20,cex = 1)

# Add points for the second PMF
lines(0:50,dbinom(0:50,size=50,prob = 0.3), type="h",lwd=2, col = "red")


# Add a legend
legend("topright", legend = c("X+Y", "Bin(n+m,p)"), 
       col = c("lightblue", "red"), lty = 1, pch = 19, lwd = 2)



