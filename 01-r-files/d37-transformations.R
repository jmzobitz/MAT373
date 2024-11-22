# Generate data

X <- runif(1000)  # Example data

# Plot histogram
hist(-0.5 * log(1 - X), breaks = 50, probability = TRUE, 
     main = "Y=-0.5 * log(1 - X)", xlab = "Value")

# Add a theoretical density line (e.g., exponential density)
curve(dexp(x, rate = 2), add = TRUE, col = "blue", lwd = 2)

# Add a label to the curve
text(x = 1.5, y = 0.8, labels = "Exponential Density\n(rate = 2)", col = "blue", cex = 0.8)
