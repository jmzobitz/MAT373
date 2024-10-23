# Day 24: What's the area?

# Goal: evaluate the integral of exp(-x^2) from 0 to 1
# Determine how many points you will generate and random x and y points

n_points <- 10^4

x <- runif(n_points,min=0,max=1)
y <- runif(n_points,min=0,max=1)

# How many points are within the area?
sum(exp(-x^2)>y)

# What is the proportion?
sum(exp(-x^2)>y)/n_points


# Change to an indicator variable if you are within the area (TRUE)
within_area <- exp(-x^2)>y

# Create a data table for the running sample proportion:
sample_proportion <- data.frame(n_point=1:n_points,
                           proportion = cumsum(within_area)/1:n_points)

# Now plot the sample proportion as the number of points increase:
plot(sample_proportion$n_point, sample_proportion$proportion, type = "o", col = "darkgreen", 
     xlab = "Point", ylab = "Sample proportion", main = "What's the area?",
     ylim=c(0,1))
