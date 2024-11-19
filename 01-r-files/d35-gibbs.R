### MAT 373, Day 35
### Simple Gibbs Sampling Problem - adapted from https://stephens999.github.io/fiveMinuteStats/gibbs1.html

# Example 1:
# sample from distribution X given Y above
sample_XgivenY = function(y){
  if(y==0){
    x <- rbinom(1, size = 1, prob = 0.2) # returns 1 with probability 0.2; otherwise 0
  } else {
    x <- rbinom(1, size = 1, prob = 0.6)
  } 
  return(x)
}

#' sample from distribution Y given X above
sample_YgivenX = function(x){
  if(x==0){
    y <- rbinom(1, size = 1, prob = 1/7)
  } else {
    y <- rbinom(1, size = 1, prob = 0.5)
  }
  return(y)
}

# Now let's simulate the joint distribution
niter <- 1000
X <- rep(0,niter)
Y <- rep(0,niter)
X[1] <- 1
Y[1] <- 1 # start from (1,1)
for(i in 2:niter){
  X[i] <- sample_XgivenY(Y[i-1])
  Y[i] <- sample_YgivenX(X[i])
}

# Set this up as a data frame
res <- data.frame(X=X,Y=Y)

# See the top entries
head(res,20)

# Summary table
table(data.frame(X=X,Y=Y))/niter

# Example 2: chicken / egg

x <- 7  # 7 eggs hatched
lambda <- 10
a <- 1
b <- 1

niter <- 10^4
p <- rep(0,niter)
N <- rep(0,niter)

p[1] <- 0.5
N[1] <- 2*x

for (i in 2:niter) {
  p[i] <- rbeta(1,x+a,N[i-1]-x+b)
  N[i] <- x + rpois(1,lambda*(1-p[i-1]))
}

p <- p[-(1:(niter/2))]
N <- N[-(1:(niter/2))]

# Display histograms of p and N
hist(p)
hist(N)

# Report out summary statistics
summary(p)
summary(N)
