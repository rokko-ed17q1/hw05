
## ----- linear-state-space-representation ----------------------------------

lssr <- function(a, b) {
  # Complete the function definition!!

  
}


## ----- solution-parameters -----------------------------------------------
## DO NOT EDIT

A <- matrix(c(
  0.0, 1.0, 0.0,
  0.0, 0.0, 1.0,
  -0.3, 0.7, 0.5
), nrow = 3, byrow = TRUE)

B <- matrix(c(
  0.0, 0.0, 1.0
))

x0 <- matrix(c(
  0.4678439, 0.5641670, 0.6803218
))

## ----- solution-input ---------------------------------------------------
## DO NOT EDIT

# set.seed(100)
u <- function(n) {
  # Random numbers uniformly distributed between -0.5 and 0.5
  runif(n) - 0.5
}

## ----- solution-simulation ----------------------------------------------

g <- lssr(A, B)
nperiods <- 1000
inputs <- u(nperiods)
result <- matrix(0, nperiods + 1, dim(A)[2])

result[1, ] <- x0
for (t in 1:nperiods) {
  result[t + 1, ] <- g(result[t, ], inputs[t])
}

ggplot2::qplot(0:nperiods, result[, 1], geom = 'line')
