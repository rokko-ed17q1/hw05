## ---- cobb-douglas ---------------------------------------------------------

cobb_douglas <- function(alpha) {
  f <- function(k) k ^ alpha
  # return(f)  ## R implicitly returns f
}
f <- cobb_douglas(alpha = 0.3)
f(3)


## ---- reduced-utility ------------------------------------------------------

reduced_utility <- function(U, f) {
  u <- function(x, y) U(f(x) - y)
}

u_log <- reduced_utility(log, cobb_douglas(0.3))
u_log(3, 1)


## ---- ar1-model ------------------------------------------------------------

linsys_1d <- function(a, x) {
  # AR(1) model
  # a: coefficient
  # x: initial value
  
  state <- x
  update_rule <- function(u) {
    out <- state
    state <<- a * state + u
    return(out)
  }
  return(update_rule)
}

update <- linsys_1d(a = 0.8, x = 0.2)


## ---- autonomous-system -------------------------------------------------

linear_autonomous <- function(a) {
  function(x) a %*% x
}


## ---- coeff-matrix ------------------------------------------------------

A <- matrix(c(
  0.8, 1,
  0, 0.2
), nrow = 2, byrow = TRUE)
A


## ---- autonomous-initial ------------------------------------------------

x10 <- matrix(c(1, 0))
x20 <- matrix(c(0, 10))
x30 <- matrix(c(-0.5, -0.5))


## ---- autonomous-simulation ---------------------------------------------

simulate <- function(x0, update_rule, nperiods) {
  result <- matrix(0, nperiods + 1, length(x0))
  result[1, ] <- x0
  
  for (t in 1:nperiods) {
    result[t + 1, ] <- update_rule(result[t, ])
  }
  result
}


## ---- autonomous-simulation-x10 ---------------------------------------------

tmax <- 100
t <- 0:tmax

result <- simulate(x10, linear_autonomous(A), nperiods = tmax)
x1 <- result[, 1]
ggplot2::qplot(t, x1)


## ---- autonomous-simulation-x20 ---------------------------------------------

result <- simulate(x20, linear_autonomous(A), nperiods = tmax)
x1 <- result[, 1]
ggplot2::qplot(t, x1)

## ---- autonomous-simulation-x30 ---------------------------------------------

result <- simulate(x30, linear_autonomous(A), nperiods = tmax)
x1 <- result[, 1]
ggplot2::qplot(t, x1)
