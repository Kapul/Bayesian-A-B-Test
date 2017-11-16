rm(list=ls())
library(stats)

##################
# Generate Beta
##################

set.seed(1)
a1 <- 1
b1 <- 1
a2 <- 2
b2 <- 2

Pa <- rbeta(1, a1, b1)
Pb <- rbeta(1, b2, b2)

##################
# Function to compute posterior distribution
##################

posterior <- function(alpha1, beta1, alpha2, beta2, Ya, Yb){
  if(Ya == 1){ # For Beta distribution: posterior B(a+success, b+failure)
    Palpha1 <- alpha1 + 1
    Pbeta1 <- beta1
  } else{
    Palpha1 <- alpha1
    Pbeta1 <- beta1 + 1
  }
  if(Yb == 1){
    Palpha2 <- alpha2 + 1
    Pbeta2 <- beta2
  } else{
    Palpha2 <- alpha2
    Pbeta2 <- beta2 + 1
  }
  return(c(Palpha1, Pbeta1, Palpha2, Pbeta2))
}

##################
# Function to compute probabilities that A is better than B
##################

estimate.prob <- function(alpha1, beta1, alpha2, beta2){
  N <- 100000
  Za <- rbeta(N, alpha1, beta1)
  Zb <- rbeta(N, alpha2, beta2)
  I <- (Za >= Zb)
  prob <- sum(I)/N
  return(prob)
}

##################
# Generate outcomes
##################

Nt <- 10000
Ya <- c()
Yb <- c()
for (t in 1:Nt){
  Ya[t] <- rbinom(1, 1, Pa)
  Yb[t] <- rbinom(1, 1, Pb)
}

##################
# Run experiment
##################

i <- 0
Ni <- 10000
tol <- 0.05
P <- 1
new_coef <- c(1, 1, 1, 1)
while (i <= Ni & P > tol){
  i <- i + 1
  new_coef <- posterior(new_coef[1], new_coef[2], new_coef[3], new_coef[4], Ya[i], Yb[i])
  P1 <- estimate.prob(new_coef[1], new_coef[2], new_coef[3], new_coef[4])
  P2 <- 1 - P1
  P <- min(P1, P2)
}





