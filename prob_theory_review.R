# Review of Probability Theory
# Julio Huato / SFC

# My functions to compute (1) the length of the sample space of a r.v., 
# (2) its expected value, (3) its variance, (4) its standard deviation, 
# and (5) coefficient of variation.  These are all POPULATION stats.

# Test that the sum of prob = 1.
testp <- function(p){
  sum(p) == 1
}

# Expectation, var, sd, and coeff of var functions:

popEx <- function(x, p){
  sum(p*x)
}

popvarx <- function(x, p){
  sum(p*(x-popEx(x, p))^2)
}

popsdx <- function(x, p){
  sqrt(popvarx(x, p))
}

popcvx <- function(x, p){
  popsdx(x, p)/popEx(x, p)
}

# Example:
x <- c(2, 5, 8)
px <- c(.3, .45, .25)
testp(px)
popEx(x, px)
popvarx(x, px)
popsdx(x, px)
popcvx(x, px)

# Joint (bivariate) probability distribution
# Functions to compute conditional probabilities,
# conditional expectations, conditional variances,
# and conditional standard deviations.
# Also, population covariance and population 
# correlation between two random varibles.

# Make sure that the values of X are on the rows and 
# the values of Y on the columns.  Then, the joint 
# probabilities below list, first, the probability
# of the first value of X and first value of Y, then
# the probability of the first value of X and 2nd
# value of Y, etc.:

# Test the lengths of joint probability distribution vars.
testlengths <- function(x, y, pxy){
  length(pxy) == length(x)*length(y)
}

# Replace the values of vars x and y:
x <- c(2, 5, 8)
y <- c(0, 1)

# The joint probabilities as a simple list,
# going left to right along each row (across columns),
# then moving to next row, etc.  Make sure that
# the length of the joint probability list is the
# product of the lengths of x and y:
pxy <- c(.05, .15, .3, .2, .25, .05)

# Run these tests:
testp(pxy)
testlengths(x, y, pxy)

# Run the following to structure the joint probability
# list as a matrix and compute the marginal probabilities:
n=length(x)
m=length(y)
pxy <- matrix(unlist(pxy, n*m), ncol = m, byrow = TRUE)
pxy
margpx <- rep(0, n)
margpy <- rep(0, m)
for (i in 1:n) {
  for (j in 1:m) { 
    margpx[i] <- sum(pxy[i,])
    margpy[j] <- sum(pxy[,j])
      } }
margpx
margpy

# Run the following to compute the conditional
# probabilities of y taking each value given each 
# value of x:
pygivenx <- matrix(0, n, m)
pxgiveny <- matrix(0, n, m)
for (i in 1:n) {
  for (j in 1:m) { 
    pygivenx[i, j] <- pxy[i,j]/margpx[i]
    pxgiveny[i, j] <- pxy[i,j]/margpy[j]
  } }
pygivenx
pxgiveny

# Run to compute the conditional expectations, 
# conditional variances, and conditional standard
# deviations of y given each value of x.

# DEBUG FROM HERE...

Eygivenx <- rep(0, n)
varygivenx <- rep(0, n)
for (i in 1:n) { 
  Eygivenx[i] <- popEx(pygivenx[i], y)  
  varygivenx[i] <- popvarx(pygivenx[i], y)
    }
Eygivenx
varygivenx
popvarx(pygivenx[1], y)

popvarx(y, margpy)
margpy
y




# Run to compute the conditional probabilities of
# x taking each value given each value of y:

# Population covariance and correlation between x and y




