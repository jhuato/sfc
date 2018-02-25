# Review of Probability Theory
# Julio Huato / SFC

# My functions to compute (0) the length of the sample space of a r.v., 
# (1) its expected value, (2) its variance, (3) its standard deviation, 
# and (4) coefficient of variation:

# Test that sum(p) = 1.
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

# Run to compute the unconditioned expectations,
# variances, and stand devs.
popEx(x, margpx)
popvarx(x, margpx)
popsdx(x, margpx)
popEx(y, margpy)
popvarx(y, margpy)
popsdx(y, margpy)

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
# deviations of y given each value of x, and of
# x given each value of y.
Eygivenx <- rep(0, n)
varygivenx <- rep(0, n)
sdygivenx <- rep(0, n)
for (i in 1:n) { 
  Eygivenx[i] <- popEx(y, pygivenx[i,])  
  varygivenx[i] <- popvarx(y, pygivenx[i,])
  sdygivenx[i] <- popsdx(y, pygivenx[i,])
}
Eygivenx
varygivenx
sdygivenx

Exgiveny <- rep(0, m)
varxgiveny <- rep(0, m)
sdxgiveny <- rep(0, m)
for (j in 1:m) { 
  Exgiveny[j] <- popEx(x, pxgiveny[,j])  
  varxgiveny[j] <- popvarx(x, pxgiveny[,j])
  sdxgiveny[j] <- popsdx(x, pxgiveny[,j])
}
Exgiveny
varygivenx
sdygivenx

# Law of iterated expectations:
popEx(Eygivenx, margpx) == popEx(y, margpy)
popEx(Exgiveny, margpy) == popEx(x, margpx)

# Run to compute the population covariance and 
# correlation between x and y:
devx <-  rep(0, n)
devy <-  rep(0, m)
popcovxy1 <- matrix(0, n, m)
popcovxy <- 0
for (i in 1:n) { 
  for (j in 1:m) {
    devx[i] <- x[i]- popEx( x, margpx )
    devy[j] <- y[j]- popEx( y, margpy )
    popcovxy1[i, j] <- sum( pxy[i, j]*devx[i]*devy[j] )
    popcovxy <- sum(popcovxy1)
  } } 
popcovxy

popsdx(x, margpx)*popsdx(y, margpy)
popcorrelxy <- popcovxy/( popsdx(x, margpx) * popsdx(y, margpy) )
popcorrelxy

# Bayes theorem
x
y
pxy
margpx
margpy
pygivenx
pxgiveny

condpxgiveny <- matrix(0, n, m)
condpxgiveny
for (i in 1:n) { for (j in 1:m) {
  condpxgiveny[i, j] <- margpx[i]*(pygivenx[i,j]/sum(margpx*pygivenx[,j]))
} }
condpxgiveny

# Test:
condpxgiveny == pxgiveny

# In-class exercises
x <- c(0, 1)
y <- c(10, 20, 30)

pxy <- c(0.10, 0.20, 0.05, 0.05, 0.35, 0.25)

# Determine the unconditional (or plain) standard deviations of
# x and y, respectively. Compute the marginals first.

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
popEx(x, margpx)
popEx(y, margpy)
popvarx(x, margpx)
popvarx(y, margpy)
popsdx(x, margpx)
popsdx(y, margpy)

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
Eygivenx <- rep(0, n)
varygivenx <- rep(0, n)
sdygivenx <- rep(0, n)
for (i in 1:n) { 
  Eygivenx[i] <- popEx(y, pygivenx[i,])  
  varygivenx[i] <- popvarx(y, pygivenx[i,])
  sdygivenx[i] <- popsdx(y, pygivenx[i,])
}
Eygivenx
varygivenx
sdygivenx

# Run to compute the population covariance and 
# correlation between x and y:
devx <-  rep(0, n)
devy <-  rep(0, m)
popcovxy1 <- matrix(0, n, m)
popcovxy <- 0
for (i in 1:n) { 
  for (j in 1:m) {
    devx[i] <- x[i]- popEx( x, margpx )
    devy[j] <- y[j]- popEx( y, margpy )
    popcovxy1[i, j] <- sum( pxy[i, j]*devx[i]*devy[j] )
    popcovxy <- sum(popcovxy1)
  } } 
popcovxy

popsdx(x, margpx)*popsdx(y, margpy)
popcorrelxy <- popcovxy/( popsdx(x, margpx) * popsdx(y, margpy) )
popcorrelxy
