# Experimenting with R
# Julio Huato
rm() # Removes all objects from workspace
cat("\014") # Clears the consol
A <- matrix(c(1,2,3,0),nrow=2,ncol=2) # filled by column
B <- matrix(c(0,3,2,1),nrow=2,ncol=2,byrow=T) # filled by row
A
B
C <-A%*%B # matrix multiplication
C
t(C) # transpose of C
D <- 10*A # Hadamard product (scalar multiplication of a matrix)
D

X <- matrix(c(1,3,5,2,4,6), nrow= 3)
X
Y <- matrix(c(8,8,8,8,8,8), nrow= 2)
Y
require(matrixcalc)
Z <- X %*% Y
matrix.trace(Z)

# Curve plotting
curve(exp(-(1/2)*(x^2)/sqrt(2*pi)), from=-4, to=4, type="l")

