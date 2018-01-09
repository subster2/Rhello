library(mvtnorm)
x.points <- seq(-3,3,length.out=100)
y.points <- x.points
z <- matrix(0,nrow=100,ncol=100)
mu <- c(1,1)
sigma <- matrix(c(2,1,1,1),nrow=2)
for (i in 1:100) {
for (j in 1:100) {
z[i,j] <- dmvnorm(c(x.points[i],y.points[j]),
mean=mu,sigma=sigma)
}
}
contour(x.points,y.points,z)
