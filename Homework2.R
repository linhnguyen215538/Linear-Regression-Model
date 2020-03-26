x <- rnorm(33,50,10)
error <- rnorm(33,0,5)
y <- 10 - 2*x + error
plot(x,y)
write.csv(cbind(x,y),file="xy.csv")

least_squares_dist <-function(params, u,v)
{
  a <- params[1]
  b <- params[2]
  x <- (u-a*b+b*v)/(1+b^2)
  y <- a+b*x
  squared_distance <- (x-u)^2 + (y-v)^2
  return (sum(squared_distance)/length(u))
}
least_squares_dist(c(10,-2),x,y)
optim(c(10,-2),least_squares_dist,u=x,v =y)

mean(x)
mean(y)

a <- rep (0,1000)
b <- rep (0,1000)
x <- rnorm(33,50,10)
for (i in 1:1000)
{
  error <- rnorm(33,0,5)
  y <- 10 - 2*x + error
  
  ans <- optim(c(10,-2),least_squares_dist,u=x,v =y)$par
  a[i] <- ans[1]
  b[i] <- ans[2]
}

hist(a)
hist(b)
abline(v=10, col="blue")
abline(v=-2, col="red")
mean(a)
mean(b)
# elise_fun <- function(params, a, b, u, v){
#   x <- params[1]
#   return (x*x-2*x*u+u*u+v*v-2*a*v+2*b*v*x+a*a+b*b*x*x+2*a*b*x)
# }
# optim(c(0),elise_fun,a=2,b=1,u=2,v=2)
  

ans <- optim(c(10,-2),least_squares_dist,x=x,y =y)$par




