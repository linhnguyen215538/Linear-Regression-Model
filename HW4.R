x <-rnorm(100,50,10)
error <- rnorm(100,0,5)
y <- -100 + 0.3*x+ error
write.csv(cbind(x,y), file = "xy.csv")

slr <- lm(y~x)
sqr_loss_from_mean <- sum((y-mean(y))^2)
r_squared <- sum((slr$fitted.values-mean(y))^2)/sqr_loss_from_mean

yhat <-slr$fitted.values
slr2 <-lm(y~ yhat)
r_squared2 <- sum((slr2$fitted.values-mean(y))^2)/sqr_loss_from_mean

y_foo <- yhat +2
slr3 <- lm(y~y_foo)
r_squared3 <- sum((slr3$fitted.values-mean(y))^2)/sqr_loss_from_mean


difference_obj <- function(b)
{
  y_bias <- yhat + b
  difference <- sum((y-y_bias)^2) - sqr_loss_from_mean
  return(abs(difference))
}
optim(2, difference_obj)

uniroot(interval=c(0,3), difference_obj)
difference_obj2 <- function(bias)
{
  y_bias <- yhat - bias
  difference2 <- sum((y-y_bias)^2) - sqr_loss_from_mean
  return(difference2)
}

uniroot(interval=c(0,3), difference_obj2)