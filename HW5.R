n<-40
beta <- matrix(c(-1.89, 1.68), ncol=1)
X<-cbind(rep(1,n), runif(n,1,10))
head(X)
error = rep(NA,n)

for (i in 1:n)
{
error[i]<- rnorm(1,mean= 0,sd=X[i,2]*6)
}
head(error)

Y<-X%*%beta+error
beta_hat <- solve(t(X)%*%X)%*%t(X)%*%Y
View(beta_hat)

Y_hat <- X%*% beta_hat
residuals= Y-Y_hat
plot(X[,2], residuals, xlab='Y_hat', ylab='Residuals' )
abline(h=0)

xtx_inv <- solve(t(X)%*%X)
sigma2_hat <-crossprod(Y-X%*%beta_hat)/(n-1)
#View(sigma2_hat)
#var_beta <- as.numeric(sigma2_hat)*xtx_inv
var_yhat <-as.numeric(sigma2_hat)*X%*%xtx_inv%*%t(X)
#View(var_yhat)
alpha <-0.5
ts<-sapply(c(alpha/2, 1-alpha/2), qt, df=n-1)
bounds <- sapply(ts,function(t){t*sqrt(diag(var_yhat))})
#View(bounds)
#beta_CI <- matrix(rep(beta_hat,2), ncol=2)+bounds
#View(beta_CI)
Y_upperbound <- Y_hat+bounds[,1] #X%*%beta_CI
Y_lowerbound <- Y_hat+bounds[,2]

#bootstrap
B<-1000
output_boot<-matrix(NA, ncol=2, nrow=B)
for (i in 1:B)
{
boot_ind <-sample(1:n,n, replace=TRUE)
boot_x<-X[boot_ind,]
boot_y<-Y[boot_ind,]
boot_slr <-lm(boot_y~ boot_x-1)
output_boot[i,] <-boot_slr$coefficients
}

#View(output_boot)
#summary(output_boot)
#str(output_boot)
Y_boot <- output_boot%*%t(X)
head(Y_boot)
Yboot_bounds <-apply(Y_boot,2,quantile,c(0.025,0.975))
#View(Yboot_bounds)

Yboot_upperbound <- Yboot_bounds[1,] #X%*%beta_CI
Yboot_lowerbound <- Yboot_bounds[2,]

plot(X[,2],Y)
slr <-lm(Y~X[,2])
summary(slr)
abline(a=slr$coefficients[1], b= slr$coefficients[2], col = 'red')
lines((smooth.spline(X[,2],Y_upperbound, spar=0.35)),col="blue")
lines((smooth.spline(X[,2],Y_lowerbound, spar=0.35)),col="blue")
lines((smooth.spline(X[,2],Yboot_upperbound, spar=0.35)),col="black")
lines((smooth.spline(X[,2],Yboot_lowerbound, spar=0.35)),col="black")
legend(1.3,-22, legend=c("SLR line", "95% CI assuming Const Var","95% CI bootstrap"),
       col=c("red", "blue",'black'), lty=1:1, cex=0.7)
