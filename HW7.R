n<-1000
error<-rnorm(1000,0,sqrt(20))
#View(error)
beta <- c(0,1.2)
prob= rep(NA,20)
for (k in 1:20)
{
  prob[k] <- 1/sqrt(k)
}
#View(prob)
#sum(prob)
#norm_prob = prob/sum(prob)
#View(norm_prob)
group_length <- rmultinom(1, 1000, prob)

#x<-rbinom(1000,100,0.5) 
#flip the coin 100 times each --> get a certain number of heads, then you draw it 1000 times
x<-as.list(rep(NA,20))
for (i in 1:20)
{
x[[i]]<-rbinom(group_length[i],100,0.5) 
}

#View(x)
mean_x<-lapply(x,mean)
#View(mean_x)
mean_x<-unlist(mean_x)
mean_x <-cbind(rep(1,20),mean_x)

x<-unlist(x)

y<-x*1.2+error
y_list <- as.list(rep(NA,20))
y_list[[1]] <- y[1:group_length[1]]
for (i in 2:20)
{
  count<-0
  for(j in 1:(i-1))
  {
    count <- count+group_length[j]
  }
  y_list[[i]]<- y[(count+1):(count+group_length[i])]
}
#View(y_list)
plot(x,y)
indi_corr <-cor(x,y)
#View(indi_corr)
mean_y <-lapply(y_list,mean)
mean_y<-unlist(mean_y)

plot(mean_x[,2],mean_y)
aggregate_corr<-cor(mean_x[,2],mean_y)
#View(aggregate_corr)

#Q6
xtx_inv<- solve(t(mean_x)%*%mean_x)
hat_beta<-xtx_inv%*%t(mean_x) %*% mean_y
hat_sigma2<-crossprod(mean_y- mean_x %*% hat_beta) / (20-2)
var_beta <-as.numeric(hat_sigma2) *xtx_inv
alpha = 0.05
ts<-sapply(c(alpha/2,1-alpha/2), qt, df = 18)
bounds<-sapply(ts, function(t){t*sqrt(diag(var_beta))})
#View(bounds)
ols.interval<-matrix(rep(hat_beta,2),ncol=2)+bounds

diagvec <- as.vector(group_length)
diagvec2<- as.vector(sqrt(group_length))
#diagvec<-unlist(diagvec)
weight <- diag(diagvec)
weight_sqrt<-diag(diagvec2)
#View(weight_sqrt)
#View(weight)

xtx_inv_wls<- solve(t(mean_x)%*%weight%*%mean_x)
hat_beta_wls<-xtx_inv_wls %*% t(mean_x) %*% weight %*% mean_y
hat_sigma2_wls<-crossprod(weight_sqrt%*%(mean_y- mean_x %*% hat_beta_wls))/(20-2)
var_beta_wls <-as.numeric(hat_sigma2_wls) *xtx_inv_wls
alpha = 0.05
ts<-sapply(c(alpha/2,1-alpha/2), qt, df = 18)
bounds_wls<-sapply(ts, function(t){t*sqrt(diag(var_beta_wls))})
#View(bounds)
wls.interval<-matrix(rep(hat_beta_wls,2),ncol=2)+bounds_wls

#Q8
x <-cbind(rep(1,1000),x)
xtx_ind<- solve(t(x)%*%x)
hat_beta_ind<-xtx_ind%*%t(x) %*% y
hat_sigma2_ind<-crossprod(y-x %*% hat_beta_ind) / (1000-2)
var_beta_ind <-as.numeric(hat_sigma2_ind) *xtx_ind
alpha = 0.05
ts<-sapply(c(alpha/2,1-alpha/2), qt, df = 1000-2)
bounds_ind<-sapply(ts, function(t){t*sqrt(diag(var_beta_ind))})
View(bounds_ind)
ols.interval_ind<-matrix(rep(hat_beta_ind,2),ncol=2)+bounds_ind
View(ols.interval_ind)

#Q9
xnew<-as.list(rep(NA,20))
for (i in 1:20)
{
  xnew[[i]]<-rbinom(group_length[i],100,(i-10)/200+0.5) 
}
View(xnew)

mean_xnew<-lapply(xnew,mean)
View(mean_xnew)
mean_xnew<-unlist(mean_xnew)
mean_xnew <-cbind(rep(1,20),mean_xnew)

xnew<-unlist(xnew)

ynew<-xnew*1.2+error
ynew_list <- as.list(rep(NA,20))
ynew_list[[1]] <- ynew[1:group_length[1]]
for (i in 2:20)
{
  count<-0
  for(j in 1:(i-1))
  {
    count <- count+group_length[j]
  }
  ynew_list[[i]]<- ynew[(count+1):(count+group_length[i])]
}
#View(y_list)
#plot(xnew[,2],ynew)
indi_corr_new <-cor(xnew,ynew)
View(indi_corr_new)
mean_ynew <-lapply(ynew_list,mean)
mean_ynew<-unlist(mean_ynew)

plot(mean_xnew[,2],mean_ynew)
#aggregate_corr<-cor(mean_x[,2],mean_y)

#95% CI for beta using OLS at individual level
xnew <-cbind(rep(1,1000),xnew)
xtx_ind_new<- solve(t(xnew)%*%xnew)
hat_beta_ind_new<-xtx_ind_new%*%t(xnew) %*% ynew
hat_sigma2_ind_new<-crossprod(ynew-xnew %*% hat_beta_ind_new) / (1000-2)
var_beta_ind_new <-as.numeric(hat_sigma2_ind_new) *xtx_ind_new
alpha = 0.05
ts<-sapply(c(alpha/2,1-alpha/2), qt, df = 1000-2)
bounds_ind_new<-sapply(ts, function(t){t*sqrt(diag(var_beta_ind_new))})
View(bounds_ind_new)
ols.interval_ind_new<-matrix(rep(hat_beta_ind_new,2),ncol=2)+bounds_ind_new
View(ols.interval_ind_new)

#95% CI for beta using WLS at aggregate level
diagvec <- as.vector(group_length)
diagvec2<- as.vector(sqrt(group_length))
weight <- diag(diagvec)
weight_sqrt<-diag(diagvec2)

xtx_inv_wls_new<- solve(t(mean_xnew)%*%weight%*%mean_xnew)
hat_beta_wls_new<-xtx_inv_wls_new %*% t(mean_xnew) %*% weight %*% mean_ynew
hat_sigma2_wls_new<-crossprod(weight_sqrt%*%(mean_ynew- mean_xnew %*% hat_beta_wls_new))/(20-2)
var_beta_wls_new<-as.numeric(hat_sigma2_wls_new)*xtx_inv_wls_new
alpha = 0.05
ts<-sapply(c(alpha/2,1-alpha/2), qt, df = 18)
bounds_wls_new<-sapply(ts, function(t){t*sqrt(diag(var_beta_wls_new))})
#View(bounds)
wls.interval_new<-matrix(rep(hat_beta_wls_new,2),ncol=2)+bounds_wls_new
#abline(a=hat_beta_wls_new[1],b=hat_beta_wls_new[2])

#Q10
poss_x <- seq(0,100, length.out= 101)
poss_x<-cbind(rep(1,101),poss_x)
I_mat<-diag(rep(1,101))
y_predict <- poss_x*hat_beta_ind_new[2]+hat_beta_ind_new[1]
cov_mat<-as.numeric(hat_sigma2_ind_new)*(I_mat+poss_x%*%xtx_ind_new%*%t(poss_x))
lower_ybound<-y_predict-qt(1-0.05/2,101-2)*sqrt(diag(cov_mat))
upper_ybound<-y_predict+qt(1-0.05/2,101-2)*sqrt(diag(cov_mat))
plot(xnew[,2], ynew)
#plot(poss_x,y_predict,type='l')
lines(poss_x,lower_ybound, type='l',col='red')
lines(poss_x,upper_ybound, type='l',col='red')

#Q11
cov_mat_11<-as.numeric(hat_sigma2_wls_new)*(I_mat+poss_x%*%xtx_inv_wls_new%*%t(poss_x))
ypredict_wls <- poss_x*hat_beta_wls_new[2]+hat_beta_wls_new[1]
lower_ybound11<-ypredict_wls-qt(1-0.05/2,101-2)*sqrt(diag(cov_mat_11))
upper_ybound11<-ypredict_wls+qt(1-0.05/2,101-2)*sqrt(diag(cov_mat_11))

#plot(poss_x,y_predict,type='l')
lines(poss_x,lower_ybound11, type='l',col='blue')
lines(poss_x,upper_ybound11, type='l',col='blue')
legend(28,85,c('Individual','WLS'),c('red','blue'), cex=0.6)

#Q14
err<-rnorm(1000,0, sqrt(10))
x<-matrix(runif(99000,0,1),ncol=99)
x<-cbind(rep(1,1000),x)
beta<-matrix(rep(0,100),ncol=1)
y<-x%*%beta+err
beta_hat_ols<-lm(y~x-1)$coefficients

gamma_obj_func <- function(gamma)
{
  val<- (gamma^2)*10*sum(diag((solve(t(x)%*%x))))
}

opt<-optimise(interval=c(0,1),gamma_obj_func)
opt$minimum

#Q15
mu<-rep(2,100)
gamma<-0.99
mse_ols<-rep(NA,100)
mse_shrink<-rep(NA,100)
for (i in 1:100)
{
  error<-rnorm(1000,0,sqrt(10))
  x<-matrix(runif(99000,0,1),ncol=99)
  x<-cbind(rep(1,1000),x)
  beta<-matrix(rep(0,100),ncol=1)
  beta_0<- rep(0,100)
  y<-x%*%beta+error
  beta_hat_ols<-lm(y~x-1)$coefficients
  beta_shrink <- gamma*(beta_hat_ols-mu)+mu
  mse_ols[i]<-t(beta_hat_ols-beta_0)%*%(beta_hat_ols-beta_0)
  mse_shrink[i]<-t(beta_shrink-beta_0)%*%(beta_shrink-beta_0)
}
mean(mse_ols)
mean(mse_shrink)