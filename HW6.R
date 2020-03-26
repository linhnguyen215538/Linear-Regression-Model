#Q1
theo_cov<-matrix(c(10,0,20,0,100,100,20,100,148.33), nrow=3,ncol=3,byrow=T)
View(theo_cov)
#Q2
Frob_norm <- rep(NA,200)
for (i in 1:200)
{
x <-rnorm(100,0,sqrt(10))
y<-rexp(100,0.1)
error<- runif(100,-5,5)
z<- y+2*x+error
cov_hat<-matrix(c(var(x),cov(x,y),cov(x,z),cov(y,x),var(y),cov(y,z),cov(z,x), cov(z,y),var(z)),ncol=3,nrow=3,byrow=T)
#View(cov_hat)
D<- theo_cov-cov_hat
Frob_norm[i]<- sqrt(sum(D^2))
#View(Frob_norm)
}
quantile(Frob_norm,0.025)
quantile(Frob_norm,.975)

Frob_norm2 <- rep(NA,200)
for (i in 1:200)
{
  x <-rnorm(10000,0,sqrt(10))
  y<-rexp(10000,0.1)
  error<-runif(10000,-5,5)
  z<-y+2*x+error
  cov_hat<-matrix(c(var(x),cov(x,y),cov(x,z),cov(y,x),var(y),cov(y,z),cov(z,x), cov(z,y),var(z)),ncol=3,nrow=3,byrow=T)
  #View(cov_hat)
  D<- theo_cov-cov_hat
  Frob_norm2[i]<- sqrt(sum(D^2))
  #View(Frob_norm)
}
quantile(Frob_norm2,0.025)
quantile(Frob_norm2,.975)

#Q3
#x3<-cbind(rep(1,20),matrix(rnorm(20,0,10), nrow=20))
err_matrix <- matrix(NA, nrow = 10000, ncol= 20)
for (i in 1:10000)
{
err_matrix[i,]<-rnorm(20,0,2)
}

err_mean <- colMeans(err_matrix)
View(err_mean)
#head(err_matrix)
diff_matrix <- err_matrix - err_mean
#head(diff_matrix)
#diff <- matrix(NA, nrow = 10000, ncol= 20)
cov_est = diag(20)
cov_est = (t(diff_matrix)%*%(diff_matrix))/10000
View(cov_est)
#for (i in 1:10000)
#{
#  cov_hat = err_matrix[i]
#}
  
#Q4
x0<-rep(1,20)
x1<-rbinom(20,1,0.3)
x2<-runif(20,-10,10)
x3<-rnorm(20,100,10)
x4<-2*x1-x2+0.3*x3
x5<-1-x1
x<-cbind(x0,x1,x2,x3,x4,x5)
eigen(t(x)%*%x)
xb<-cbind(x1,x2,x3,x4,x5)
eigen(t(xb)%*%xb)
xc<-cbind(x1,x2,x3,x5)
eigen(t(xc)%*%xc)
xd<-cbind(x0,x2,x3,x4,x5)
eigen(t(xd)%*%xd)
xe<-cbind(x2,x3,x4,x5)
eigen(t(xe)%*%xe)
xf<-cbind(x0,x1,x2,x3,x4)
eigen(t(xf)%*%xf)
xg<-cbind(x0,x1,x5)
eigen(t(xg)%*%xg)

#Q5
z5<-qbinom(c(0.025,0.975),100,0.05)
z5

#Q6
y6<- rnorm(1000,0,10)
x6<- matrix(NA,nrow= 1000, ncol=99)
for (i in 1:1000)
{
x6[i,]<- rnorm(99,2,16)
}
x6<-cbind(rep(1,1000),x6)

xtx_inv <-solve(t(x6)%*%x6)
hat_beta <-xtx_inv%*%t(x6)%*%y6
hat_sigma2 <- crossprod(y6-x6%*%hat_beta)/(1000-100)
var_beta <- as.numeric(hat_sigma2) * xtx_inv
SE_beta <- sqrt(diag(var_beta))
View(SE_beta)
sig_features<- hat_beta/SE_beta
sig_fea_count = rep(0,100)
for (i in 1:100)
{
  if(abs(sig_features[i])> qt(0.975,900))
  {
    sig_fea_count[i]=1
  }
}
sig_fea_count
sum(sig_fea_count)

#Q7
k=0
for (j in 1:1000)
{
y6<- rnorm(1000,0,10)
x6<- matrix(NA,nrow= 1000, ncol=99)
for (i in 1:1000)
{
  x6[i,]<- rnorm(99,2,16)
}

x6<-cbind(rep(1,1000),x6)

xtx_inv <-solve(t(x6)%*%x6)
hat_beta <-xtx_inv%*%t(x6)%*%y6
hat_sigma2 <- crossprod(y6-x6%*%hat_beta)/(1000-100)
var_beta <- as.numeric(hat_sigma2) * xtx_inv
SE_beta <- sqrt(diag(var_beta))

A<- abs(hat_beta/SE_beta)

if(max(A) > qt(1-0.05/200,900))
  {
  k=k+1
  }
}

#Q8
x <-rnorm(200,10,sqrt(10))
z <-rbinom(200,1,0.4)
error <- runif(200,-3,3)
y<-5-x+2*z+3*x*z+error
xz<-x*z
slr<-lm(y~x+z+xz)
slr$coefficients

z0<-which(z==0)
z1<-which(z==1)

slr0<-lm(y[z0]~x[z0])
slr0$coefficients

slr1<-lm(y[z1]~x[z1])
slr1$coefficients
