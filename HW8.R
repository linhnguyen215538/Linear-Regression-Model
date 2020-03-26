set.seed(70)
n<-1000
x<-rnorm(n,-5,1)
y<-rnorm(n,5,1)
error<-rnorm(n,0,1)
z<-x+y+error
modz<-lm(y~z)
modxz<-lm(y~z+x)

m<-100
xnew<-rnorm(m,-5,1)
ynew<-rnorm(m,5,1)
errnew<-rnorm(m,0,1)
znew<-xnew+ynew+errnew

yz_pred <- znew*modz$coefficients[2]+modz$coefficients[1]
yxz_pred <- xnew*modxz$coefficients[3]+ znew*modxz$coefficients[2]+modxz$coefficients[1] 
#View(ynew)
#View(yz_pred)
SSEz <- (1/100)*sum((ynew-yz_pred)^2)
SSExz <- (1/100)*sum((ynew-yxz_pred)^2)
#SSEz
#SSExz

#Q3:
n<-1000
x<-rnorm(n,-5,1)
y<-rnorm(n,5,1)
error<-rnorm(n,0,1)
z<-x+y+error
modz<-lm(y~z)
modxz<-lm(y~z+x)

B<-500
difference <-rep(NA,B)
for (i in 1:B)
{
  m<-100
  xnew<-rnorm(m,-5,1)
  ynew<-rnorm(m,5,1)
  errnew<-rnorm(m,0,1)
  znew<-xnew+ynew+errnew
  
  yz_pred <- znew*modz$coefficients[2]+modz$coefficients[1]
  yxz_pred <- xnew*modxz$coefficients[3]+ znew*modxz$coefficients[2]+modxz$coefficients[1] 
  #View(ynew)
  #View(yz_pred)
  SSEz <- (1/100)*sum((ynew-yz_pred)^2)
  SSExz <- (1/100)*sum((ynew-yxz_pred)^2)
  difference[i]<-SSEz-SSExz
}
lowerbound<-quantile(difference,0.025)
upperbound<-quantile(difference,0.975)

#Q4:
group<-rep(c(1:10),100)
x_group <-cbind(group,x)
y_group <-cbind(group,y)
z_group <-cbind(group,z)
diff.q4<-rep(NA,10)
SSEz.q4<-rep(NA,10)
SSExz.q4<-rep(NA,10)
for (i in 1:10)
{
train_x <- x_group[x_group[,1]!=i,2]
train_y <- y_group[y_group[,1]!=i,2]
train_z <- z_group[z_group[,1]!=i,2]

modelz<-lm(train_y~train_z)
modelxz<-lm(train_y~train_z+train_x)

test_x<-x_group[x_group[,1]==i,2]
test_y<-y_group[y_group[,1]==i,2]
test_z<-z_group[z_group[,1]==i,2]

yz_test <- test_z*modelz$coefficients[2]+modelz$coefficients[1]
yxz_test <- test_x*modelxz$coefficients[3]+ test_z*modelxz$coefficients[2]+modelxz$coefficients[1] 
#View(ynew)
#View(yz_pred)
SSEz.q4[i] <- (1/100)*sum((test_y-yz_test)^2)
SSExz.q4[i] <- (1/100)*sum((test_y-yxz_test)^2)
}
m1<-mean(SSEz.q4)
s1<-sd(SSEz.q4)
m2<-mean(SSExz.q4)
s2<-sd(SSExz.q4)

t_val <-(m1-m2)/sqrt(s1^2/10+s2^2/10)
t_val
qt(0.975,18)

#Q7:
n<-100
x<-rnorm(n,0,0.9)
err<-rnorm(n,0,1)
y<-1+5*sin(x)+err
ols<-lm(y~x)
betahat<-matrix(ols$coefficients,ncol=1)
yhat<-matrix(cbind(rep(1,100),x)%*%beta.hat)
xnew<- cbind(rep(1,10),seq(min(x),max(x),length.out= 10))
sigma2.hat<-crossprod(y-yhat)/(n-2)
ynew<-matrix(NA,ncol=10,nrow=1000)
for (i in 1:10)
{
  ynew[,i]<-rnorm(1000,xnew[i,]%*%betahat, sd=sqrt(sigma2.hat))
}
ynew_upperbound<-apply(ynew,2,quantile,0.95)
ynew_lowerbound<-apply(ynew,2,quantile,0.05)
plot(x,y)
lines(xnew[,2],ynew_upperbound,col='red')
lines(xnew[,2],ynew_lowerbound,col='red')
legend(-2,6,'90% Interval','red', cex=0.6)