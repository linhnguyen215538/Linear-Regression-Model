#install.packages("glmnet")
library(glmnet)
data<-read.csv('C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Linear Reg\\Linear Regression HW\\HW9\\cleaned_tmax_201001_201905.csv', header = TRUE)
attach(data)

# count<-0
# for (i in 1:1095)
#   for (j in 1:115)
# {
#   if(is.na(data[j,i])==0)
#   {
#     count=count+1
#     data[,i]<-NULL
#   }
#     return (count)
# }
# count
cln_cols<- apply(data,2,function(x) mean(is.na(x))==0)
null_cols<- apply(data,2,function(x) mean(is.na(x))==1)
data[,null_cols]<-NULL
dim(data)

#Q3
index<-which(names(data)=='USH00487388')
index
y<- as.matrix(data[,index])
x<- as.matrix(data[,-index])

train_y<-as.matrix(y[1:103,])
test_y<-y[104:115,]

decomp<-svd(x)
eigenvalue<-as.matrix(NA, nrow=115,ncol=0)
total_eigen<-sum((decomp$d)^2)
total_eigen
for (k in 1: 115)
{
  
  eigenvalue[k]<-sum((decomp$d[1:k])^2)/total_eigen
}
plot(eigenvalue)
#View(eigenvalue)
#cumsum(decomp$d^2)/total_eigen

#Q4
W=x%*%decomp$v[,1:2]
plot(W[,1], type='l',col='red', ylim=c(-1e5,5e4))
lines(W[,2],col='blue')
abline(h=0)

#Q5
train_x<-x[1:103,]
test_x<-x[104:115,]

decomp_train<-svd(train_x)
decomp_test<-svd(test_x)
# test_w=cbind(matrix(1,12),test_x%*%decomp_test$v[,1:2])
# train_w=cbind(matrix(1,103),train_x%*%decomp_train$v[,1:2])
test_w=as.matrix(test_x%*%decomp_train$v[,1:2])
train_w=as.matrix(train_x%*%decomp_train$v[,1:2])

mod_train<-lm(train_y~train_w)
betahat<-as.matrix(mod_train$coefficients)
yhat<- cbind(1,test_w)%*%betahat
prediction_err<-sqrt((1/12)*sum((test_y-yhat)^2))
prediction_err

#Q7
lasso.cv<- cv.glmnet(train_x, train_y, lambda = 10^seq(1.8, -3, length.out = 200), alpha = 1)
plot(lasso.cv)
gamma<-lasso.cv$lambda.min

lasso<-glmnet(train_x,train_y, alpha=1,lambda=gamma, intercept=TRUE)
y_lassohat<-predict.glmnet(lasso,newx=test_x,s=gamma)
#View(y_lassohat)
lasso_err<-sqrt((1/12)*sum((test_y-y_lassohat)^2))
lasso_err

#Q8
ridge.cv<- cv.glmnet(train_x, train_y, lambda = 10^seq(-3, 3, length.out = 200), alpha = 0)
plot(ridge.cv)
gamma_ridge<-ridge.cv$lambda.min
gamma_ridge
ridge<-glmnet(train_x,train_y, alpha=0,lambda=gamma_ridge*2, intercept=TRUE)
y_ridgehat<-predict.glmnet(ridge,newx=test_x,s=gamma_ridge)
#View(y_ridgehat)
ridge_err<-sqrt((1/12)*sum((test_y-y_ridgehat)^2))
ridge_err
