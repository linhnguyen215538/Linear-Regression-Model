#Q8
data<-read.csv("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Linear Reg\\Linear Regression HW\\HW5\\HW5\\pulsar_stars.csv\\pulsar_stars.csv",sep=',', header=TRUE)
head(data)
attach(data)
#head(target_class)
positives = data[target_class==1,]  
negatives=data[target_class==0,]
#head(positives)

positive_group <- rep(1:5,328)
negative_group <- rep(1:5,3252)
positive_group <- sample(positive_group,1639, replace= F)
negative_group <- sample(negative_group,16259, replace= F)
positives <- cbind(positives,positive_group)
negatives <- cbind(negatives,negative_group)

names(positives)[10]<-'Group'
names(negatives)[10]<-'Group'
data <- rbind(positives, negatives)

#Q9
trainingdata <- rbind(data[data[,10]==1,], data[data[,10]==2,],data[data[,10]==3,],data[data[,10]==4,])
group5<-data[data[,10]==5,]
View(group5[,1:8])
train_response <-data.matrix(trainingdata[,9])
train_X <-data.matrix(trainingdata[,1:8])

slr <- lm(train_response ~ train_X -1)
beta_hat <- matrix(slr$coefficients, ncol=1)
View(beta_hat)
p_tilda <- data.matrix(group5[,1:8])%*%beta_hat
#View(predict_group5)
#summary(predict_group5)
#Q10
logit<-function(x) {exp(x)/(1+exp(x))}

#Q11
#sapply(logit,function(logit)
  
  mod<-glm(train_response~train_X-1, family=binomial(link="logit"))
  newbeta_hat <- matrix(mod$coefficients, ncol=1)
  View(newbeta_hat)
  z <-data.matrix(group5[,1:8])%*% newbeta_hat
  p_hat <- exp(z)/(1+exp(z))
  summary(p_hat)

#Q12
  plot(p_tilda,p_hat,xlab = 'p_tilda', ylab= 'p_hat')
  
#Q13
  k <-length(p_tilda)
#View(k)
precision_logit <- rep(NA,k)
recall_logit <- rep(NA, k)
for (i in 1:k)
{
  Yhat <-rep(0,k)
  for (j in 1:k)
  {
    if (p_hat[j]>=p_hat[i])
      Yhat[j]<-1
  }

  precision_logit[i] <-sum(group5[,9]==1&Yhat==1)/sum(Yhat==1)
  recall_logit[i] <-sum(group5[,9]==1&Yhat==1)/sum(group5[,9]==1)
}
#View(Yhat)
#sum(Yhat==1)
#sum(group5[,9]==1)

precision_reg <- rep(NA,k)
recall_reg <- rep(NA, k)
for (i in 1:k)
{
  Yhat_reg <-rep(0,k)
  for (j in 1:k)
  {
    if (p_tilda[j]>=p_tilda[i])
      Yhat_reg[j]<-1
  }
  
  precision_reg[i] <-sum(group5[,9]==1&Yhat_reg==1)/sum(Yhat_reg==1)
  recall_reg[i] <-sum(group5[,9]==1&Yhat_reg==1)/sum(group5[,9]==1)
}
#View(Yhat)
#sum(Yhat==1)
#sum(group5[,9]==1)

plot(recall_logit,precision_logit, xlab= "Recall", ylab='Precision',col='blue',pch=20)
points(recall_reg,precision_reg, col='red',pch=20)
legend(0,0.4,c('Regression','Logistic'),c('red','blue'), cex=0.6)

# group1 <-data[,10][data[,10]==1]
# length(group1)
# group2 <-data[,10][data[,10]==2]
# length(group2)
# group3 <-data[,10][data[,10]==3]
# length(group3)
# group4 <-data[,10][data[,10]==4]
# length(group4)
# group5 <-data[,10][data[,10]==5]
# length(group5)

# for (i in 1:17898)
# {
#   if(target_class[i]='0')
#   {
#     positives[i] = data[i]
#     i=i+1
#   }
#   else
#   {
#     negatives[i] = data[i]
#   }
# }

