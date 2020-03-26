x <- c(0,2,4,6,8,10)
y <- c(439, 439.12,439.21, 439.31, 439.4, 439.5)

mean(x)

randomx= seq(from =0, to =10, by=0.1)
View(randomx)
#head(randomx)
#length(randomx)
numerator= (randomx-mean(randomx))^2
denominator = sum((x-mean(x))^2)
sigma_sq= (sum((y-439.01095-0.04914*x)^2))/4
standard_err= sqrt(sigma_sq*(1/length(randomx)+numerator/denominator))
View(standard_err)

plot(x,y, xlab = "Weight (kg)", ylab = "Length (cm)")
slr<-lm(y~x)
randomy1=randomx*slr$coefficients[2]+slr$coefficients[1]+2*standard_err
randomy2=randomx*slr$coefficients[2]+slr$coefficients[1]-2*standard_err
abline(a= slr$coefficients[1], b= slr$coefficients[2], col = "red")
lines(randomx,randomy1, type="l", col="blue")
lines(randomx,randomy2, type="l", col="blue")

legend(0.3,439.45, legend=c("SLR line", "Confidence interval"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

mean(y)
adj <-c("fruit", "aromas",
        "acidity", "finish", "tannins", "cherry", "black", "ripe", "red", "rich", "fresh", "oak", "spice",
        "dry", "berry", "full", "plum", "apple", "soft", "sweet")
sampled_feature <- sample(adj,10)

#load("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Linear Reg\\Linear Regression HW\\HW3\\hw3_filtered_wine_reviews.csv")
data <- read.table("C:\\Users\\Elise Nguyen\\OneDrive\\Documents\\Linear Reg\\Linear Regression HW\\HW3\\hw3_filtered_wine_reviews.csv", header=T,sep=",")

description <- tolower(data$description)
description <- gsub('[^a-zA-Z]+', ' ',description)
description <- strsplit(description, " ")

feature_count <- rep(NA, length(description))
for (i in 1:length(description))
{
  feature_count[i] <- sum(description[[i]]%in%sampled_feature)
}
head(feature_count)

lm(points~feature_count)

mean(feature_count)
numer1= (3-mean(feature_count))^2
denom1=sum((feature_count-mean(feature_count))^2)
fraction1=numer1/denom1
yhat= 0.2922*feature_count+87.8668
sigma_sq2=sum((data$points-yhat)^2)/129969
variance = sigma_sq2*(129972/129971+fraction1)
