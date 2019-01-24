#for my comp
setwd("/Users/louisjochems/Documents/HW1")
#for R studio 
setwd("~/")

#Question 1
math<-read.csv("math_scores.csv")
x=math$LSD_concentration
hist(x)

modelq1=lm(math$MATH_score~math$LSD_concentration)
coef(modelq1)
confint(modelq1)

plot(math$MATH_score~math$LSD_concentration); curve(89.123874+ -9.009466*x, add=T)

#way from class
a<-89.123874
b<- -9.009466
y=math$MATH_score
y_hat=a+b*math$LSD_concentration

RSS=sum((math$MATH_score-Yhat)^2)
SS=sum((math$MATH_score-mean(math$MATH_score))^2)
R2=(SS-RSS)/SS

#function for hw
r2<-function(y_hat,y) {
  RSS<-sum((((y_hat))-(y))^2)
  TSS<-sum(((y)-(mean(y)))^2)
  return(1-RSS/TSS)}

r2(runif(100),runif(100))

r2(c(1:10),jitter(c(1:10)))

rmse=function(y_hat,y) {return(sqrt(mean((y-y_hat)^2)))}
   



#Question 2 
miracle <- read.csv("miracle_food.csv")
x=miracle$pomegranate
hist(x)

modelq2=lm(miracle$Weight_loss~miracle$pomegranate)
coef(modelq2)
confint(modelq2)

plot(miracle$Weight_loss~miracle$pomegranate); curve(-0.1789802+ -0.5251053*x, add=T)

#way from class
a<- -0.1789802
b<- -0.5251053
y=miracle$Weight_loss
y_hat=a+b*miracle$pomegranate

RSS=sum((miracle$Weight_loss-Yhat)^2)
SS=sum((miracle$Weight_loss-mean(miracle$Weight_loss))^2)
R2=(SS-RSS)/SS

#function in hw
r2<-function(y_hat,y) {
  RSS<-sum((((y_hat))-(y))^2)
  TSS<-sum(((y)-(mean(y)))^2)
  return(1-RSS/TSS)}

rmse=function(y_hat,y) { 
  sqrt(mean((y-y_hat)^2, na.rm=T))}




#Question 3
#math
a<-89.123874
b<- -9.009466
y=math$MATH_score
y_hat=a+b*math$LSD_concentration
n_math=nrow(math)
mae=function(y_hat,y) {
  ABS<-(sum(abs((y)-(y_hat))))
  n <- n_math
  return(ABS/n)
  }

#miracle
a<- -0.1789802
b<- -0.5251053
y=miracle$Weight_loss
y_hat=a+b*miracle$pomegranate
n_miracle=nrow(miracle)
mae=function(y_hat,y) {
  ABS<-(sum(abs((y)-(y_hat))))
  n <- n_miracle
  return(ABS/n)
}




#Question 4 
temperature <- runif(100, min=10,max=45)
slope <- 3
intercept <- 5
sd <- 3
x=temperature; hist(x)
Germination_rate <- rnorm(mean=intercept+slope*temperature, n=100, sd=3)
plot(Germination_rate~temperature); curve(5+ 3*x,add=T)
modelq4=lm(Germination_rate~temperature)
coef(modelq4)
summary(modelq4)



#question 5 
predictor <- runif(1000,0,100)
response <- rnorm(1000,0,predictor)
plot(response~predictor)
