setwd("C:\\Jig12681")

# Read the data set
be<-read.csv("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 10 - Logistic Regression\\goodforu-class12.csv")
str(be)

# Removing first column 
be<-be[,-c(1)]

# Check for missing value
colSums(is.na(be))
# or
sapply(be,function(x)sum(is.na(x))) # No missing value found

# Convert all veriable as factor
#be<-lapply(be,factor)
#str(be)

# Subsetting data to solve first question, for product A X23, X2, x9, x16, x30, x38
a<-data.frame(be$X2,be$X9,be$X16,be$X23,be$X30)
str(a)
# Perciption of brand A rate 4 less
table(a$be.X23)
sum(2836,2218,3384,3722)/sum(2836,2218,3384,3722,5831,2874,1451,1040,416,342)
# By using pipeline 
library(dplyr)
x<-a%>%filter(a$be.X23<=4)%>%count()
x
y<-(x/count(a))*100
y
# Perciption of brand A rate 5 or more
p<-a%>%filter(a$be.X23>=5)%>%count()
p
# Creating tow factor for our terget variable X23 
a$goodbad<-ifelse(a$be.X23 >=5,1,0)

#a[]<-lapply(a,factor)
# removing original X23 variable
a<-a[,c(-4)]
str(a)
a$goodbad<-as.factor(a$goodbad)
str(a)
# deviding the data set into train and test
set.seed(200)
index<-sample(nrow(a),0.70*nrow(a),replace = F)
train<-a[index,]
test<-a[-index,]
# Question no 8 solution  
library(gains)
library(irr)
library(caret)
# Building the model
mod1<-glm(goodbad~.,data=a,family = "binomial")
summary(mod1)
exp(coefficients(mod1))/1+exp(coefficients(mod1))
exp(coefficients(mod1))
step(mod1)

str(train)
# Building the model 
mod2<-glm(goodbad~.,data=train,family = "binomial")
summary(mod2)
coefficients(mod2)
#step(mod2,direction = "both")

# Adding the predict value to test data set
pred<-predict(mod2,type="response",newdata = test)
head(pred)

# find out the proportion of the good customer in data calculating the class level
table(train$goodbad)/nrow(train)
table(test$goodbad)/nrow(test)
# As per the proportion of good customer we set the class level at 0.4957286
pred<-ifelse(pred>= 0.4957286,1,0)
# other method
pred1<-predict(mod2,type="response",newdata = test)
library(InformationValue)
outcut<-optimalCutoff(test$goodbad,pred1)[1]
outcut
# Validaation of the model
kappa2(data.frame(test$goodbad,pred))

# Confution matrix
confusionMatrix(pred,test$goodbad,positive = "1")


# ROCR Curve 
library(ROCR)
predicted<-mod2$fitted.values
pred1<-prediction(predicted,train$goodbad)
pref<-performance(pred1,"tpr","fpr")
plot(pref)

auc<-performance(pred1,"auc")
auc<-unlist(slot(auc,"y.values")) # Slot gives information about the individual slots
auc


# converting all variable as factor
# test<-lapply(test,factor)
test$goodbad<-as.numeric(test$goodbad)
# Gain chart
library(gains)
gains(test$goodbad,predict(mod2,type="response",newdata = test),groups=10)
test$prob<-predict(mod2,type = "response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

library(dplyr)
b<-a%>%filter(a$be.X2==1)%>%count()
b
c<-a%>%filter(a$be.X16==1)%>%count()
c
d<-a%>%filter(a$be.X9==1)%>%count()
d


#-------------
b<-data.frame(be$X2,be$X9,be$X16,be$X23,be$X30)
str(b)

b[]<-lapply(b,factor)
#b<-b[,c(-4)]
str(b)

# Model building
mod2<-glm(be.X23~.,data=b,family = "binomial")
summary(mod2)
exp(coefficients(mod1))
step(mod1)
boxplot(a$be.X30)
--