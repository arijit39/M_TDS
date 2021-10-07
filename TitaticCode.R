library(dplyr)
library(tidyverse)
library(ggplot2)
library(plyr)

train = read.csv("E:/Kaggle/train.csv")
summary(train)
dim(train)
names <- c('Embarked' ,'Sex','Survived')
train[,names] <- lapply(train[,names] , factor)
summary(train)
train <- train %>% select(c(-Last.Name,-First.Name,-Ticket,-Cabin))


train$Age[is.na(train$Age)]<-mean(train$Age,na.rm=TRUE)
train$Fare[train$Fare == 0]<- NA
train$Fare[is.na(train$Fare)]<-mean(train$Fare,na.rm=TRUE)

train$Embarked[train$Embarked != "C" & train$Embarked != "S" & train$Embarked != "Q"] <- NA
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
train$Embarked[is.na(train$Embarked)] <- "S"

attach(train)
names(train)

glm.fit=glm(Survived~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = train, family = binomial)
summary (glm.fit)
summary(glm.fit)$coef
glm.probs = predict(glm.fit, type = "response")
glm.probs[1:50]
contrasts(train$Survived)
dim(train)
glm.pred=rep(0,891)
view(glm.pred)
glm.pred[glm.probs>.5]=1
view(glm.pred)
table(glm.pred, train$Survived)

test = read.csv("E:/Kaggle/test.csv")
view(test)
summary(test)
str(test)

test$Embarked <- as.factor(test$Embarked)
test$Sex <- as.factor(test$Sex)
glm.probsnew <- predict(glm.fit, test, type="response")
glm.probsnew[1:50]
view(glm.probsnew)
view(glm.probs)

dim(test)
glm.prednew=rep(0,411)
glm.prednew[glm.probsnew>.5]=1
view(glm.prednew)
PassengerId <-seq(892,1309)
Testresult<- data.frame(PassengerId, glm.prednew)
view(Testresult)
names(Testresult)[names(Testresult) == "glm.prednew"] <- "Survived"
view(Testresult)
write.csv(Testresult,"E:/Kaggle/Testresult.csv", row.names = FALSE)