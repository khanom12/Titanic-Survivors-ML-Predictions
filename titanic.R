# Omar Khan
# Feb-21-2018
# First ML Code

# set working direcoty
setwd("~/kaggle/Titanic")

# import data files
train <- read.csv("~/kaggle/Titanic/train.csv")
test <- read.csv("~/kaggle/Titanic/test.csv")

# view data files
view(train)
view(test)

# view the data frame for train.csv
str(train)

# view count of 'survived' column of train.csv
table(train$Survived)

# view proportion of 'survived'
prop.table(table(train$Survived))

# set 'everyone dies' as heuristic for test.csv
test$Survived <- rep(0, 418)

# extract prediction results and write to new file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# view summary of 'Sex' column
summary(train$Sex)

# view comparison of 'Sex' with 'Survived'
prop.table(table(train$Sex, train$Survived),1)

# reset survived column with 0 and add 1 for females
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

# extract prediction results and write to new file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allmenperish.csv", row.names = FALSE)

# summary of age variable
summary(train$Age)

# create variable 'Child' to determine if passenger < 18 years old
train$Child <- 0
train$Child[train$Age < 18] <- 1

# view number of survivors from Child and Sex
aggregate(Survived ~ Child + Sex, data = train, FUN = length)

# view proportion of survivors
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x) / length(x)})

# create variable 'Fare2' 
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# view proportions
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {sum(x) / length(x)})

# set female of class 3 to perish
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

# extract prediction results and write to new file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "heuristic3.csv", row.names = FALSE)

# add library for decision trees
library(rpart)

# create the tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# examine tree
plot(fit)
text(fit)

# add libraries for better data visualization
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# visualize a fancier tree
fancyRpartPlot(fit)

# extract prediction results and write to new file
prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "decisionTree.csv", row.names = FALSE)

# override tree controls
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 2, cp = 0))
