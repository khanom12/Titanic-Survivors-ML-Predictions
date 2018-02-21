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

# create variable 'isChild' to determine if passenger < 18 years old
train$Child <- 0
train$Child[train$Age < 18] <- 1

