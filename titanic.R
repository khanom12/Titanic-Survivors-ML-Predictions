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

# perform feature extraction
#----------------------------

# import data files
train <- read.csv("~/kaggle/Titanic/train.csv")
test <- read.csv("~/kaggle/Titanic/test.csv")

# bind the train and test datasets
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)

# split name attribute by comma using regex
strsplit(combi$Name[1], split = '[,.]')[[1]][2]
combi$Title <-strsplit(combi$Name, split = '[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

# combime mme and mlle and other inneffective titles as they seen to have no effect on result
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'


# change string back to factor type
combi$Title <- factor(combi$Title)

# determine family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# extract surname
combi$Surname <- sapply(combi$Name, FUN = function(x) {strsplit(x, split = '[,.]')[[1]][1]})

# create family id
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")

# family needs to have more that 2 members otherwise considered Small
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

# split train and test sets
train <- combi[1:891,]
test <- combi[892:1309,]

# add libraries for better data visualization
library(rattle)
library(rpart)
library(rpart.plot)
library(RColorBrewer)

# create tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data=  train, method = "class")

# extract prediction results and write to new file
prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submit, file = "dTreeFeatures.csv", row.names = FALSE)

# implement random forests
summary(combi$Age)

# use ANOVA
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
summary(combi)

# replace and encode empty values
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

# reduce familyIDs
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
#combi$Sex <- as.factor(combi$Sex)

# Split back into test and train sets
train <- combi[1:891,]
test <- combi[892:1309,]

# include random forest library
library(randomForest)

# set random seed
set.seed(415)

# create model
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data=train, importance=TRUE, ntree=2000)

# determine variable importance
varImpPlot(fit)

# extract prediction results and write to new file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "randomForestPrediction.csv", row.names = FALSE)

# include party library
library(party)

# set random seed
set.seed(415)

# create model
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, controls=cforest_unbiased(ntree=2000, mtry=3)) 

# extract prediction results and write to new file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "forestFinale.csv", row.names = FALSE)

