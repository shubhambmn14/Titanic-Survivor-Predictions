#Since this is a binary classification problem, I have first used logistic regression to predict if a person will survive or not.
#Then i have used Random forest classification to predict the survival chances



TITANIC – R

install.packages('xgboost')
train <- read.csv("train.csv") #reading the training data file
str(train)
sum(is.na(train)) #find out the number of missing values
summary(train)
table(train$Survived)
prop.table(table(train$Survived))
aggregate(Survived ~ Sex, data = train, FUN = length)
aggregate(Survived ~ Sex, data = train, FUN = sum)
aggregate(Survived ~ Sex, data = train, FUN = function(x){sum(x)/length(x)})
test <- read.csv("test.csv")
combi <- rbind(train, test)
summary(combi$Age) #Shows 263 NA's
#converting the Name to character for manipultatio
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][2]})
#removing the extra space in the Title by the help of sub function
combi$Title <- sub(' ','',combi$Title)
combi$Title <- factor(combi$Title)
str(combi$Title)
table(combi$Title)
#We can see that there are lot of titles which are having only 1 or 2 members.
#Based on some research, we know that Mme = Missus (Mrs) 
#Mlle = Miss (no abbreviation, unless you use: Ms).
#converting the Title back to character for easy manipulation.
combi$Title <- as.character(combi$Title)
combi$Title[combi$Title == 'Mlle'] <- 'Ms'
combi$Title[combi$Title == 'Mme'] <- 'Mrs'
combi$Title <- factor(combi$Title)
table(combi$Title)
combi$Title[combi$Title == 'Ms'] <- 'Miss'
str(combi$Title)
combi[combi$Title == 'Major', 2]
train <- combi[1:891,]
train[train$Title == 'Dona',3]
train[train$Title == 'the Countess', 3]
newclass <- c('Don', 'Rev', "Col", "Dr", "Capt", "Major", "Sir", "Dona", "Jonkheer", "Lady", "the Countess")
combi$Title[combi$Title %in% newclass] <- 'Rareclass'
table(combi$Title)
combi$Title
sub("",'',newclass)
newclass
str(combi)
combi$Title <- as.character(combi$Title)
combi$Title[combi$Title == 'Col'] <- 'Rare'
combi$Title <- factor(combi$Title)
table(combi$Sex, combi$Title)
table(train$Title)
write.csv(combi, file="combined.csv", row.names = FALSE)
#to replace the missing Age variable
aggregate(Age ~ Title, data = combi, FUN = function(x){mean(x, na.rm = TRUE)})
aggregate(Age ~ Title, data = combi, FUN = function(x){median(x, na.rm = TRUE)})
str(combi$Age)

combi$Age[is.na(combi$Age) & combi$Title == 'Master'] <- 4
combi$Age[is.na(combi$Age) & combi$Title == 'Miss'] <- 22
combi$Age[is.na(combi$Age) & combi$Title == 'Mr'] <- 29
combi$Age[is.na(combi$Age) & combi$Title == 'Mrs'] <- 35
combi$Age[is.na(combi$Age) & combi$Title == 'Rareclass'] <- 47.5
str(combi$Age)
summary(combi$Age)
rm("iris", "iris_virginica")
table(combi$Embarked)
combi[combi$Embarked == '',]
#Analysis based on tickets. lot of people had a same ticket number!
abc <- data.frame(table(combi$Ticket))
abc[abc$Freq == 6,]
abc[abc$Freq == 2,]
table(train$Survived)
prop.table(table(train$Survived))
table(combi$Title, combi$Survived)
new_train <- combi[1:891,]
aggregate(Survived ~ Title + Pclass, data = new_train, FUN = function(x){sum(x)/length(x)})
combi$Agebracket <- NA
combi$Agebracket[combi$Age <= 5] <- 'smallchild'
combi$Agebracket[combi$Age > 5 & combi$Age <=10] <- 'child'
combi$Agebracket[combi$Age > 10 & combi$Age <=20] <- 'teenager'
combi$Agebracket[combi$Age > 20 & combi$Age <=50] <- 'Adult'
combi$Agebracket[combi$Age > 50 & combi$Age <=80] <- 'Old'
combi$Agebracket <- factor(combi$Agebracket)
aggregate(Survived ~ Agebracket, data = new_train, FUN = function(x){ sum(x)/length(x)})
aggregate(Survived ~ Pclass + Agebracket, data = new_train, FUN = sum)
aggregate(Survived ~ Pclass + Agebracket, data = new_train, FUN = length)
aggregate(Survived ~ Pclass + Agebracket, data = new_train, FUN = function(x){ sum(x)/length(x)})

write.csv(combi, file = "combined.csv", row.names = FALSE)

combi$Familysize <- combi$SibSp + combi$Parch + 1
summary(combi$Familysize)
#writing back the test file as well.
test <- combi[892:1309, ]
write.csv(test, file = "newtest.csv", row.names = FALSE)
library(psych)
summary(combi$Fare)
#exploring the Embarked
combi$Embarked <- factor(combi$Embarked)
summary(combi$Embarked)
aggregate(Pclass ~ Embarked, data = new_train, FUN = length)
combi$Embarked <- as.character(combi$Embarked)
combi$Embarked[combi$Embarked == ' '] <- 'S'
#familysize VS survival
combi$Familysize <- factor(combi$Familysize)
aggregate(Survived ~ Familysize, data = new_train, FUN = function(x){sum(x)/length(x)})
str(test$Familysize)
write.csv(combi, file = "combined.csv", row.names = FALSE)
str(new_train$Cabin)
#analyze the Fare
aggregate(Fare ~ Pclass, data = combi, FUN = median)
combi$PassengerId[is.na(combi$Fare)]
#Passenger ID 1044 has a NA. Substituting it with median of the Fare for the Pclass = 3
combi$Fare[1044] <- 8.05

##############Running the logistic regression#################
#converting the Fare and Age variables to log(Fare) and log(Age)
hist(combi$Age, breaks = 40) #Its not a Gaussian distribution
hist(combi$Fare, breaks = 40)
summary(combi$Age)
combi$Age <- log(1+combi$Age)
combi$Fare <- log(1+combi$Fare)
train <- combi[1:891,]
test <- combi[892:1309,]
set.seed(777)
####Partitioning the date*********
sample <- floor(0.90*nrow(train))
train_ind <- sample(seq_len(nrow(train)), size = sample)
train_final <- train[train_ind,]
test_final <- train[-train_ind,]
###########fitting the model##################
train_final <- train_final[, -c(1,4,9,11,13,14,15)]
test_final <- test_final[, -c(1,4,9,11,13,14,15)]

model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = train_final)

summary(model)
#Since Embarked is not significant, removing that
train_final <- train_final[,-8]
test_final <- test_final[,-8]
fitted.results <- predict(model,newdata= subset(test, select = c(2,3,5,6,7,8,10)),type='response')
fitted.results <- ifelse(fitted.results > 0.6,1,0)
test$Survived <- fitted.results
testwrite <- test[, c(1,2)]
write.csv(testwrite, file = "logistic4.csv", row.names = FALSE)
misClasificError <- mean(fitted.results != test_final$Survived)
print(paste('Accuracy',1-misClasificError))
setwd("D://Sunstone//kaggle//Titanic")

set.seed(777)
####Partitioning the date*********
sample <- floor(0.95*nrow(train))
train_ind <- sample(seq_len(nrow(train)), size = sample)
train_final <- train[train_ind,]
test_final <- train[-train_ind,]
model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = train_final)

summary(model)

#Not sure how much the log function worked
str(combi$Age)
summary(combi$Age)
hist(train$Age, breaks = 40)
hist(test$Age, breaks = 40)
rm(test_data)


###############********Random Forest Implementation*********################
library(randomForest)
combi <- read.csv("combined.csv")
combi_RF <- combi[, -c(1,4,6,7,8,9,11,12,14)]
train <- combi_RF[1:891, ]
test <- combi_RF[892:1309,]
fit <- randomForest(as.factor(Survived) ~., train, importance = TRUE, ntree = 2000) 
varImpPlot(fit)
summary(fit)
#Predict output
predicted <- predict(fit,test, type = "class")
Acutaltest <- combi[892:1309, c(1,2)]
Acutaltest$Survived <- predicted
write.csv(Acutaltest, file = "Randomforest8.csv", row.names = FALSE)

combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
train <- combi[1:891,]
sum(is.na(train))
cor.test(train$Age, train$Fare)
library(carrplot)
