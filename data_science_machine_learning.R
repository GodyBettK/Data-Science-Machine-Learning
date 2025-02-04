# install.packages("nycflights13")
# install.packages("plotly")
#install.packages("ggthemes")
#install.packages("MASS")
# install.packages("glmnet")
# install.packages("car")
# install.packages("caret")

setwd("D:/OTHER FILES/STTs/DATA SCIENCE/JOSE PORTILLA/Data_Science_and_Machine_Learning_Bootcamp_with_R/all_datasets")
getwd()




##Advanced Programming in R
#seq()
v1 <- seq(1,100, by = 4)
#sort()
sort(v1, decreasing = FALSE)library(ggplot2)
library(plotly)
library(ggthemes)
library(dplyr)
library(caTools)
library(nycflights13)
library(corrgram)
library(corrplot)
library(caTools)
library(MASS)
library(car)
library(caret)
library(glmnet)
library(Amelia)
library(rpart)
library(rpart.plot)
library(ISRL2)
#rev()
rev(v1)
#str()
str(v1)
#append()
v2 <- seq(200, 250, by = 10)
v3 <- append(v1, v2)



pl <- ggplot(mtcars, aes(mpg,wt)) + geom_point()
print(pl)

plt <- ggplotly(pl)
print(plt)

#import student performance dataset
df <- read.csv('student-mat.csv', sep = ';')
head(df)
summary(df)

#cleaning the dataset
any(is.na(df))  #check for null values
str(df)

#select numeric columns only
num.cols <- sapply(df,is.numeric)
print(num.cols)
#filter the numeric columns
cor.data <- cor(df[,num.cols])
print(cor.data)

#install additional packages for correlation
# install.packages("corrgram")
# install.packages("corrplot")
library(corrgram)
library(corrplot)

#EDA
print(corrplot(cor.data, method = 'color'))

corrgram(df, order = TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

ggplot(df, aes(x = G3)) + geom_histogram(bins = 20, alpha = 1, fill ='orange')

#BUILD THE MODEL
#split data into Train and Test set
df <- read.csv('student-mat.csv', sep = ';')
# install.packages("caTools")
library(caTools)

#set a seed
set.seed(101)

#split up sample
sample <- sample.split(df$G3, SplitRatio = 0.7)

#Train model using 70% of data
train <- subset(df, sample == TRUE)

#30% of dataset will be test data.
test <- subset(df, sample == FALSE)

###TRAIN & BUILD THE MODEL
##we intend to predict the outcome of G3
model <- lm(G3 ~ ., data = train)
#Run Model
print(model)
#Interpret the model
print(summary(model))

#FEATURE SELECTION STEPWISE

# Install and load MASS
# install.packages("MASS")
library(MASS)

# Fit a full model
# model <- lm(G3 ~ ., data = train)

# Perform stepwise selection
stepwise_model <- stepAIC(model, direction = "both")
summary(stepwise_model)



#print residuals
res <- as.data.frame(residuals(model)) #get the residuals and structure as data frame
class(res)
# res <- as.data.frame(res)
head(res)
# ggplot(res, aes(res)) + geom_histogram(fill = 'blue', alpha = 0.8)

##PART 2 LINEAR REGRESSION
#Run Model


#Make Predictions
G3.predictions <- predict(model, test)

results <- cbind(G3.predictions, test$G3)
colnames(results) <- c('predicted', 'actual')
results <- as.data.frame(results)
print(head(results))

#TAKE CARE OF NEGATIVE VALUES
to_zero <- function(x){
  if(x < 0)
    {
    return(0)
    }else{
    return(x)
  }
  }

#APPY ZERO FUNCTION
results$predicted <- sapply(results$predicted, to_zero)

#MEAN SQUARED ERROR
mse <- mean((results$actual - results$predicted)^2)
print("MSE")
print(mse)
#RMSE
rmse <- mse^0.5
print("Root MSE")
print(rmse)

#CALCULATE R2- CORRELATION COEFFICIENT
SSE <- sum((results$predicted - results$actual)^2)
SST <- sum((mean(df$G3) - results$actual)^2)
R2 <- 1 - (SSE/SST)
print(R2)

###21.MACHINE LEARNING PROJECT
# 
# bike <- read.csv("bikeshare.csv")
# head(bike)
# str(bike)
#  #EDA
# library(ggplot2)
# ggplot(bike, aes(x = temp, y = count)) + geom_point(alpha = 0.3, aes(color = temp)) + theme_bw()
# 
# #convert datetime column to POSIXct FORMAT
# bike$datetime <- as.POSIXct(bike$datetime)
# bike$datetime
# 
# pl <- ggplot(bike, aes(datetime, count)) + geom_point(aes(color = temp), alpha=0.5) + theme_bw()
# pl + scale_color_continuous(low = '#5DD8CE', high = '#FF6E2E') + theme_bw()



#################################################################################################
#22. LOGISTIC REGRESSION

df.train <- read.csv('titanic_train.csv')
head(df.train)
str(df.train)

# install.packages("Amelia")
# library(Amelia)

#check for mising data
#missmap(df.train, main = 'Missing Map', col = c('yellow', 'black'), legend = FALSE)

ggplot(df.train, aes(Survived)) + geom_bar()
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))
ggplot(df.train, aes(Sex)) + geom_bar(aes(fill = factor(Sex)))
ggplot(df.train, aes(Age)) + geom_histogram(bins = 20, fill = 'blue', alpha = 0.6)

ggplot(df.train, aes(SibSp)) + geom_bar()
ggplot(df.train, aes(Fare)) + geom_histogram(fill = 'darkgreen', color = 'black', alpha = 0.5)

#boxplot
pl <- ggplot(df.train, aes(Pclass, Age)) + geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.8)) + scale_y_continuous(breaks = seq(min(0), max(90), by = 2)) + theme_bw()
pl

#IMPUTATION OF AGE usinf average age of passengers by CLASS
impute_age <- function(age, class){
  out <- age
  for (i in 1:length(age)) {
    if(is.na(age[i])){
      if (class[i] == 1) {
        out[i] <- 37
      }else if (class[i] == 2) {
        out[i] <- 29
      }else {
        out[i] <- 24
      }
    }else {
      out[i] <- age[i]
    }
  }
  return(out)
}

###
fixed.ages <- impute_age(df.train$Age, df.train$Pclass)
df.train$Age <- fixed.ages
missmap(df.train, main = 'Imputation Check', col = c('yellow', 'black'), legend = FALSE)


###
str(df.train)
#Exclude the PassengerId, Name, Ticket, Cabin columns as we will not be using them
df.train <- subset( df.train, select = -c(PassengerId,Name, Ticket, Cabin))
head(df.train)

#Convert Survived, Pclass, SibSp & Parch to Factor
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$SibSp <- factor(df.train$SibSp)
df.train$Parch <- factor(df.train$Parch)

str(df.train)

#Train the Model
log.model <- glm(Survived ~ ., family = binomial(link = "logit"), data = df.train)
summary(log.model)

#predit
library(caTools)
set.seed(101)
split <- sample.split(df.train$Survived, SplitRatio = 0.7)
final.train <- subset(df.train, split == TRUE)
final.test <- subset(df.train, split == FALSE)
final.log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = final.train)
summary(final.log.model)

#Test the final.log.model using final.test dataset
fitted.probabilities <- predict(final.log.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)
missClassError <- mean(fitted.results != final.test$Survived)
accuracy <- 1 - missClassError
print(accuracy)

#CREATE CONFUSION MATRIX
table(final.test$Survived, fitted.probabilities>0.5)

###################################################################################################
#24. K-NEAREST NEIGHBORS (KNN) - for Classification

install.packages("ISLR")
library(ISLR)

#load dataset
str(Caravan)
#check for missing values
any(is.na(Caravan))

#check variance for column 1
var(Caravan[, 1])
var(Caravan[, 2]) 

#create purchase object from the last column 'Purchase'
purchase <- Caravan[, 86]

standardized.Caravan <- scale(Caravan[, -86])
str(standardized.Caravan)

var(standardized.Caravan[, 1])
var(standardized.Caravan[, 2])

###Train Test Split - we can use CaTools, highly recomended to use
test.index <- 1:1000
test.data <- standardized.Caravan[test.index, ]
test.purchase <- purchase[test.index]

#Train 
train.data <- standardized.Caravan[-test.index, ]
train.purchase <- purchase[-test.index]

##########################33
###KNN Model
library(class)
set.seed(101)
predicted.purchase <- knn(train.data, test.data, train.purchase, k = 3)
head(predicted.purchase)

missclass.error <- mean(test.purchase != predicted.purchase)
print(missclass.error)

#####CHOOSING A K-VALUE using the K-Elbow Method

predicted.purchase <- NULL
error.rate <- NULL

for (i in 1:20) {
  set.seed(101)
  predicted.purchase <- knn(train.data, test.data, train.purchase, k = i)
  error.rate[i] <- mean(test.purchase != predicted.purchase) 
}

####VISUALIZE THE K-ELBOW METHOD
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
print(error.df)

ggplot(error.df, aes(x = k.values, y = error.rate)) + geom_point() + geom_line(lyt = 'dotted', color = 'red')


##############################################################################################################
###DECISION TREES AND RANDOM FORESTS
##Decision Tree
# install.packages('rpart')
library(rpart)
str(kyphosis)
head(kyphosis)  #kyphosis dataset

tree <- rpart(Kyphosis ~ ., method = 'class', data = kyphosis)
printcp(tree)
plot(tree, uniform = TRUE, main = 'Kyphosis Tree')
text(tree, use.n = TRUE, all = TRUE)

##Better package for Plotting
#install.packages("rpart.plot")
library(rpart.plot)
prp(tree)

##Random Forest
#install.packages("randomForest")
library(randomForest)
rf.model <- randomForest(Kyphosis ~ ., data = kyphosis)
print(rf.model)
rf.model$ntree
rf.model$confusion
rf.model$err.rate
rf.model$votes

###############################################################################
###SUPPORT VECTOR MACHINES####
##library(ISLR3) head(iris3)
library(ISLR)
print(head(iris))

# install.packages("e1071")
library(e1071)
help("svm")
model <- svm(Species ~ ., data = iris)
summary(model)

pred.values <- predict(model, iris[1:4])

table(pred.values, iris[,5])

#######TUNING SVM
tune.results <- tune(svm, train.x = iris[1:4], train.y = iris[,5], 
                     kernel = 'radial', ranges = list(cost = c(0.1, 1, 10), gamma = c(0.5, 1, 2)))

summary(model)

tuned.svm <- svm(Species ~ ., kernel = 'radial', data = iris, cost = 1.5, gamma = 0.1)
summary(tuned.svm)


#################################################################################################################33
########### K-MEANS CLUSTERING
install.packages("cluster")
library(ISLR)
head(iris)

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + geom_point(size = 4) + theme_gdocs()

set.seed(101)

irisCluster <- kmeans(iris[, 1:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)

library(cluster)

clusplot(iris, irisCluster$cluster, color = T, shade = T, labels = 0, lines = 0 )

help("clusplot")
help("kmeans")


###############################################################################
####### NATURAL LANGUAGE PROCESSING (NLP)

install.packages("tm")
install.packages("jsonlite")
install.packages("twitteR")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("e1017")
install.packages("class")

#https://developer.x.com/en/apps

library(twitteR)
library(wordcloud)
library(tm)
library(RColorBrewer)

####CONNECT TO TWITTER

ckey <- c("")  ####Insert  your own authentication tokens & keys
skey <- c("")
token <- c("")
sectoken <- c("")

setup_twitter_oauth(ckey, skey, token, sectoken)

######SEARCH TWITTER

soccer.tweets <- searchTwitter('arsenal', n = 1000, lang = 'en')


























