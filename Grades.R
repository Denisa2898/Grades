#PURPOSE: to see which factor influence more the student final grade 

library(readxl)
library(dplyr)
library(psych) 
library(Hmisc) #for stat.desc
library(pastecs) #for stat.desc
library(caret) #for classification and regression training
grades <- read_excel("/Users/denisa1417/Desktop/Lucrare de disertatie/student_grades_prediction2.xlsx")
View(grades)

grades$finalavgrades <- round(grades$finalavgrades, 0) #round the final grades

sum(is.na(grades)) #count the na if there is na -> 0 na (part of the dataset cleaning process)

var_numerice <- grades[c(2, 11, 12, 13, 28, 32)] #table with numeric variables: age, travel time, study time, failures, absence, finalavgrades
sapply(var_numerice, class) #check if the variables are part of numeric class 
stat.desc(var_numerice) #descriptive statistics
summary(var_numerice) #check the min, max, median, mean, Q1, Q3

####################### ANALYZE ##############################

#check the number of values
table(grades$sex) #F: 5752  #M: 5236
table(grades$age) #15   16   17   18   19   20   21   22 
                  #2272 2912 2720 2272  672   84   28   28 

#identifying and eliminating extreme values (part of the dataset cleaning process)
#age
boxplot(grades$age, main = "Age")
boxplot.stats(grades$age)$out
grades$age <- ifelse(grades$age >=  20, NA, grades$age) #the age >= 20 was extreme values, so I eliminated them

#travel time
boxplot(grades$traveltime, main = "Travel Time")
boxplot.stats(grades$traveltime)$out
grades$traveltime <- ifelse(grades$traveltime >= 26, NA, grades$traveltime) #I eliminated the values bigger or equal with 26 minutes
                                                                            
#study time
boxplot(grades$studytime, main = "Study Time")
boxplot.stats(grades$studytime)$out
grades$studytime <- ifelse(grades$studytime >= 500, NA, grades$studytime) #I eliminated the values bigger or equal with 500 minutes

#failures
boxplot(grades$failures, main = "Number of failures")
boxplot.stats(grades$failures)$out
table(grades$failures) # 0 fail - 8688; 1 fail- 1400; 2 failures - 476, 3 failures - 424 (there are too many extreme values for records with 1,2,3 failures)
grades$failures <- ifelse(grades$failures >= 1, NA, grades$failures) #so I decided to keep only the records with 0 fail in order not to affect the final analysis

#absences
boxplot(grades$absences, main = "Number of absences")
boxplot.stats(grades$absences)$out
grades$absences <- ifelse(grades$absences >= 16, NA, grades$absences) #I eliminated the values bigger or equal with 16 absences

#First grade
boxplot(grades$G1, main = "First grade")
boxplot.stats(grades$G1)$out
grades$G1 <- ifelse(grades$G1 >= 16, NA, grades$G1) #I eliminated the values bigger or equal with 16 score

#Second grade
boxplot(grades$G2, main = "Second grade")
boxplot.stats(grades$G2)$out
grades$G2 <- ifelse(grades$G2 <= 0, NA, grades$G2) #I eliminated the 0 values

#Third grade
boxplot(grades$G3, main = "Third grade")
boxplot.stats(grades$G3)$out
grades$G3 <- ifelse(grades$G3 <= 0, NA, grades$G3) #I eliminated the 0 values

#Final grade
boxplot(grades$finalavgrades, main = "Final grade")
boxplot.stats(grades$finalavgrades)$out
grades$finalavgrades <- ifelse(grades$finalavgrades >= 16, NA, grades$finalavgrades) #I eliminated the values bigger or equal with 16 score

#check for na (part of the dataset cleaning process)
is.na(grades)
sum(is.na(grades)) #5432
grades <- na.omit(grades) #I eliminated the na values
View(grades)

#save new dataset
write_xlsx(grades, path = "/Users/denisa1417/Desktop/Lucrare de disertatie/grades_new123.xlsx", col_names = TRUE)

nrow(grades) #7656
ncol(grades) #32
min(grades$studytime) #20
max(grades$studytime) #1000

min(grades$traveltime) #5
max(grades$traveltime) #80

#I used the group_by() and summarise() functions to calculate percentages

#address
table(grades$address) #R:1764,  U:5892
result_address <- grades %>%
  group_by(address) %>%
  summarise(Percentage = n() / nrow(grades) * 100)
result_address #R:23%, U:77%

#mother education
table(grades$Medu) #0 - 56, 1 - 1156, 2 - 2156, 3 - 1736, 4 - 2552
result_Medu <- grades %>%
  group_by(Medu) %>%
  summarise(Percentage = n() / nrow(grades) * 100)
result_Medu #0 - 0.73%, 1 - 15,1%,2 - 28,2%, 3 - 22,7%, 4 - 33,3%

#father education
table(grades$Fedu) #0 - 28, 1- 1576, 2 - 2212,  3 - 2100, 4 - 1740
result_Fedu <- grades %>%
  group_by(Fedu) %>%
  summarise(Percentage = n() / nrow(grades) * 100)
result_Fedu #0 - 0.36%, 1 - 20,6%, 2 - 28,9%, 3 - 27,4%, 4 - 22,7%

obj1 <- ggplot(data = grades,aes(x = Mjob, y = finalavgrades, fill = sex)) + geom_boxplot()#finalavgrades of students of both sexes depending on the mother's job
obj1 #the both female and male students whose mother works in health have the highest finalavgrades 

#############  dataset without the na

g <- read_excel("/Users/denisa1417/Desktop/Lucrare de disertatie/grades_new123.xlsx")
View(g)
sum(is.na(g)) #0 na

obj2 <- ggplot(data = g,aes(x = Fjob, y = finalavgrades, fill = sex)) + geom_boxplot()
obj2 #the female students whose father works in health have the highest finalavgrades
     #the male students whose father works as teacher have the highest finalavgrades

obj3 <- ggplot(data = g, aes(x = age, y = finalavgrades, col = sex, shape = sex))+geom_point()+geom_smooth(method="lm",se=F)+facet_grid(~sex)
obj3 #the female students tend to learn better as they get older
     #the male students tend to learn worse as they get older

summary(g)

var_numerice <- select(g, age, traveltime, studytime, G1, G2, G3, finalavgrades )
View(var_numerice)

library(pastecs)
library(moments) #kurtosis, skewness
res <- stat.desc(var_numerice) #descriptive statistics
round(res, 2)
summary(var_numerice)
kurtosis(g$age) #2.58 - the distribution is too peaked
skewness(g$age) #0.38 - positive skewness indicates a greater number of smaller values

hist(g$Dalc, breaks = 8, col = "gray", xlab = "Alcohol M-F", main = "Alcohol M-F Histogram")
#the largest amount of alcohol is consumed on Mondays (during the week) by the students

hist(g$studytime, breaks = 8, col = "gray", xlab = "Study Time", main = "Study Time Histogram")
#most students study between 0 and 300 minutes

View(g)

table(g$sex) #F:3988, M:3668
table(g$address) #R:1764, U:5892
table(g$famsize) #Greater than 3: 5440, Less than 3: 2216
table(g$internet) #No:1404, Yes:6252

######### The independent t-test for binary samples
boxplot(g$finalavgrades ~ g$sex)
var.test(g$finalavgrades ~ g$sex)
#H0: the variances are equal
#H1: the variances are different
# F test to compare two variances
# 
# data:  g$finalavgrades by g$sex
# F = 1.0121, num df = 3987, denom df = 3667, p-value = 0.7104
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.9498586 1.0783343
# sample estimates:
#   ratio of variances 
# 1.012103 
#INTERPRETATION: the p-value is p = 0.7104 > 0.05 (significance level) => there is no significant difference between the two variances

table(g$sex)
t.test(g$finalavgrades ~ g$sex, data = g, var.equal = FALSE) #the variance is not same
# Welch Two Sample t-test
# 
# data:  g$finalavgrades by g$sex
# t = -9.2474, df = 7608.1, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.6191629 -0.4025740
# sample estimates:
#   mean in group F mean in group M 
# 10.44333        10.95420 

g1 <- ggplot(g, aes(y = finalavgrades, x = sex))

g1 + stat_summary(fun = mean, geom = "bar", color = "black", fill = "white") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .2)

g$Mjob <- as.factor(g$Mjob)
levels(g$Mjob)
levels(g$Mjob) <- c("1", "2", "3", "4", "5")

g$Fjob <- as.factor(g$Fjob)
levels(g$Fjob)
levels(g$Fjob) <- c("1", "2", "3", "4", "5")

write_xlsx(g, path = "/Users/denisa1417/Desktop/Lucrare de disertatie/grades_new1123.xlsx", col_names = TRUE)
View(g)

g1 <- read_excel("/Users/denisa1417/Desktop/Lucrare de disertatie/grades_new1123.xlsx")
View(g1)

g$Mjob <- as.numeric(g$Mjob)
g1$activities <- as.factor(g1$activities)
levels(g1$activities)
levels(g1$activities) <- c("1", "2")
g1$activities <- as.numeric(g1$activities)


g1$internet <- as.factor(g1$internet)
levels(g1$internet)
levels(g1$internet) <- c("1", "2")
g1$internet <- as.numeric(g1$internet)

var <- c ("finalavgrades", "traveltime", "studytime", "absences", "Mjob", "activities", "internet")
cr1 <- g1[var]
cr1
res <- cor(cr1) #correlation matrix helps to choose the variables correlated with other and significant as a pair for the analysis
round(res, 3) #some of them present a positive correlation, some of the negative correlation and all of them present a weak correlation
            
cor.test(g1$finalavgrades, g1$traveltime) #between finalavgrades and travel time is a negative, weak correlation (-0.0988365) =>
                                          # => if finalavgrades increases, travel time decreases => 
                                          # => the less the student travels, the better his grades, but it is a week correlation
cor.test(g1$finalavgrades, g1$Mjob) #0.02328125 => positive and weak correlation
cor.test(g1$finalavgrades, g1$internet) #0.09387055 => positive and weak correlation

################### import data set after some changes in SPSS

library(readxl)
g2 <- read_excel("/Users/denisa1417/Desktop/Lucrare de disertatie/grades_new11123.xlsx")
View(g2)
print(str(g2)) #the type of the variables

##### balancing variable for logistic regression and decision trees

#rename
g2$finalavgrades_dummy <- as.factor(g2$finalavgrades_dummy)
levels(g2$finalavgrades_dummy)[levels(g2$finalavgrades_dummy) == "0"] <- "fail"
levels(g2$finalavgrades_dummy)[levels(g2$finalavgrades_dummy) == "1"] <- "success"

table(g2$finalavgrades_dummy) #fail: 1612, success: 3333 => unbalanced
#   =>  I have to balancing via oversampling using replacement for the minority class (fail)
g3 <- upSample(g2, g2$finalavgrades_dummy)
str(g3)
table(g3$finalavgrades_dummy) #fail: 3333, success: 3333 => balanced

#rename
g3$sex <- as.factor(g3$sex)
levels(g3$sex)[levels(g3$sex) == "0"] <- "F"
levels(g3$sex)[levels(g3$sex) == "1"] <- "M"
g3$sex

g3$address <- as.factor(g3$address)
levels(g3$address)[levels(g3$address) == "0"] <- "U"
levels(g3$address)[levels(g3$address) == "1"] <- "R"
g3$address

g3$famsize <- as.factor(g3$famsize)
levels(g3$famsize)[levels(g3$famsize) == "0"] <- "LE3"
levels(g3$famsize)[levels(g3$famsize) == "1"] <- "GT3"
g3$famsize

#save new dataset
write_xlsx(g3, path = "/Users/denisa1417/Desktop/Lucrare de disertatie/grades_new1123.xlsx", col_names = TRUE)

g4 <- read_excel("/Users/denisa1417/Desktop/Lucrare de disertatie/grades_new1123.xlsx")
View(g4)

### transform into 0 and 1 for logistic regressin
levels(g4$finalavgrades_dummy)[levels(g4$finalavgrades_dummy) == "fail"] <- 0
levels(g4$finalavgrades_dummy)[levels(g4$finalavgrades_dummy) == "success"] <- 1

#factor
g4$schoolsup <- as.factor(g4$schoolsup)
g4$paid <- as.factor(g4$paid )
g4$activities <- as.factor(g4$activities)
g4$higher <- as.factor(g4$higher)
g4$internet <- as.factor(g4$internet)
g4$famrel <- as.factor(g4$famrel)
g4$goout <- as.factor(g4$goout)
g4$Walc <- as.factor(g4$Walc)
g4$health<- as.factor(g4$health)
g4$famsup <- as.factor(g4$famsup)
g4$age <- as.factor(g4$age)
g4$finalavgrades_dummy <- as.factor(g4$finalavgrades_dummy)

levels(g4$age)[levels(g4$age) == 15] <- "< 18 ani"
levels(g4$age)[levels(g4$age) == 16] <- "< 18 ani"
levels(g4$age)[levels(g4$age) == 17] <- "< 18 ani"
levels(g4$age)[levels(g4$age) == 18] <- ">= 18 ani"
levels(g4$age)[levels(g4$age) == 19] <- ">= 18 ani"
levels(g4$age)
table(g4$finalavgrades_dummy, g4$age)

#### 70% for training set and 30% for testing set
#shuffle data because it is more efficient for creating the training and testing set
shuffle<- sample(1:nrow(g4))  
shuffle
g4 <- g4[shuffle, ]
head(g4)

### creating the sets
set.seed(222) #it keeps my result, otherwise the values of the 70/30 set would always be different
              #set up seed for the idea of the exact reproduction

createTrainTest <- sample(2, nrow(g4),
                          replace = TRUE,
                          prob = c(0.7, 0.3))

g4_train <- g4[createTrainTest == 1,] 
g4_test <- g4[createTrainTest == 2,]

View(g4_train)
table(g4_train$finalavgrades_dummy) #fail: 2352, success: 2346; it is also balanced

View(g4_test)
table(g4_test$finalavgrades_dummy) #fail: 982, success: 987; it is aslo balanced

summary(g4_train)
sapply(g4_train, class)
str(g4_train)

### REGRESIA LOGISTICA
library(nnet)
library(foreign)
library(ggplot2)
library(reshape2)
View(g4_train)

with(g4_train, table(g4_train$finalavgrades_dummy, g4_train$famrel))
#           1    2    3    4    5
# fail       0   73  503 1161  618  => 618 students who fail out of the total number (training set) have a very good relationship with the family
# success   50   54  387 1159  692  => 50 students who succes out of the total number (training set) have a very bad relationship with the family

with(g4_train, do.call(rbind, tapply(g4_train$absences, g4_train$finalavgrades_dummy, function(x) c(M = mean(x), SD = sd(x)))))

########### LOGISTIC REGRESSION  

##descrierea bazei de date cu functii de descriere 
sapply(g4_train, class)
dim(g4_train) 
names(g4_train)
str(g4_train)
summary(g4_train)

###contingency table (there must be no values of 0 in order to show a relationship between the categorical variables)###
xtabs(~finalavgrades_dummy+sex, data = g4) #1090 females students fail,1498 male students fail
xtabs(~finalavgrades_dummy+address, data = g4)
xtabs(~finalavgrades_dummy+famsize, data = g4)
xtabs(~finalavgrades_dummy+schoolsup, data = g4)
xtabs(~finalavgrades_dummy+famsup, data = g4)
xtabs(~finalavgrades_dummy+paid, data = g4)
xtabs(~finalavgrades_dummy+activities, data = g4)
xtabs(~finalavgrades_dummy+higher, data = g4)
xtabs(~finalavgrades_dummy+internet, data = g4)
xtabs(~finalavgrades_dummy+age, data = g4)
xtabs(~finalavgrades_dummy+famrel, data = g4)
xtabs(~finalavgrades_dummy+goout, data = g4)
xtabs(~finalavgrades_dummy+Walc, data = g4)
xtabs(~finalavgrades_dummy+health, data = g4)
xtabs(~finalavgrades_dummy+Medu, data = g4)
xtabs(~finalavgrades_dummy+Fedu, data = g4)
xtabs(~finalavgrades_dummy+Fjob, data = g4)
xtabs(~finalavgrades_dummy+Mjob, data = g4)

#regression
#TRAININ DATASET
g4_train$finalavgrades_dummy <- relevel(g4_train$finalavgrades_dummy, ref = "success")

model1 <- glm(finalavgrades_dummy~., data =g4_train, family = binomial())
summary(model1)

# model2 <- step(model1, direction = "backward") #start with a full (saturated) model and eliminate variables one by one until is found a model that best explains the data
# summary(model2)

model0 <- glm(finalavgrades_dummy ~ 1, data = g4_train, family = binomial())
model0

model3 <- step(model0, direction = "forward", scope = formula(model1)) #start with a null model (no variables) and add variables one by one until the stopping criteria is met
summary(model3)

model4 <- glm(finalavgrades_dummy ~ sex + Fedu + schoolsup + Mjob + 
  famrel + age + Medu + internet + absences + address + Fjob + higher + activities + 
  health + paid + goout + famsup + famsize, data = g4_train, family = binomial())
summary(model4) #model 4 which have all the variables is the best, has the smallest AIC and smallest residual deviance

# Confusion matrix - g4_train
probs <-predict(model4, g4_train, type='response')
pred.logit <- rep('success',length(probs))
pred.logit[probs>=0.6] <- 'fail'
pred.logit <- as.factor(pred.logit)

# Confusion matrix
confusionMatrix(g4_train$finalavgrades_dummy, pred.logit) #65% are classified correctly
table(g4_train$finalavgrades_dummy)

res_roc <- predict(model4, g4_train, type = 'response')
library(ROCR)
ROCRPred = prediction(res_roc, g4_train$type)
ROCRPref <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPref, colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))

exp(cbind(OR = coef(model4), confint(model4)))


#### now try the model on the test data set ############
g4_test$finalavgrades_dummy <- relevel(g4_test$finalavgrades_dummy, ref = "success")

test_model4 <-glm(finalavgrades_dummy ~ sex + Fedu + schoolsup + Mjob + 
                    famrel + age + Medu + internet + absences + address + Fjob + higher + activities + 
                    health + paid + goout + famsup + famsize, data = g4_test, family = binomial())
summary(test_model4) #AIC: 2350.9

# Matricea de confuzie - g4_test
probs_test <-predict(test_model4, g4_test, type='response')
pred.logit_test <- rep('success',length(probs_test))
pred.logit_test[probs_test >=0.5] <- 'fail'
pred.logit_test <- as.factor(pred.logit_test)

#matricea de confuzie
confusionMatrix(g4_test$finalavgrades_dummy, pred.logit_test) #67% are classified correctly
table(g4_test$finalavgrades_dummy)

### check the hypothesize for model4 from test data set ###
### the hypothesis of independence of errors

plot(test_model4) 

exp(cbind(OR = coef(test_model4), confint(test_model4)))

#chi-square
with(test_model4, null.deviance - deviance) #988.12
#freedom degrees
with(test_model4, df.null - df.residual) #27
#p-value
with(test_model4, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) #7.62675e-191
#the likelihood function
logLik(test_model4) #-2761.633 (df=28)

# install.packages ("epicalc", repos = "https://medipe.psu.ac.th/epicalc")
# library(epicalc)
# logistic.display(test_model4)

####ROC CURVE####
test_model4_roc <- multinom(finalavgrades_dummy ~ sex + Fedu + schoolsup + Mjob + 
                            famrel + age + Medu + internet + absences + address + Fjob + higher + activities + 
                            health + paid + goout + famsup + famsize, data = g4_test)

library(ROCR)
pred1 <- predict(test_model4_roc, g4_test, type = 'prob')
hist(pred1)

pred1 <- prediction(pred1, g4_test$finalavgrades_dummy)
eval1 <- performance(pred1, "acc")
eval1
plot(eval1)
abline(h = 0.50, v = Inf)

#Identify Best Value
max <- which.max(slot(eval1, "y.values")[[1]])
max
acc <- slot(eval1, "y.values")[[1]][max]
acc
cut <- slot(eval1, "x.values")[[1]][max]
cut
print(c(Accuracy = acc, Cutoff = cut))

#ROC Curve
roc1 <- performance(pred1, "tpr", "fpr") #trp - true positive rate - 662/ (662+348) => 0,655 => 65,5% acuratete
#fpr - false positive rate - 330/ (659+330) => 0,333 => 33,33%
roc1

plot(roc1, colorize = T,main = "Curba ROC", ylab = "Sensitivity", xlab = "1-Specificity") #sensitivity = tpr
abline(a = 0, b = 1)


################## DECISION TREES #########################
### CART

library(gmodels)#crostable
library(caret)
library(rpart)
library(rpart.plot)
library(data.table)
library(dplyr)
library(tidyverse)
library(plyr)

#model training
model_dt <- rpart(finalavgrades_dummy ~ ., data = g4_train, method="class")
rpart.plot(model_dt)
printcp(model_dt)
#print(model_dt)
summary(model_dt)
#plotcp(model_dt)

###########################
#prediction
pred.train.dt <- predict(model_dt, g4_test, type = "class")
pred.train.dt

#confusion matrix
confMatrix<-table(pred.train.dt, g4_test$finalavgrades_dummy)
confMatrix

#precision
presicion_dt<- confMatrix[1,1]/(sum(confMatrix[1,]))
presicion_dt #1

#accuracy
accuracy_Test <- sum(diag(confMatrix)) / sum(confMatrix)
accuracy_Test #1

#performance - perfect
confusionMatrix(pred.train.dt, g4_test$finalavgrades_dummy)
cptable <- printcp(model_dt) 

#prepruned
# Compute the accuracy of the pruned tree
g4_test$pred <- predict(model_dt, g4_test, type = "class")
g4_test$pred
accuracy_initial_model <- mean(g4_test$pred == g4_test$finalavgrades_dummy)
accuracy_initial_model #1

# Grow a tree with minsplit of 100 and max depth of 8
hr_model_preprun <- rpart(finalavgrades_dummy ~ ., data = g4_train, method = "class", 
                          control = rpart.control(cp = 0, maxdepth = 10,minsplit = 100))
rpart.plot(hr_model_preprun)
printcp(hr_model_preprun)

# Compute the accuracy of the pruned tree
g4_test$pred <- predict(hr_model_preprun, g4_test, type = "class")
accuracy_prepruned <- mean(g4_test$pred == g4_test$finalavgrades_dummy)
accuracy_prepruned #1

#postpruned
#Postpruning
# Prune the hr_base_model based on the optimal cp value
hr_model_pruned <- prune(model_dt, cp = 0.015424 )
# Compute the accuracy of the pruned tree
g4_test$pred <- predict(hr_model_pruned, g4_test, type = "class")
accuracy_postprun <- mean(g4_test$pred == g4_test$finalavgrades_dummy)
accuracy_postprun
data.frame(accuracy_initial_model, accuracy_prepruned, accuracy_postprun)

set.seed(123)
model2 <- train(
  finalavgrades_dummy ~., data = g4_train, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)


# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model2)


#make a C50 classifciation tree
library(C50)
dec50 <- C5.0(finalavgrades_dummy~., data = g4_train, method = 'class')
plot(dec50)

View(g4_train)

summary(dec50)
print(dec50)

C5.0Control(dec50)

#make a prediction
predict1 <- predict(dec50, g4_test, type = 'class')
predict1
table_p <- table(g4_test$finalavgrades_dummy, predict1)
table_p


#confusion matrix
confusionMatrix(predict1, g4_test$finalavgrades_dummy)

ruleModel <- C5.0(finalavgrades_dummy ~ ., data = g4_train, rules = TRUE)
ruleModel
summary(ruleModel)


ruleModel <- C5.0(finalavgrades_dummy ~ ., data = g4_test, rules = TRUE)
ruleModel
summary(ruleModel)

a <- data.frame(sex = c("F"), famrel = c(1), activities = c(0), Fedu = c(1), Fjob = c(1), health = c(1), age = c(15), absences = c(6), goout = c(5), Mjob = c(1), famsup = c(0), higher = c(1), famsize = c("GT3"), internet = c(0), Medu = c(1), paid = c(0), Walc = c(1), schoolsup = c(1), address = c("R"))
result <- predict(dec50, a)
print(result)
