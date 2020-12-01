############################### EDA ###################################################################
install.packages("psych")
library(psych)

View(Zoo)
attach(Zoo)
summary(Zoo)
is.na(Zoo)
qqnorm(hair)
qqline(hair)
qqnorm(feathers)
qqline(feathers)
qqnorm(milk)
qqline(milk)
qqnorm(eggs)
qqline(eggs)
qqnorm(airborne)
qqline(airborne)
qqnorm(aquatic)
qqline(aquatic)
qqnorm(predator)
qqline(predator)
qqnorm(toothed)
qqline(toothed)
qqnorm(backbone)
qqline(backbone)
qqnorm(breathes)
qqline(breathes)
boxplot(Zoo$venomous)
boxplot(Zoo$fins)
boxplot(Zoo$legs)
boxplot(Zoo$tail)
boxplot(Zoo$domestic)
boxplot(Zoo$catsize)
boxplot(Zoo$type)
plot(aquatic,fins)
plot(domestic,tail)
plot(hair,feathers)
hist(hair)
hist(domestic)
hist(tail)
hist(breathes)

pairs.panels(Zoo)
cor(type,hair)
cor(type,domestic)
cor(type,tail)
cor(type,fins)
cor(type,breathes)
cor(type,toothed)
cor(type,aquatic)
cor(type,predator)
cor(type,airborne)
cor(type,feathers)


############################### Table of "type" ########################################################
View(Zoo)
Zoo <- Zoo[-1]  ##### dropped the animal.name feature
View(Zoo)
table(Zoo$type)
str(Zoo)


####################### Table or Proportions with more informative labels #########################
round(prop.table(table(Zoo$type)) * 100, digits = 2)


############## Summarizing any three numeric features ##############################################
summary(Zoo[c("domestic", "aquatic", "toothed")])


############# Created Normalization Function #########################################################

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

################## Applied Normalize Function on the Zoo dataset ###################################
Zoo_n <- as.data.frame(lapply(Zoo[1:17], normalize))
View(Zoo_n)

summary(Zoo_n$type)  ### confirmed that the normalization function is working


################ Created Training and Testing Data ##################################################
Zoo_train <- Zoo_n[1:70, ]
Zoo_test <- Zoo_n[71:101, ]


################ Created labels for Training and Test Data #########################################
Zoo_train_labels <- Zoo[1:70, 1]
Zoo_train_labels

Zoo_test_labels <- Zoo[71:101, 1]
Zoo_test_labels


################ Training a Model on the data #######################################################
install.packages("class")
library(class)

test_acc <- NULL
train_acc <- NULL

for (i in seq(1,50,2))
{
  Zoo_train_pred <- knn(train=Zoo_train,test=Zoo_train,cl=Zoo_train_labels,k=i)
  train_acc <- c(train_acc,mean(Zoo_train_pred==Zoo_train_labels))
  Zoo_test_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=i)
  test_acc <- c(test_acc,mean(Zoo_test_pred==Zoo_test_labels))
}

##################### Evaluating Model Performance #################################################

install.packages("gmodels")
library(gmodels)

############## Created the cross tabulation of predicted vs. actual ###############################
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred,
           prop.chisq=FALSE)


######################## Improving model performance ##############################################

####### used scale function to z-score standardize a data frame ################

Zoo_z <- as.data.frame(scale(Zoo))
View(Zoo_z)


### Confirming that the transformation was applied correctly ############
summary(Zoo_z$type)



### Created training and testing datasets ###############################################
Zoo_train <- Zoo_z[1:70, ]
Zoo_test <- Zoo_z[71:101, ]


############### Re-classifying test cases by building model ########################################
Zoo_test_pred <- knn(train = Zoo_train, test = Zoo_test,
                     cl = Zoo_train_labels, k=1)



############## Created the cross tabulation of predicted vs. actual ###############################
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred,
           prop.chisq=FALSE)



################################# Testing Accuracy ################################################# 

#### Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(1,50,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(1,50,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(1,50,2)))


################### Plotting 2 different graphs on same co-ordinate axis ###########################
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


Zoo_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=15)


############## Tried with several different values of k ##############################################
Zoo_train <- Zoo_n[1:70, ]
Zoo_test <- Zoo_n[71:101, ]


Zoo_test_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=5)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE) 

Zoo_test_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=10)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE)

Zoo_test_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=15)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE)

Zoo_test_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=20)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE)



######### This is a good model with "0" Miss classification.###############################
Zoo_test_pred <- knn(train = Zoo_train, test = Zoo_test, cl = Zoo_train_labels, k=5)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE) 

