############################### EDA ###################################################################
install.packages("psych")
library(psych)

View(glass)
attach(glass)
summary(glass)
is.na(glass)
qqnorm(RI)
qqline(RI)
qqnorm(Na)
qqline(Na)
qqnorm(Mg)
qqline(Mg)
qqnorm(Al)
qqline(Al)
qqnorm(Si)
qqline(Si)
qqnorm(K)
qqline(K)
qqnorm(Ca)
qqline(Ca)
qqnorm(Ba)
qqline(Ba)
qqnorm(Fe)
qqline(Fe)
qqnorm(Type)
qqline(Type)
boxplot(glass$RI)
boxplot(glass$Na)
boxplot(glass$Al)
boxplot(glass$Si)
boxplot(glass$K)
boxplot(glass$Ca)
boxplot(glass$Ba)
boxplot(glass$Fe)
boxplot(glass$Type)
plot(RI,Na)
plot(Si,K)
plot(Ca,Ba)
hist(Si)
hist(Fe)
hist(Type)
hist(Al)

pairs.panels(glass)
cor(Type,RI)
cor(Type,Na)
cor(Type,Mg)
cor(Type,Al)
cor(Type,Si)
cor(Type,K)
cor(Type,Ca)
cor(Type,Ba)
cor(Type,Fe)


############################### Table of "Type" ########################################################
View(glass)
glass <- glass[-1]  ##### dropped the RI (refractive index) feature

View(glass)
table(glass$Type)
str(glass)


####################### Table or Proportions with more informative labels #########################
round(prop.table(table(glass$Type)) * 100, digits = 2)


############## Summarizing any three numeric features ##############################################
summary(glass[c("K", "Si", "Type")])


############# Created Normalization Function #########################################################

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

################## Applied Normalize Function on the glass dataset ###################################
glass_n <- as.data.frame(lapply(glass[1:8], normalize))
View(glass_n)

summary(glass_n$Si)  ### confirmed that the normalization function is working


################ Created Training and Testing Data ##################################################
glass_train <- glass_n[1:114, ]
glass_test <- glass_n[115:214, ]


################ Created labels for Training and Test Data #########################################
glass_train_labels <- glass[1:114, 1]
glass_train_labels

glass_test_labels <- glass[115:214, 1]
glass_test_labels


################ Training a Model on the data #######################################################
install.packages("class")
library(class)

test_acc <- NULL
train_acc <- NULL


for (i in seq(1,50,2))

{
  glass_train_pred <- knn(train=glass_train,test=glass_train,cl=glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(glass_train_pred==glass_train_labels))
  glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=i)
  test_acc <- c(test_acc,mean(glass_test_pred==glass_test_labels))
}
  

##################### Evaluating Model Performance #################################################

install.packages("gmodels")
library(gmodels)

############## Created the cross tabulation of predicted vs. actual ###############################
CrossTable(x = glass_test_labels,  y = glass_test_pred,
           prop.chisq=FALSE)


######################## Improving model performance ##############################################

####### used scale function to z-score standardize a data frame ################
glass_z <- as.data.frame(scale(glass)) 
View(glass_z)


### Confirming that the transformation was applied correctly ############
summary(glass_z$Type)


### Created training and testing datasets ###############################################
glass_train <- glass_z[1:114, ]
glass_test <- glass_z[115:214, ]


############### Re-classifying test cases by building model ########################################
glass_test_pred <- knn(train = glass_train, test = glass_test,
                       cl = glass_train_labels, k=1)



############## Created the cross tabulation of predicted vs. actual ###############################
CrossTable(x = glass_test_labels, y = glass_test_pred,
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


glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=15)



############## Tried with several different values of k ##############################################
glass_train <- glass_n[1:114, ]
glass_test <- glass_n[115:214, ]

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=5)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=10)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=15)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=20)
CrossTable(x = glass_test_labels, y = glass_test_pred, prop.chisq=FALSE)




