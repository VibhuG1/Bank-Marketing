rm(list=ls())
setwd("~/Desktop/Machine Learning/7. Logistic regression")
data <- read.csv("bank-additional-full.csv",header = T,sep = ';')

library(party)
library(dplyr) 
library(sqldf)
library(plotly)
library(rpivotTable)

data$y <- (data$y == "yes")*1


tail(data,100)
str(data)

summary(data)

hist(data$age)
sort(unique(data$pdays))



temp <- sqldf("select  count(y) as cnt,pdays
               from data 
               group by pdays order by pdays")


rpivotTable(data, rows="education", col="y" , aggregatorName="Count", 
                       vals="mpg")


# Missing Values
colnames(data)
sum(is.na(data$nr.employed))
sapply(data, function(x) sum(is.na(x)))
apply(data, 2, function(x) sum(is.na(x)))

#treat missing values
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)


boxplot(data$age)
summary(data$age)
sd(data$age)

data$Age_bin <- cut(data$age, breaks = c(15,25,35,45,55,65,100),
                    include.lowest = TRUE)
sort(unique(data$Age_bin))
data$Age_bin
Age.freq <- table(data$Age_bin)
Age.freq
class(Age.freq)
barplot(Age.freq)
class(data)

unique(data$pcon )
data$pcon <- (data$pdays != 999)*1
str(data)
data$pcon <- as.factor(data$pcon)



data$y <- as.factor(data$y)
data <- subset(data, select=-c(age,pdays))


#Chisq test 
library(MASS)       
tbl = table(data$y, data$Age_bin) 
tbl
chisq.test(tbl)

library(InformationValue)

factor_vars <- c ("job", "marital", "education", "default", "housing", "loan", "contact",
                  "month","day_of_week", "poutcome","Age_bin","pcon")


all_iv <- data.frame(VARS=factor_vars, IV=numeric(length(factor_vars)), STRENGTH=character(length(factor_vars))
                     , stringsAsFactors = F)  # init output dataframe

for (factor_var in factor_vars){
  all_iv[all_iv$VARS == factor_var, "IV"] <- InformationValue::IV(X=data[, factor_var], Y=data$y)
  all_iv[all_iv$VARS == factor_var, "STRENGTH"] <- attr(InformationValue::IV(X=data[, factor_var], Y=data$y), "howgood")
}

all_iv <- all_iv[order(-all_iv$IV), ]  # sort

all_iv

for(factor_var in factor_vars){
  data[[factor_var]] <- WOE(X=data[, factor_var], Y=data$y)
}

unique(WOE(X=data[, "pcon"], Y=data$y))

#removed due to low IV
data <- subset(data, select=-c(marital ,day_of_week,housing, loan))






#removed due to high VIF  
data <- subset(data, select=-c(nr.employed))
data <- subset(data, select=-c(emp.var.rate))
data <- subset(data, select=-c(poutcome))




library(caret)
intrain<-createDataPartition(y=data$y,p=0.7,list=FALSE)
train<-data[intrain,]
test<-data[-intrain,]
str(test)

#Logistic Regression
model <- glm(y ~ ., family = binomial(link = 'logit'), data = train)
summary(model)

# Multicolliniarity

library(car)
vif(model)




#Confusion Matrix

train$predict <- predict(model, newdata=train, type='response')
train$predict_r <- ifelse(train$predict > 0.5,1,0)


test$predict <- predict(model, newdata=test, type='response')
test$predict_r <- ifelse(test$predict > 0.5,1,0)

misClasificError_train <- mean(train$predict_r != train$y)
1-misClasificError_train



misClasificError_test <- mean(test$predict_r != test$y)
1-misClasificError_test



table(test$predict_r,test$y)
table(train$predict_r,train$y)
 
nrow(test)
table(test$y)
# ROC
library(pROC)

auc_train <- auc(train$y, train$predict)
auc_train

auc_test<- auc(test$y, test$predict)
auc_test

g <- roc(y ~ predict, data = train)
g <- roc(y ~ predict, data = test)
g

plot(g) 


#final variable list
 colnames(data)
[1] "job"            "marital"        "education"      "default"        "contact"       
[6] "month"          "day_of_week"    "duration"       "campaign"       "previous"      
[11] "cons.price.idx" "cons.conf.idx"  "euribor3m"      "y"              "Age_bin"       
[16] "pcon"
