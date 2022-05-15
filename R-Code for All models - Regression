getwd() 

install.packages("zoo")
library(zoo)
library(lubridate)
library(dplyr)
library(psych)
library(openxlsx)
library(data.table)
library(randomForest)
library(gbm)
library(caret)
library(mltools)
library(e1071)
library(glmnet)
library(rpart)


Superstore <- read.xlsx("C:\\Users\\dell\\Desktop\\746 Data Science\\DataScience_MachineLearning\\Superstore.xlsx")
date <-read.csv("C:\\Users\\dell\\Desktop\\746 Data Science\\DataScience_MachineLearning\\Superstore_date.csv",header = TRUE,sep=",")
Superstore$OrderDate <- date$Order.Date
Superstore$ShipDate <- date$Ship.Date
Superstore$OrderDate <- mdy(Superstore$OrderDate)
Superstore$ShipDate <- mdy(Superstore$ShipDate)

#############create new variable###############
#Year and Month variables for order date:
Superstore$OrderYear<-year(Superstore$OrderDate)
Superstore$OrderMonth<-month(Superstore$OrderDate)

# create seasonality variable for order date
yq <- as.yearqtr(as.yearmon(Superstore$OrderDate, "%m/%d/%Y") + 1/12)
Superstore$Season <- factor(format(yq, "%q"), levels = 1:4, 
                            labels = c("winter", "spring", "summer", "fall"))

#season dummy:
Superstore$Spring=ifelse(Superstore$Season=="spring",1,0)
Superstore$Summer=ifelse(Superstore$Season=="summer",1,0)
Superstore$Fall=ifelse(Superstore$Season=="fall",1,0)
Superstore$Winter=ifelse(Superstore$Season=="winter",1,0)


#Freq variable:frequency of repeat purchase for customers
table(Superstore$Customer.ID)
Superstore <- data.table(Superstore)
Superstore[, `freq` := .N, by = Customer.ID]

#Ship mode dummy:
Superstore$ship_1st=ifelse(Superstore$Ship.Mode=="First Class",1,0)
Superstore$ship_2nd=ifelse(Superstore$Ship.Mode=="Second Class",1,0)
Superstore$ship_standard=ifelse(Superstore$Ship.Mode=="Standard Class",1,0)
Superstore$ship_sameday=ifelse(Superstore$Ship.Mode=="Same Day",1,0)

#Segment dummy:
Superstore$seg_cus=ifelse(Superstore$Segment=="Consumer",1,0)
Superstore$seg_cor=ifelse(Superstore$Segment=="Corporate",1,0)
Superstore$seg_home=ifelse(Superstore$Segment=="Home Office",1,0)

#Region dummy:"South""West""Central""East"   
Superstore$South=ifelse(Superstore$Region=="South",1,0)
Superstore$West=ifelse(Superstore$Region=="West",1,0)
Superstore$Central=ifelse(Superstore$Region=="Central",1,0)
Superstore$East=ifelse(Superstore$Region=="East",1,0)

#Big city dummy:
Superstore$NY=ifelse(Superstore$City=="New York City",1,0)
Superstore$LA=ifelse(Superstore$City=="Los Angeles",1,0)

#Category dummy:
Superstore$cat_fur=ifelse(Superstore$Category=="Furniture",1,0)
Superstore$cat_off=ifelse(Superstore$Category=="Office Supplies",1,0)
Superstore$cat_tech=ifelse(Superstore$Category=="Technology",1,0)

str(Superstore)
names(Superstore)<-str_replace_all(names(Superstore), "-",".")

#Subcategory dummy:
table1=table(Superstore$Sub.Category)
mytable=sort(table1,decreasing=TRUE)
table2=round(prop.table(mytable)*100,2)
sort(table2,decreasing=TRUE)

table1=table(Superstore$State)
sort(table1,decreasing=TRUE)
table1=table(Superstore$City)
sort(table1,decreasing=TRUE)

# create dummies for major classes (the number of value larger than 500), the remaining minority classes are merged to "others"
Superstore$sub_Binders=ifelse(Superstore$Sub.Category=="Binders",1,0)
Superstore$sub_Paper=ifelse(Superstore$Sub.Category=="Paper",1,0)
Superstore$sub_Furnishings=ifelse(Superstore$Sub.Category=="Furnishings",1,0)
Superstore$sub_Phones=ifelse(Superstore$Sub.Category=="Phones",1,0)
Superstore$sub_Storage=ifelse(Superstore$Sub.Category=="Storage",1,0)
Superstore$sub_Art=ifelse(Superstore$Sub.Category=="Art",1,0)
Superstore$sub_Accessories=ifelse(Superstore$Sub.Category=="Accessories",1,0)
Superstore$sub_Chairs=ifelse(Superstore$Sub.Category=="Chairs",1,0)
Superstore$sub_Others=ifelse(Superstore$Sub.Category=="Labels"|Superstore$Sub.Category=="Appliances"|
                               Superstore$Sub.Category=="Tables"|Superstore$Sub.Category=="Envelopes"|
                               Superstore$Sub.Category=="Bookcases"|Superstore$Sub.Category=="Fasteners"|
                               Superstore$Sub.Category=="Supplies"|Superstore$Sub.Category=="Machines"|
                               Superstore$Sub.Category=="Copiers",1,0)
View(Superstore)
str(Superstore)

############### Splitting Data ################
floor(0.2 * nrow(Superstore))
floor(0.4 * nrow(Superstore))
floor(0.6 * nrow(Superstore))
floor(0.8 * nrow(Superstore))

# train:first 80%
set.seed(13343)
train1 <- Superstore[1:7995,]
test1 <- Superstore[7996:nrow(Superstore),]

# train: first 60% + last 20%
set.seed(13343)
train2 <- Superstore[c(1:5996,7996:nrow(Superstore)),]
test2 <- Superstore[5997:7995,]

# train: first 40% + last40%
set.seed(13343)
train3 <- Superstore[c(1:3997,5997:nrow(Superstore)),]
test3 <- Superstore[3998:5996,]

# train: first 20% + last60%
set.seed(13343)
train4 <- Superstore[c(1:1998,3998:nrow(Superstore)),]
test4 <- Superstore[1999:3997,]

# train: last80%
set.seed(13343)
train5 <- Superstore[c(1999:nrow(Superstore)),]
test5 <- Superstore[1:1998,]


###############Stepwise Regression#################
library(caret)
set.seed(13343)
train1 <- createDataPartition(y = Superstore$Profit,p=.8,list = FALSE)
training <- Superstore[train1,]
test <- Superstore[-train1,]
#forward-stepwise
library(lars)
y = training$Profit
x = cbind(training$Sales, training$Quantity, training$Discount,
          training$freq,training$OrderYear,training$OrderMonth,as.factor(training$Ship.Mode),
          as.factor(training$Segment), as.factor(training$State),as.factor(training$City), as.factor(training$Region),as.factor(training$Category),
          training$Season,as.factor(training$Sub.Category))
res = lars(x, y, type="stepwise",)
print(summary(res))
res

####################linear regression###################
res= lm(Profit ~ Sales+Discount+Sub.Category+Category+Quantity+Region+OrderYear, data = train1)

#calculate mse, mae, r^2 for test dataset
lm.test<-predict(res,newdata = test1)
R2(lm.test,test1$Profit,form="traditional")
MAE(lm.test,test1$Profit)
mltools::mse(lm.test,test1$Profit)
RMSE(lm.test,test1$Profit)
#lm train2
res= lm(Profit ~ Sales+Discount+Sub.Category+Category+Quantity+Region+OrderYear, data = train2)
lm.test<-predict(res,newdata = test2)
R2(lm.test,test2$Profit,form="traditional")
MAE(lm.test,test2$Profit)
mltools::mse(lm.test,test2$Profit)
RMSE(lm.test,test2$Profit)
#lm train3
res= lm(Profit ~ Sales+Discount+Sub.Category+Category+Quantity+Region+OrderYear, data = train3)
lm.test<-predict(res,newdata = test3)
R2(lm.test,test3$Profit,form="traditional")
MAE(lm.test,test3$Profit)
mltools::mse(lm.test,test3$Profit)
RMSE(lm.test,test3$Profit)
#lm train4
res= lm(Profit ~ Sales+Discount+Sub.Category+Category+Quantity+Region+OrderYear, data = train4)
lm.test<-predict(res,newdata = test4)
R2(lm.test,test4$Profit,form="traditional")
MAE(lm.test,test4$Profit)
mltools::mse(lm.test,test4$Profit)
RMSE(lm.test,test4$Profit)
#lm train5
res= lm(Profit ~ Sales+Discount+Sub.Category+Category+Quantity+Region+OrderYear, data = train5)
lm.test<-predict(res,newdata = test5)
R2(lm.test,test5$Profit,form="traditional")
MAE(lm.test,test5$Profit)
mltools::mse(lm.test,test5$Profit)
RMSE(lm.test,test5$Profit)

#############ridge regression#######################
#train1
y <- train1$Profit
x <- data.matrix(train1[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
model <- glmnet(x, y, alpha = 0, lambda = 31.61955)
#
y_test <- test1$Profit
x_test <- data.matrix(test1[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
y_predicted <- predict(model, s = 31.61955, newx = x_test)
#
R2(y_predicted,test1$Profit,form="traditional")
MAE(y_predicted,test1$Profit)
mltools::mse(y_predicted,test1$Profit)
RMSE(y_predicted,test1$Profit)
#train2
y <- train2$Profit
x <- data.matrix(train2[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
model <- glmnet(x, y, alpha = 0, lambda = 31.61955)
y_test <- test2$Profit
x_test <- data.matrix(test2[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
y_predicted <- predict(model, s = 31.61955, newx = x_test)
#
R2(y_predicted,test2$Profit,form="traditional")
MAE(y_predicted,test2$Profit)
mltools::mse(y_predicted,test2$Profit)
RMSE(y_predicted,test2$Profit)
#train3
y <- train3$Profit
x <- data.matrix(train3[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
model <- glmnet(x, y, alpha = 0, lambda = 31.61955)
y_test <- test3$Profit
x_test <- data.matrix(test3[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
y_predicted <- predict(model, s = 31.61955, newx = x_test)
#
R2(y_predicted,test3$Profit,form="traditional")
MAE(y_predicted,test3$Profit)
mltools::mse(y_predicted,test3$Profit)
RMSE(y_predicted,test3$Profit)
#train4
y <- train4$Profit
x <- data.matrix(train4[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
model <- glmnet(x, y, alpha = 0, lambda = 31.61955)
y_test <- test4$Profit
x_test <- data.matrix(test4[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
y_predicted <- predict(model, s = 31.61955, newx = x_test)
#
R2(y_predicted,test4$Profit,form="traditional")
MAE(y_predicted,test4$Profit)
mltools::mse(y_predicted,test4$Profit)
RMSE(y_predicted,test4$Profit)
#train5
y <- train5$Profit
x <- data.matrix(train5[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
model <- glmnet(x, y, alpha = 0, lambda = 31.61955)
y_test <- test5$Profit
x_test <- data.matrix(test5[, c('Sales', 'Discount', 'Sub.Category', 'Category','Quantity','Region','OrderYear')])
y_predicted <- predict(model, s = 31.61955, newx = x_test)
#
R2(y_predicted,test5$Profit,form="traditional")
MAE(y_predicted,test5$Profit)
mltools::mse(y_predicted,test5$Profit)
RMSE(y_predicted,test5$Profit)

############### kNN Regression : Train 1 ################

#Predict using KNN regression 
tuneGrid <- expand.grid(k = seq(1,59, by=2))
set.seed(123)
model_knn <- train(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                     sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others, data=train1, method='knn',
                   preProcess = c('center','scale'),
                   trControl = trainControl(method = 'repeatedcv', number = 10, repeats = 5),
                   tuneGrid = tuneGrid)
model_knn

#RMSE plot for train1
train1results <- model_knn$results
train1results |> ggplot(aes(x=k,y=RMSE))+geom_point()+geom_line()

#Performance measures for test1
R2 <- R2(test1$Profit, predict(model_knn,test1))
R2
RMSE <- RMSE(test1$Profit, predict(model_knn,test1))
RMSE
MSE <- mse(test1$Profit, predict(model_knn,test1))
MSE
MAE <- MAE(test1$Profit, predict(model_knn,test1))
MAE

############### kNN Regression : Train 2 ################

#Predict using KNN regression 
tuneGrid <- expand.grid(k = seq(1,59, by=2))
set.seed(123)
model_knn <- train(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                     sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others, data=train2, method='knn',
                   preProcess = c('center','scale'),
                   trControl = trainControl(method = 'repeatedcv', number = 10, repeats = 5),
                   tuneGrid = tuneGrid)
model_knn

#RMSE plot for train2
train2results <- model_knn$results
train2results |> ggplot(aes(x=k,y=RMSE))+geom_point()+geom_line()

#Performance measures for test2
R2 <- R2(test2$Profit, predict(model_knn,test2))
R2
RMSE <- RMSE(test2$Profit, predict(model_knn,test2))
RMSE
MSE <- mse(test2$Profit, predict(model_knn,test2))
MSE
MAE <- MAE(test2$Profit, predict(model_knn,test2))
MAE

############### kNN Regression : Train 3 ################

#Predict using KNN regression 
tuneGrid <- expand.grid(k = seq(1,59, by=2))
set.seed(123)
model_knn <- train(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                     sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others, data=train3, method='knn',
                   preProcess = c('center','scale'),
                   trControl = trainControl(method = 'repeatedcv', number = 10, repeats = 5),
                   tuneGrid = tuneGrid)
model_knn

#RMSE plot for train3
train3results <- model_knn$results
train3results |> ggplot(aes(x=k,y=RMSE))+geom_point()+geom_line()

#Performance measures for test3
R2 <- R2(test3$Profit, predict(model_knn,test3))
R2
RMSE <- RMSE(test3$Profit, predict(model_knn,test3))
RMSE
MSE <- mse(test3$Profit, predict(model_knn,test3))
MSE
MAE <- MAE(test3$Profit, predict(model_knn,test3))
MAE

############### kNN Regression : Train 4 ################

#Predict using KNN regression 
tuneGrid <- expand.grid(k = seq(1,59, by=2))
set.seed(123)
model_knn <- train(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                     sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others, data=train4, method='knn',
                   preProcess = c('center','scale'),
                   trControl = trainControl(method = 'repeatedcv', number = 10, repeats = 5),
                   tuneGrid = tuneGrid)
model_knn

#RMSE plot for train4
train4results <- model_knn$results
train4results |> ggplot(aes(x=k,y=RMSE))+geom_point()+geom_line()

#Performance measures for test4
R2 <- R(test4$Profit, predict(model_knn,test4))
R2
RMSE <- RMSE(test4$Profit, predict(model_knn,test4))
RMSE
MSE <- mse(test4$Profit, predict(model_knn,test4))
MSE
MAE <- MAE(test4$Profit, predict(model_knn,test4))
MAE

############### kNN Regression : Train 5 ################

#Predict using KNN regression 
tuneGrid <- expand.grid(k = seq(1,59, by=2))
set.seed(123)
model_knn <- train(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                     sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others, data=train5, method='knn',
                   preProcess = c('center','scale'),
                   trControl = trainControl(method = 'repeatedcv', number = 10, repeats = 5),
                   tuneGrid = tuneGrid)
model_knn

#RMSE plot for train5
train5results <- model_knn$results
train5results |> ggplot(aes(x=k,y=RMSE))+geom_point()+geom_line()

#Performance measures for test5
R2 <- R2(test5$Profit, predict(model_knn,test5))
R2
RMSE <- RMSE(test5$Profit, predict(model_knn,test5))
RMSE
MSE <- mse(test5$Profit, predict(model_knn,test5))
MSE
MAE <- MAE(test5$Profit, predict(model_knn,test5))
MAE

###############SVR################
#Predict using SVM regression
#Train 1
#training
model_svm <- svm(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                   sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others,
                 data=train1, kernel='radial',  gamma= 
                   "1",epsilon=seq(0,1,0.1), cost=10)

pred <- predict(model_svm, train1)
R2(pred,train1$Profit,form="traditional")
MAE(pred,train1$Profit)
mltools::mse(pred,train1$Profit)
RMSE(pred,train1$Profit)

#testing
pred2 <- predict(model_svm, test2)
R2(pred2,test2$Profit,form="traditional")
MAE(pred2,test2$Profit)
mltools::mse(pred2,test2$Profit)
RMSE(pred2,test2$Profit)

#Train2
#training
model_svm <- svm(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                   sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others,
                 data=train2, kernel='radial',  gamma= 
                   "1",epsilon=seq(0,1,0.1), cost=10)
pred <- predict(model_svm, train2)
R2(pred,train2$Profit,form="traditional")
MAE(pred,train2$Profit)
mltools::mse(pred,train2$Profit)
RMSE(pred,train2$Profit)

#testing
pred1 <- predict(model_svm, test2)
R2(pred1,test2$Profit,form="traditional")
MAE(pred1,test2$Profit)
mltools::mse(pred1,test2$Profit)
RMSE(pred1,test2$Profit)

#Train3
#training
model_svm <- svm(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                   sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others,
                 data=train3, kernel='radial',  gamma= 
                   "1",epsilon=seq(0,1,0.1), cost=10)
pred <- predict(model_svm, train3)
R2(pred,train3$Profit,form="traditional")
MAE(pred,train3$Profit)
mltools::mse(pred,train3$Profit)
RMSE(pred,train3$Profit)

#testing
pred1 <- predict(model_svm, test3)
R2(pred1,test3$Profit,form="traditional")
MAE(pred1,test3$Profit)
mltools::mse(pred1,test3$Profit)
RMSE(pred1,test3$Profit)

#Train4
#training
model_svm <- svm(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                   sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others,
                 data=train4, kernel='radial',  gamma= 
                   "1",epsilon=seq(0,1,0.1), cost=10)
pred <- predict(model_svm, train4)
R2(pred,train4$Profit,form="traditional")
MAE(pred,train4$Profit)
mltools::mse(pred,train4$Profit)
RMSE(pred,train4$Profit)

#testing
pred1 <- predict(model_svm, test4)
R2(pred1,test4$Profit,form="traditional")
MAE(pred1,test4$Profit)
mltools::mse(pred1,test4$Profit)
RMSE(pred1,test4$Profit)

#Train5
#training
model_svm <- svm(Profit ~ OrderYear+South+West+Central+East+cat_fur+cat_off+cat_tech+Sales+Quantity+Discount+
                   sub_Binders+sub_Paper+sub_Furnishings+sub_Phones+ sub_Storage+sub_Art+sub_Accessories+sub_Chairs+sub_Others,
                 data=train5, kernel='radial',  gamma= 
                   "1",epsilon=seq(0,1,0.1), cost=10)
pred <- predict(model_svm, train5)
R2(pred,train5$Profit,form="traditional")
MAE(pred,train5$Profit)
mltools::mse(pred,train5$Profit)
RMSE(pred,train5$Profit)

#testing
pred1 <- predict(model_svm, test5)
R2(pred1,test5$Profit,form="traditional")
MAE(pred1,test5$Profit)
mltools::mse(pred1,test5$Profit)
RMSE(pred1,test5$Profit)

############### Decision Tree : Train 1################
set.seed(123)
tfit <- rpart(Profit ~ Category + Sub.Category + Region + Sales + Quantity + Discount 
              + OrderYear , 
              data = train1, method = 'anova', parms = list(split = 'information'),
              control = rpart.control(cp = 0.0001))
print(tfit)

printcp(tfit) 
plotcp(tfit)

set.seed(123)
tfit_pruned = prune(tfit, cp=0.00057599) 
plot(tfit_pruned, uniform=TRUE)
text(tfit_pruned)

#training
dt_pred_train<-predict(tfit_pruned, train1, method = "anova")
R2(dt_pred_train,train1$Profit,form="traditional")
# R:0.6761119

#testing
dt_pred_test<-predict(tfit_pruned, test1, method = "anova")
R2(dt_pred_test,test1$Profit,form="traditional")
MAE(as.numeric(dt_pred_test),test1$Profit)
mltools::mse(dt_pred_test,test1$Profit)
RMSE(dt_pred_test,test1$Profit)
# R 0.8153307; MAE 28.26408; MSE 12472.63; RMSE 111.6809

############### Decision Tree : Train 2################
library(rpart)
library(caret)
library(mltools)

set.seed(123)
tfit <- rpart(Profit ~ Category + Sub.Category + Region + Sales + Quantity + Discount 
              + OrderYear , 
              data = train2, method = 'anova', parms = list(split = 'information'),
              control = rpart.control(cp = 0.0001))
print(tfit)
printcp(tfit) 
plotcp(tfit)

set.seed(123)
tfit_pruned = prune(tfit, cp=0.00062462) 
plot(tfit_pruned, uniform=TRUE)
text(tfit_pruned)

#training
dt_pred_train<-predict(tfit_pruned, train2, method = "anova")
R2(dt_pred_train,train2$Profit,form="traditional")
# R:0.7038972

#testing
dt_pred_test<-predict(tfit_pruned, test2, method = "anova")
R2(dt_pred_test,test2$Profit,form="traditional")
MAE(as.numeric(dt_pred_test),test2$Profit)
mltools::mse(dt_pred_test,test2$Profit)
RMSE(dt_pred_test,test2$Profit)
# R 0.6075944; MAE 31.64094; MSE 33951.68; RMSE 184.2598

############### Decision Tree : Train 3################
library(rpart)
library(caret)
library(mltools)

set.seed(123)
tfit <- rpart(Profit ~ Category + Sub.Category + Region + Sales + Quantity + Discount 
              + OrderYear , 
              data = train3, method = 'anova', parms = list(split = 'information'),
              control = rpart.control(cp = 0.0001))
print(tfit)
printcp(tfit) 
plotcp(tfit)

set.seed(123)
tfit_pruned = prune(tfit, cp=0.00049604) 
plot(tfit_pruned, uniform=TRUE)
text(tfit_pruned)

#training
dt_pred_train<-predict(tfit_pruned, train3, method = "anova")
R2(dt_pred_train,train3$Profit,form="traditional")
# R:0.6961206

#testing
dt_pred_test<-predict(tfit_pruned, test3, method = "anova")
R2(dt_pred_test,test3$Profit,form="traditional")
MAE(as.numeric(dt_pred_test),test3$Profit)
mltools::mse(dt_pred_test,test3$Profit)
RMSE(dt_pred_test,test3$Profit)
# R 0.7953653; MAE 28.17141; MSE 9855.999; RMSE 99.27738

############### Decision Tree : Train 4################
library(rpart)
library(caret)
library(mltools)

set.seed(123)
tfit <- rpart(Profit ~ Category + Sub.Category + Region + Sales + Quantity + Discount 
              + OrderYear , 
              data = train4, method = 'anova', parms = list(split = 'information'),
              control = rpart.control(cp = 0.00001))
print(tfit)
printcp(tfit) 
plotcp(tfit)

set.seed(123)
tfit_pruned = prune(tfit, cp=3.9933e-05) 
plot(tfit_pruned, uniform=TRUE)
text(tfit_pruned)

#training
dt_pred_train<-predict(tfit_pruned, train4, method = "anova")
R2(dt_pred_train,train4$Profit,form="traditional")
# R:0.7970034

#testing
dt_pred_test<-predict(tfit_pruned, test4, method = "anova")
R2(dt_pred_test,test4$Profit,form="traditional")
MAE(as.numeric(dt_pred_test),test4$Profit)
mltools::mse(dt_pred_test,test4$Profit)
RMSE(dt_pred_test,test4$Profit)
# R 0.069237; MAE 26.39784; MSE 31640.89; RMSE 177.8789

############### Decision Tree : Train 5################
library(rpart)
library(caret)
library(mltools)

set.seed(123)
tfit <- rpart(Profit ~ Category + Sub.Category + Region + Sales + Quantity + Discount 
              + OrderYear , 
              data = train4, method = 'anova', parms = list(split = 'information'),
              control = rpart.control(cp = 0.00001))
print(tfit)
printcp(tfit) 
plotcp(tfit)

set.seed(123)
tfit_pruned = prune(tfit, cp=3.7255e-04) 
plot(tfit_pruned, uniform=TRUE)
text(tfit_pruned)

#training
dt_pred_train<-predict(tfit_pruned, train4, method = "anova")
R2(dt_pred_train,train4$Profit,form="traditional")
# R:0.7902507

#testing
dt_pred_test<-predict(tfit_pruned, test4, method = "anova")
R2(dt_pred_test,test4$Profit,form="traditional")
MAE(as.numeric(dt_pred_test),test4$Profit)
mltools::mse(dt_pred_test,test4$Profit)
RMSE(dt_pred_test,test4$Profit)
# R 0.05594954; MAE 30.30986; MSE 32092.59; RMSE 179.1441

###############Random forest model##########################
colnames(training)

set.seed(123)
# Create random forest for regression
rf_model <- randomForest(training$Profit ~ ., data = training[,c(8,13,15:16,18:20)], mtry = 3,
                         importance = TRUE, na.action = na.omit)
#rf for train1

bestmtry <- tuneRF(train1[,c(8,13,15:16,18:20)], train1$Profit, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#best mtry = 4

rf <- randomForest(train1$Profit ~ ., data = train1[,c(8,13,15:16,18:20)], mtry = 4,
                   importance = TRUE, na.action = na.omit)
rf

#training

rfpred_train = predict(rf, train1[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_train,train1$Profit,form="traditional")

#r2=0.9319615 

#testing

rfpred_test <- predict(rf, test1[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_test,test1$Profit,form="traditional")
MAE(as.numeric(rfpred_test),test1$Profit)
RMSE(rfpred_test,test1$Profit)

#r2=0.879465 MAE=19.20653 RMSE=90.22736



#rf for train2

bestmtry <- tuneRF(train2[,c(8,13,15:16,18:20)], train2$Profit, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#best mtry = 6

rf <- randomForest(train2$Profit ~ ., data = train2[,c(8,13,15:16,18:20)], mtry = 6,
                   importance = TRUE, na.action = na.omit)
rf

#training

rfpred_train = predict(rf, train2[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_train,train2$Profit,form="traditional")

#r2=0.95

#testing

rfpred_test <- predict(rf, test2[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_test,test2$Profit,form="traditional")
MAE(as.numeric(rfpred_test),test2$Profit)
RMSE(rfpred_test,test2$Profit)

#r2=0.72 MAE=20.51 RMSE=154.04


#rf for train3

bestmtry <- tuneRF(train3[,c(8,13,15:16,18:20)], train3$Profit, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#best mtry = 4

rf <- randomForest(train3$Profit ~ ., data = train3[,c(8,13,15:16,18:20)],mtry= 4,
                   importance = TRUE, na.action = na.omit)
rf

#training

rfpred_train = predict(rf, train3[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_train,train3$Profit,form="traditional")

#r2=0.93

#testing

rfpred_test <- predict(rf, test3[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_test,test3$Profit,form="traditional")
MAE(as.numeric(rfpred_test),test3$Profit)
RMSE(rfpred_test,test3$Profit)

#r2=0.93 MAE=16.69 RMSE=68.52



#rf for train4

bestmtry <- tuneRF(train4[,c(8,13,15:16,18:20)], train4$Profit, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#best mtry = 6

rf <- randomForest(train4$Profit ~ ., data = train4[,c(8,13,15:16,18:20)],mtry= 6,
                   importance = TRUE, na.action = na.omit)
rf

#training

rfpred_train = predict(rf, train4[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_train,train4$Profit,form="traditional")

#r2=0.96

#testing

rfpred_test <- predict(rf, test4[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_test,test4$Profit,form="traditional")
MAE(as.numeric(rfpred_test),test4$Profit)
RMSE(rfpred_test,test4$Profit)

#r2=0.28 MAE=20.99 RMSE=156.24



#rf for train5

bestmtry <- tuneRF(train5[,c(8,13,15:16,18:20)], train5$Profit, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

#best mtry = 4

rf <- randomForest(train5$Profit ~ ., data = train5[,c(8,13,15:16,18:20)],mtry= 4,
                   importance = TRUE, na.action = na.omit)
rf

#training

rfpred_train = predict(rf, train5[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_train,train5$Profit,form="traditional")

#r2=0.94

#testing

rfpred_test <- predict(rf, test5[,c(8,13,15:16,18:20)], method = "anova")
R2(rfpred_test,test5$Profit,form="traditional")
MAE(as.numeric(rfpred_test),test5$Profit)
RMSE(rfpred_test,test5$Profit)

#r2=0.64 MAE=21.32 RMSE=116.82

#avg of 5 RMSE = 119.24


############Gradient Boosting#############
#1
# train: first 80%
set.seed(13343)
train1 <- Superstore[1:7995,]
test1 <- Superstore[7996:nrow(Superstore),]

ntree=500
gbmfit=gbm(Profit ~ as.factor(Region)+ as.factor(Category) + as.factor(Sub.Category) 
           + Sales + Quantity + Discount + as.factor(OrderYear), 
           data = train1, n.trees=ntree,interaction.depth = 3,
           shrinkage = .1,distribution = 'gaussian', n.minobsinnode = 5,
           bag.fraction = .7,train.fraction = 1)
summary.gbm(gbmfit)
gbm.train<-predict(gbmfit,newdata = train1,n.trees=500)
R2(gbm.train,train1$Profit,form="traditional")

pred<-predict(gbmfit,newdata = test1,n.trees=500)
gbm.test1<-pred-test1$Profit

R2(pred,test1$Profit,form="traditional")
MAE(pred,test1$Profit)
mltools::mse(pred,test1$Profit)
RMSE(pred,test1$Profit)

#2
# train: first 60% + last 20%
set.seed(13343)
train1 <- Superstore[c(1:5996,7996:nrow(Superstore)),]
test1 <- Superstore[5997:7995,]


ntree=500
gbmfit=gbm(Profit ~ as.factor(Region)+ as.factor(Category) + as.factor(Sub.Category) 
           + Sales + Quantity + Discount + as.factor(OrderYear), 
           data = train1, n.trees=ntree,interaction.depth = 3,
           shrinkage = .1,distribution = 'gaussian', n.minobsinnode = 5,
           bag.fraction = .7,train.fraction = 1)
summary.gbm(gbmfit)
gbm.train<-predict(gbmfit,newdata = train1,n.trees=500)
R2(gbm.train,train1$Profit,form="traditional")

pred<-predict(gbmfit,newdata = test1,n.trees=500)
gbm.test1<-pred-test1$Profit

R2(pred,test1$Profit,form="traditional")
MAE(pred,test1$Profit)
mltools::mse(pred,test1$Profit)
RMSE(pred,test1$Profit)

#3
#train: first 40% + last40%
set.seed(13343)
train1 <- Superstore[c(1:3997,5997:nrow(Superstore)),]
test1 <- Superstore[3998:5996,]


ntree=500
gbmfit=gbm(Profit ~ as.factor(Region)+ as.factor(Category) + as.factor(Sub.Category) 
           + Sales + Quantity + Discount + as.factor(OrderYear), 
           data = train1, n.trees=ntree,interaction.depth = 3,
           shrinkage = .1,distribution = 'gaussian', n.minobsinnode = 5,
           bag.fraction = .7,train.fraction = 1)
gbm.train<-predict(gbmfit,newdata = train1,n.trees=500)
summary.gbm(gbmfit)
R2(gbm.train,train1$Profit,form="traditional")

pred<-predict(gbmfit,newdata = test1,n.trees=500)
gbm.test1<-pred-test1$Profit

R2(pred,test1$Profit,form="traditional")
MAE(pred,test1$Profit)
mltools::mse(pred,test1$Profit)
RMSE(pred,test1$Profit)

#4
#train: first 20% + last60%
set.seed(13343)
train1 <- Superstore[c(1:1998,3998:nrow(Superstore)),]
test1 <- Superstore[1999:3997,]


ntree=500
gbmfit=gbm(Profit ~ as.factor(Region)+ as.factor(Category) + as.factor(Sub.Category) 
           + Sales + Quantity + Discount + as.factor(OrderYear), 
           data = train1, n.trees=ntree,interaction.depth = 3,
           shrinkage = .1,distribution = 'gaussian', n.minobsinnode = 5,
           bag.fraction = .7,train.fraction = 1)
summary.gbm(gbmfit)
gbm.train<-predict(gbmfit,newdata = train1,n.trees=500)
R2(gbm.train,train1$Profit,form="traditional")

pred<-predict(gbmfit,newdata = test1,n.trees=500)
gbm.test1<-pred-test1$Profit

R2(pred,test1$Profit,form="traditional")
MAE(pred,test1$Profit)
mltools::mse(pred,test1$Profit)
RMSE(pred,test1$Profit)

#5
#train: last80%
set.seed(13343)
train1 <- Superstore[c(1999:nrow(Superstore)),]
test1 <- Superstore[1:1998,]


ntree=500
gbmfit=gbm(Profit ~ as.factor(Region)+ as.factor(Category) + as.factor(Sub.Category) 
           + Sales + Quantity + Discount + as.factor(OrderYear), 
           data = train1, n.trees=ntree,interaction.depth = 3,
           shrinkage = .1,distribution = 'gaussian', n.minobsinnode = 5,
           bag.fraction = .7,train.fraction = 1)
summary.gbm(gbmfit)
gbm.train<-predict(gbmfit,newdata = train1,n.trees=500)
R2(gbm.train,train1$Profit,form="traditional")

pred<-predict(gbmfit,newdata = test1,n.trees=500)
gbm.test1<-pred-test1$Profit

R2(pred,test1$Profit,form="traditional")
MAE(pred,test1$Profit)
mltools::mse(pred,test1$Profit)
RMSE(pred,test1$Profit)

#####################################################
# split criterion is information entropy
# control = rpart.control(cp = 0.0001, maxdepth = 15,minsplit = 13);R 0.5799096;MAE 29.57661
set.seed(123)
tfit <- rpart(Profit ~ Category + Sub.Category + Region + Sales + Quantity + Discount 
              + OrderYear , 
              data = training, method = 'anova', parms = list(split = 'information'),
              control = rpart.control(cp = 0.0001, maxdepth = 15,minsplit = 13, minibucket=5))
print(tfit) # tree give us explicit rules, see the rule in text

plot(tfit, uniform=TRUE) # plot the tree
text(tfit)

####pruning#####
printcp(tfit)
plotcp(tfit)

tfit_pruned = prune(tfit, cp=0.00016686) 
plot(tfit_pruned, uniform=TRUE)
text(tfit_pruned)
