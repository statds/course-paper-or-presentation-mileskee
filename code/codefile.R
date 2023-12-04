library(dplyr)
#data reading in
transfer23 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer.stats/transfer23.csv')
transfer22 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer.stats/transfer22.csv')
transfer21 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer.stats/transfer21.csv')
transfer21 <- transfer21 %>% select('player_name','team','conf','Min_per','ORtg','usg','eFG','ORB_per','DRB_per','AST_per',
                                    'TO_per','blk_per','stl_per','ftr','yr','ht','new.school','dbpm')
transfer22 <- transfer22 %>% select('player_name','team','conf','Min_per','ORtg','usg','eFG','ORB_per','DRB_per','AST_per',
                                    'TO_per','blk_per','stl_per','ftr','yr','ht','new.school','dbpm')
transfer23 <- transfer23 %>% select('player_name','team','conf','Min_per','ORtg','usg','eFG','ORB_per','DRB_per','AST_per',
                                    'TO_per','blk_per','stl_per','ftr','yr','ht','new.school','dbpm')
write.csv(transfer21,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer.stats/transfer21.csv')
write.csv(transfer22,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer.stats/transfer22.csv')
write.csv(transfer23,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/transfer.stats/transfer23.csv')
stats21 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/following.year.stats/stats21.csv')
stats22 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/following.year.stats/stats22.csv')
stats23 <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/following.year.stats/stats23.csv')
full21 <- merge(transfer21,stats21,by.x='player_name',by.y='Player')
full22 <- merge(transfer22,stats22,by.x='player_name',by.y='Player')
full23 <- merge(transfer23,stats23,by.x='player_name',by.y='Player')
fulldata <- rbind(full21,full22,full23)
write.csv(fulldata,'/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/fulldata.csv')
fulldata <- read.csv('/Users/mileskee7/Desktop/College/Senior Year/W/course-paper-or-presentation-mileskee/data/fulldata.csv')
fulldata <- fulldata[,-1]
fulldata$fromp5 <- ifelse(fulldata$conf %in% c('P12','B10','SEC','B12','BE','ACC'),1,0)
fulldata$top5 <- ifelse(fulldata$Conference %in% c('P12','B10','SEC','B12','BE','ACC'),1,0)
fulldata$oimp <- ifelse(fulldata$ORTG > fulldata$ORtg,1,0)
fulldata$dimp <- ifelse(fulldata$DBPM > fulldata$dbpm,1,0)
x <- cor(fulldata[,c(4:14,18,23:33,36)])
library(corrplot)
plot <- corrplot(x)
library(keras)
########### Linear Models
###ORTG linear model
ortg <- fulldata %>% select(Min_per,ORtg,usg,eFG,ORB_per,AST_per,TO_per,ftr,ORTG,fromp5,top5,oimp)
set.seed(1234)
traindex <- sample(1:nrow(fulldata),.7*nrow(fulldata))
train <- ortg[traindex,]
label_train <- train$ORTG
train2 <- train %>% select(-ORTG) %>% scale()
test <- ortg[-traindex,]
label_test <- test$ORTG
lin_ortg <- lm(ORTG~ORtg+eFG+ORB_per+AST_per+TO_per+ftr+as.factor(fromp5)+as.factor(top5)+Min_per+usg,data=train)
summary(lin_ortg)
predictions <- predict(lin_ortg,test)
test <- cbind(test,predictions)
plot(abs(test_ortg$predictions-test_ortg$ORTG),ylab='Absolute Error',main='Test Set Error of Offensive Rating Linear Model')
mean(sqrt((test_ortg$predictions-test_ortg$ORTG)^2))
improved <- test %>% filter(oimp==1)
nrow(improved %>% filter(predictions-ORtg > 0)) / nrow(improved)
deproved <- test_ortg %>% filter(ORTG-ORtg < 0)
nrow(deproved %>% filter(predictions-ORtg < 0)) / nrow(deproved)
(nrow(improved %>% filter(predictions-ORtg > 0)) + nrow(deproved %>% filter(predictions-ORtg < 0))) / (nrow(improved) + nrow(deproved))
#only significant variables
ortg <- ortg %>% filter(Min_per >= 10)
train <- ortg[traindex,]
test <- ortg[-traindex,]
lin_ortg_2 <- lm(ORTG~ORtg+ORB_per+AST_per+ftr+fromp5+Min_per+usg,data=train)
summary(lin_ortg_2)
predictions <- predict(lin_ortg_2,test)
test_ortg <- cbind(test,predictions)
plot(abs(test_ortg$predictions-test_ortg$ORTG),ylab='Absolute Error',main='Test Set Error of Offensive Rating Linear Model')
mean(sqrt((test_ortg$predictions-test_ortg$ORTG)^2))
improved <- test_ortg %>% filter(oimp==1)
nrow(improved %>% filter(predictions-ORtg > 0)) / nrow(improved)
deproved <- test_ortg %>% filter(ORTG-ORtg < 0)
nrow(deproved %>% filter(predictions-ORtg < 0)) / nrow(deproved)
(nrow(improved %>% filter(predictions-ORtg > 0)) + nrow(deproved %>% filter(predictions-ORtg < 0))) / (nrow(improved) + nrow(deproved))

#DRTG linear model
drtg <- fulldata %>% select(Min_per,DRB_per,blk_per,stl_per,dbpm,DBPM,fromp5,top5)
set.seed(1234)
traindex <- sample(1:nrow(fulldata),.7*nrow(fulldata))
train <- drtg[traindex,]
label_train_d <- train$DBPM
train2 <- train %>% select(-DBPM) %>% scale()
test <- drtg[-traindex,]
label_test_d <- test$DBPM
lin_drtg <- lm(DBPM~dbpm+DRB_per+blk_per+stl_per+fromp5+top5+Min_per,data=train)
summary(lin_drtg)
predictions <- predict(lin_drtg,test)
test <- cbind(test,predictions)
plot(test$predictions-test$DBPM,ylab='Absolute Error',main='Test Set Error of dbpm Linear Model')
mean(sqrt((test$predictions-test$dbpm)^2)) 
improved <- test %>% filter(DBPM-dbpm > 0)
nrow(improved %>% filter(predictions-dbpm > 0)) / nrow(improved)
deproved <- test %>% filter((DBPM-dbpm < 0))
nrow(deproved %>% filter(predictions-dbpm < 0)) / nrow(deproved)
(nrow(improved %>% filter(predictions-dbpm > 0)) + nrow(deproved %>% filter(predictions-dbpm < 0))) / (nrow(improved) + nrow(deproved))
#Only significant
lin_drtg <- lm(DBPM~dbpm+DRB_per+blk_per+fromp5+top5+Min_per,data=train)
summary(lin_drtg)
predictions <- predict(lin_drtg,test)
test <- cbind(test,predictions)
plot(test$predictions-test$DBPM,ylab='Absolute Error',main='Test Set Error of dbpm Linear Model')
mean(sqrt((test$predictions-test$dbpm)^2)) 
improved <- test %>% filter(DBPM-dbpm > 0)
nrow(improved %>% filter(predictions-dbpm > 0)) / nrow(improved)
deproved <- test %>% filter((DBPM-dbpm < 0))
nrow(deproved %>% filter(predictions-dbpm < 0)) / nrow(deproved)
(nrow(improved %>% filter(predictions-dbpm > 0)) + nrow(deproved %>% filter(predictions-dbpm < 0))) / (nrow(improved) + nrow(deproved))
##################### Classification Models
##ORTG classification model
ortg <- fulldata %>% select(Min_per,ORtg,usg,eFG,ORB_per,AST_per,TO_per,ftr,ORTG,fromp5,top5,oimp)
set.seed(1234)
traindex <- sample(1:nrow(fulldata),.7*nrow(fulldata))
train <- ortg[traindex,]
test <- ortg[-traindex,]
log_ortg <- glm(oimp~ORtg+eFG+ORB_per+AST_per+TO_per+ftr+as.factor(fromp5)+as.factor(top5)+Min_per+usg,data=train)
summary(log_ortg)
predictions <- predict(lin_ortg,test)
test_ortg <- cbind(test,predictions)
plot(abs(test_ortg$predictions-test_ortg$ORTG),ylab='Absolute Error',main='Test Set Error of Offensive Rating Linear Model')
mean(sqrt((test_ortg$predictions-test_ortg$ORTG)^2))
improved <- test %>% filter(oimp==1)
nrow(improved %>% filter(predictions-ORtg > 0)) / nrow(improved)
deproved <- test_ortg %>% filter(ORTG-ORtg < 0)
nrow(deproved %>% filter(predictions-ORtg < 0)) / nrow(deproved)
(nrow(improved %>% filter(predictions-ORtg > 0)) + nrow(deproved %>% filter(predictions-ORtg < 0))) / (nrow(improved) + nrow(deproved))






####### ML Models
#ORTG ML model
test2 <- test %>% select(-ORTG) %>% scale()
model <- keras_model_sequential() %>%
  layer_dense(units=8,activation='relu', input_shape=ncol(train2)) %>%
  layer_dense(units=6,activation='relu') %>%
  layer_dense(units=1)
model %>% compile(
  loss='mse',
  optimizer=optimizer_rmsprop(),
  metrics=list('mean_absolute_error')
)  
fit <- model %>% fit(
  train2,
  label_train,
  batch_size=50,
  epochs=300,
  validation_split=.1,
  verbose=1
)
save_model_weights_tf(model, './checkpoints/my_checkpoint')
load_model_weights_tf(model, './checkpoints/my_checkpoint')
plot(fit)
model %>% evaluate(test2,label_test)
prediction <- predict(model,test2)
plot(prediction-label_test)
mean(sqrt((prediction-label_test)^2))
summary(label_test)
summary(prediction)
test_ortg_nn <- cbind(test,prediction)
improved <- test_ortg_nn %>% filter(ORTG-ORtg > 0)
nrow(improved %>% filter(prediction-ORtg > 0)) / nrow(improved)
nrow(deproved %>% filter(predictions-ORtg < 0)) / nrow(deproved)



#DRTG linear model
drtg <- fulldata %>% select(Min_per,DRB_per,blk_per,stl_per,dbpm,DBPM,fromp5,top5)
set.seed(1234)
traindex <- sample(1:nrow(fulldata),.7*nrow(fulldata))
train <- drtg[traindex,]
label_train_d <- train$DBPM
train2 <- train %>% select(-DBPM) %>% scale()
test <- drtg[-traindex,]
label_test_d <- test$DBPM
lin_drtg <- lm(DBPM~dbpm+DRB_per+blk_per+stl_per+fromp5+top5+Min_per,data=train)
summary(lin_drtg)
predictions <- predict(lin_drtg,test)
test <- cbind(test,predictions)
plot(test$predictions-test$DBPM)
mean(sqrt((test$predictions-test$dbpm)^2)) 
improved <- test %>% filter(DBPM-dbpm > 0)
nrow(improved %>% filter(predictions-dbpm > 0)) / nrow(improved)
deproved <- test %>% filter((DBPM-dbpm < 0))
nrow(deproved %>% filter(predictions-dbpm < 0)) / nrow(deproved)





test2 <- test %>% select(-DBPM) %>% scale()
model <- keras_model_sequential() %>%
  layer_dense(units=6,activation='relu', input_shape=ncol(train2)) %>%
  layer_dense(units=3,activation='relu') %>%
  layer_dense(units=1)
model %>% compile(
  loss='mse',
  optimizer=optimizer_rmsprop(),
  metrics=list('mean_absolute_error')
)  
fit <- model %>% fit(
  train2,
  label_train_d,
  epochs=300,
  validation_split=.1,
  verbose=1
)
plot(fit)
model %>% evaluate(test2,label_test_d)
prediction <- predict(model,test2)
plot(prediction-label_test)
summary(label_test)
summary(prediction)
test <- cbind(test,prediction)
improved <- test %>% filter(dbpm-DBPM > 0)
nrow(improved %>% filter(predictions-dbpm > 0)) / nrow(improved)
deproved <- test %>% filter((DBPM-dbpm < 0))
nrow(deproved %>% filter(predictions-dbpm < 0)) / nrow(deproved)
