df_log=dummy_cols(df, select_columns = c( "Faaliat","n.t.s","N.S"))
df_log=df_log[,-c(1,7,8,10,13,14)]

##spliting data into train/valid set
set.seed(1) 

train.index <- sample(rownames(df_log), length(rownames(df_log))*0.8)

train.df <- df_log[train.index,]
valid.df <- df_log[setdiff(row.names(df_log),train.index),]


log=glm(factor(outcome)~.,data=train.df,family=binomial)
summary(log)

log1=glm(factor(outcome)~.,data=train.df[,-c(41,38,33)],family=binomial)
summary(log1)

log2=glm(factor(outcome)~.,data=train.df[,-c(41,38,33,30)],family=binomial)
summary(log2)

log3=glm(factor(outcome)~.,data=train.df[,-c(41,38,33,30,15)],family=binomial)
summary(log3)


log4=glm(factor(outcome)~.,data=train.df[,-c(41,38,33,30,15,11)],family=binomial)
summary(log4)

log5=glm(factor(outcome)~.,data=train.df[,-c(41,38,33,30,15,11,21)],family=binomial)
summary(log5)

final1=glm(factor(outcome)~.,data=train.df[,-c(39,19,23,12,8,1,25,22,16,36,5,10,2,29,3,7,40,26,41,38,33,30,15,11,21,18)],family=binomial)
summary(final1)

final2=glm(factor(outcome)~.,data=train.df[,-c(8,1,25,22,16,36,5,10,2,29,3,7,40,26,41,38,33,30,15,11,21,18)],family=binomial)
summary(final2)


final1.pred <- predict(final1, valid.df[, -28], type = "response")

confusionMatrix(factor((final1.pred>0.5)+0), factor(valid.df$outcome))

final2.pred <- predict(final2, valid.df[, -28], type = "response")

confusionMatrix(factor((final2.pred>0.5)+0), factor(valid.df$outcome))
