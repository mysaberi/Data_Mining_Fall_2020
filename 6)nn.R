library(neuralnet)
library(nnet)
library(fastDummies)
library(forecast)
#create dummy variables
df_nn<-df
df_nn=dummy_cols(df, select_columns = c( "region.code", "Madrak", "Faaliat","n.t.s","N.S", "Masleh"))
df_nn=df_nn[,-c(1,7,8,10,13,14)]


##spliting data into train/valid set
set.seed(1) 
train.index <- sample(rownames(df_nn), length(rownames(df_nn))*0.8)

train.df <- df_nn[train.index,]
valid.df <- df_nn[setdiff(row.names(df_nn),train.index),]

##normalizing data
train.norm.df <- train.df
valid.norm.df <- valid.df

norm.values <- preProcess(train.df[, c(1,3,6:8,17:27)], method=c("center", "scale"))
train.norm.df[, c(1,3,6:8,17:27)] <- predict(norm.values, train.df[, c(1,3,6:8,17:27)])
valid.norm.df[, c(1,3,6:8,17:27)] <- predict(norm.values, valid.df[, c(1,3,6:8,17:27)])

nn=neuralnet(outcome~., train.norm.df, hidden =c(12,8,4),linear.output = FALSE,
             learningrate = 0.01,rep=25,err.fct = "ce",lifesign = "minimal",threshold = 0.15)


training.prediction=neuralnet::compute(nn, train.norm.df[,-28])

confusionMatrix(factor((training.prediction$net.result>0.5)+0), factor(train.norm.df$outcome))

validation.prediction=neuralnet::compute(nn, valid.norm.df[,-28])
confusionMatrix(factor((validation.prediction$net.result>0.5)+0), factor(valid.norm.df$outcome))

plot(nn,rep="best")
