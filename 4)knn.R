library(fastDummies)
library(FNN)
library(caret)

df_knn=dummy_cols(df, select_columns = c( "region.code", "Madrak", "Faaliat","n.t.s","N.S", "Masleh"))
df_knn=df_knn[,-c(1,7,8,10,13,14)]

##spliting data into train/valid set
set.seed(1) 

train.index <- sample(rownames(df_knn), length(rownames(df_knn))*0.8)

train.df <- df_knn[train.index,]
valid.df <- df_knn[setdiff(row.names(df_knn),train.index),]

##normalizing data
train.norm.df <- train.df
valid.norm.df <- valid.df

norm.values <- preProcess(train.df[, c(1,3,6:8,17:27)], method=c("center", "scale"))
train.norm.df[, c(1,3,6:8,17:27)] <- predict(norm.values, train.df[, c(1,3,6:8,17:27)])
valid.norm.df[, c(1,3,6:8,17:27)] <- predict(norm.values, valid.df[, c(1,3,6:8,17:27)])


# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy. = rep(0, 14))
# compute knn for different k on validation.

for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, -28], valid.norm.df[, -28],
                  cl = factor(train.norm.df[,28]), k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, factor(valid.norm.df[,28]))$overall[1]
}
accuracy.df

final.knn=knn(train.norm.df[, -28], valid.norm.df[, -28],cl = factor(train.norm.df[,28]), k = 3)
confusionMatrix(final.knn, factor(valid.norm.df[,28]))
