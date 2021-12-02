library(rpart)
library(rpart.plot)
library(forecast)

###preparing data dat
df[,"Madrak"]=factor(df[,"Madrak"])
df[,"Faaliat"]=factor(df[,"Faaliat"])
df[,"n.t.s"]=factor(df[,"n.t.s"])
df[,"N.S"]=factor(df[,"N.S"])
df[,"Masleh"]=factor(df[,"Masleh"])

##spliting data into train/valid set
df_tree=df
set.seed(1) 
train.index <- sample(rownames(df_tree), length(rownames(df_tree))*0.8)

train.df <- df_tree[train.index,]
valid.df <- df_tree[setdiff(row.names(df_tree),train.index),]

##normalizing data
train.norm.df <- train.df
valid.norm.df <- valid.df

norm.values <- preProcess(train.df[,c(2,4,9,11,12,23:33)], method=c("center", "scale"))
train.norm.df[, c(2,4,9,11,12,23:33)] <- predict(norm.values, train.df[, c(2,4,9,11,12,23:33)])
valid.norm.df[, c(2,4,9,11,12,23:33)] <- predict(norm.values, valid.df[, c(2,4,9,11,12,23:33)])

#default classification tree
default <- rpart(outcome ~ ., data = train.norm.df, method = "class")
# plot tree

prp(default,box.palette="RdBu", shadow.col="gray",
    nn=TRUE,split.font = 4,varlen = -15,type=1)

#accuracy of default classification tree on train set
def_pred=predict(default,train.norm.df,type = "class")

confusionMatrix(def_pred, factor(train.norm.df$outcome))

#accuracy of default classification tree on valid set
def_pred_valid=predict(default,valid.norm.df,type = "class")
confusionMatrix(def_pred_valid, factor(valid.norm.df$outcome))

####################################################################################

#almost full classification tree
full <- rpart(outcome ~ ., data = train.norm.df, method = "class",
              cp = 0, minsplit = 5, xval = 10)

# print the table of cps
printcp(full)

#prune the tree based on the cp that has the lowest error on validation set
pruned.model <- prune(full,
                      cp = full$cptable[which.min(full$cptable[,"xerror"]),"CP"])
#plot tree

prp(pruned.model,box.palette="RdBu", shadow.col="gray",
    nn=TRUE,split.font = 4,varlen = -15,type=1)

###evaluate pruned model on train data
pruned_pred=predict(pruned.model,train.norm.df,type = "class")
confusionMatrix(pruned_pred, factor(train.norm.df$outcome))

###evaluate pruned model on valid data
pruned_pred_valid=predict(pruned.model,valid.norm.df,type = "class")

confusionMatrix(pruned_pred_valid, factor(valid.norm.df$outcome))

