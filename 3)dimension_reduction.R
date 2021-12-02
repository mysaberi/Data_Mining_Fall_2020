#full
log_d_r=glm(factor(outcome)~.,data=df[,c(15:43,55)],family=binomial)

summary(log_d_r)
#sookht.p
log_d_r=glm(factor(outcome)~.,data=df[,c(15:42,55)],family=binomial)

summary(log_d_r)

#omit tel
log_d_r=glm(factor(outcome)~.,data=df[,c(15:34,36:42,55)],family=binomial)

summary(log_d_r)
#omit ashpazkhane
log_d_r=glm(factor(outcome)~.,data=df[,c(15:34,36,37,39:42,55)],family=binomial)

summary(log_d_r)
#omit fazelab
log_d_r=glm(factor(outcome)~.,data=df[,c(15:34,36,37,39:41,55)],family=binomial)

summary(log_d_r)


#omit jaro.b
log_d_r=glm(factor(outcome)~.,data=df[,c(15:27,29:34,36,37,39:41,55)],family=binomial)

summary(log_d_r)

#omit cooler.g.s
log_d_r=glm(factor(outcome)~.,data=df[,c(15:27,29:34,36,37,39:40,55)],family=binomial)

summary(log_d_r)

#omit radio
log_d_r=glm(factor(outcome)~.,data=df[,c(15:17,19:27,29:34,36,37,39:40,55)],family=binomial)

summary(log_d_r)

#omit DVD
log_d_r=glm(factor(outcome)~.,data=df[,c(15:17,19,20,22:27,29:34,36,37,39:40,55)],family=binomial)

summary(log_d_r)

#omit motor
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,19,20,22:27,29:34,36,37,39:40,55)],family=binomial)

summary(log_d_r)

#omit yakhchal
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,19,20,22,23,24,26,27,29:34,36,37,39:40,55)],family=binomial)

summary(log_d_r)



#omit tv.r
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,19,22,23,24,26,27,29:34,36,37,39:40,55)],family=binomial)

summary(log_d_r)



#omit mobile
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,19,22,24,26,27,29:34,36,37,39:40,55)],family=binomial)

summary(log_d_r)

#omit cooler.a.s
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,19,22,24,26,27,29:34,36,37,40,55)],family=binomial)

summary(log_d_r)


#omit gaz
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,19,22,24,26,29:34,36,37,40,55)],family=binomial)

summary(log_d_r)



#omit zabt
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,22,24,26,29:34,36,37,40,55)],family=binomial)

summary(log_d_r)



#omit package
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,22,24,26,29:34,36,37,55)],family=binomial)

summary(log_d_r)


#omit cooler.a
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,22,24,26,29:31,33,34,36,37,55)],family=binomial)

summary(log_d_r)


#omit m.zarf
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,22,24,26,29:31,34,36,37,55)],family=binomial)

summary(log_d_r)


#omit panke
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,22,24,26,29,30,34,36,37,55)],family=binomial)

summary(log_d_r)


#omit freeizer
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,22,26,29,30,34,36,37,55)],family=binomial)

summary(log_d_r)


#omit yakhchal.f
log_d_r=glm(factor(outcome)~.,data=df[,c(15,17,22,29,30,34,36,37,55)],family=binomial)

summary(log_d_r)

dim(df[,c(15,17,22,29,30,34,36,37,55)])


df=df[,-c(16,18:21,23:28,31:33,35,38:43)]

write.csv(df,'C:/Users/Saberi/Desktop/df.csv')























