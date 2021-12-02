library(dplyr)
library(tidyr)
library(ggcorrplot)
library(MASS)
library(naniar)
df <- read.csv("C:/Users/Saberi/Desktop/New folder/data mining/final project/Data-0.csv")
#convert na char to na
df[ df == "NA" ] <- NA
#######
#missing
vis_miss(df)

miss=df%>%
  miss_var_summary()
miss_var_summary(data = df)
#######
#cleaning-a
omit1=c(21,35,38,46,47)

#cleaning-b

a=data.frame(t(df[,-omit1] %>%
                 summarise_all(list(~n_distinct(.)))))
                    
colnames(a)='number_of_unique_values'

omit2=c(2,39,40)

#cleaning-c
df[,16:50]=apply(df[,16:50], 2,function(y){replace_na(y,replace=0)})


#cleaning-d
df[,c(55,56,58,59,60,61,62,63,64)]=apply(df[,c(55,56,58,59,60,61,62,63,64)], 2,function(y){replace_na(y,replace=0)})
#cleaning-e \ create outcome

b=df[is.na(df$D_Yarane),65:68]
#So we don't have any record which is zero in all income sources

y=rowSums(df[,65:68],na.rm = TRUE)

outcome=0
for(i in 1:length(y)){
  outcome[i]=ifelse(y[i]>quantile(y,0.7),1,0)
}

df=data.frame(df,outcome)
omit3=c(65:68)

#########cleaning-f
#T.shaghel
w=df[,c(3,9,10,69)]%>%
  filter(is.na(T.shaghel))%>%
  group_by(Faaliat,Tedad.a)%>%
  summarize(number_of_NA_records=n())



df[is.na(df$D_Azad)&is.na(df$D_Mozd),]=df%>%
  filter( is.na(D_Mozd)&is.na( D_Azad==1 ))%>%
  mutate(T.shaghel = replace(T.shaghel, which(is.na(T.shaghel) ), 0))


df[!is.na(df$D_Azad)|!is.na(df$D_Mozd),]=df%>%
  filter( !is.na(D_Mozd)|!is.na( D_Azad==1 ))%>%
  mutate(T.shaghel = replace(T.shaghel, which(is.na(T.shaghel) ), 1))

#Masleh
df[,c(14,15)]%>%
  group_by(N.S,Masleh)%>%
  summarize(n=n())

df[is.na(df$Masleh),'Masleh']=8
# madrak va inedu

df[,c(6:8)]%>%
  group_by(Savad,InEdu,Madrak)%>%
  summarize(n=n())


df[is.na(df$InEdu),'InEdu']=2
df[is.na(df$Madrak),'Madrak']=0

##########
#cleaning-g

df[df$Gender==2,"Gender"]=0
df[df$Savad ==2,"Savad"]=0
df[df$InEdu==2,"InEdu"]=0

df[df$sookht.p==1,"sookht.p"]=0
df[df$sookht.p==4,"sookht.p"]=1
df[df$sookht.g==11,"sookht.g"]=0
df[df$sookht.g==14,"sookht.g"]=1
df[df$sookht.ab==21,"sookht.ab"]=0
df[df$sookht.ab==24,"sookht.ab"]=1
omit4=c(52,53)
###aplly all the ommiting on the data
df=df[,-c(omit1,omit2,omit3,omit4)]
#cleaning-final part- creating regions
address=df['Address']%>%
   separate(Address, into = c('WASTE', 'LEFTOVER'), sep = 3)%>%
   separate(LEFTOVER, into = c('region-code', 'Leftover'), sep = 2)

df=data.frame(address$`region-code`,df[,-1])
names(df)[1]='region.code'
vis_miss(df)
############

