library(psych)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(MASS)

###first plot 

par(mfcol = c(2,1))
parcoord(df[df$outcome == 0, c(2,4,12,44:54)],main="خانوار های کم درامد") 
parcoord(df[df$outcome == 1, c(2,4,12,44:54)],main="خانوار های پر درامد")
par(mfcol = c(1,1))
###2- outcome
ggplot(df) +
  geom_bar(aes(x=factor(outcome)),fill = "darkblue")+ 
  scale_x_discrete(labels = c('خانوار کم درآمد',"خانوار پر درآمد"))+
  ggtitle('نمودار فراوانی سطح درآمد خانوار')+
  labs(x='سطح درآمد خانوار',y="تعداد")
###3- Gender
ggplot(df[df$outcome==0,]) +
  geom_bar(aes(x=factor(Gender)),fill = "cornflowerblue")+ 
  scale_x_discrete(labels = c('زن','مرد'))+
  ggtitle('نمودار فراوانی خانوار های کم درامد بر اساس جنسیت سرپرست خانوار')+
  labs(x='جنسیت',y="تعداد")

ggplot(df[df$outcome==1,]) +
  geom_bar(aes(x=factor(Gender)),fill = "dodgerblue3")+ 
  scale_x_discrete(labels = c('زن','مرد'))+
  ggtitle('نمودار فراوانی خانوار های پر درامد بر اساس جنسیت سرپرست خانوار')+
  labs(x='جنسیت',y="تعداد")


###4-Savad
ggplot(df[df$outcome==0,]) +
  geom_bar(aes(x=factor(Savad)),fill = "lightpink")+ 
  scale_x_discrete(labels = c("فاقد سواد","با سواد"))+
  ggtitle('نمودار فراوانی خانوار های کم درامد بر اساس سطح سواد سرپرست خانوار')+
  labs(x='سطح سواد',y="تعداد")

ggplot(df[df$outcome==1,]) +
  geom_bar(aes(x=factor(Savad)),fill = "hotpink2")+ 
  scale_x_discrete(labels = c("فاقد سواد","با سواد"))+
  ggtitle('نمودار فراوانی خانوار های پر درامد بر اساس سطح سواد سرپرست خانوار')+
  labs(x='سطح سواد',y="تعداد")


###5-InEdu
ggplot(df[df$outcome==0,]) +
  geom_bar(aes(x=factor(InEdu)),fill = "cornflowerblue")+ 
  scale_x_discrete(labels = c('در حال تحصیل نیست','در حال تحصیل است'))+
  ggtitle('نمودار فراوانی خانوار های کم درامد بر اساس محصل بودن سرپرست خانوار')+
  labs(x='وضعیت ادامه تحصیل',y="تعداد")

ggplot(df[df$outcome==1,]) +
  geom_bar(aes(x=factor(InEdu)),fill = "dodgerblue3")+ 
  scale_x_discrete(labels = c('در حال تحصیل نیست','در حال تحصیل است'))+
  ggtitle('نمودار فراوانی خانوار های پر درامد بر اساس محصل بودن سرپرست خانوار')+
  labs(x='وضعیت ادامه تحصیل',y="تعداد")
q=df[,c('InEdu',"outcome")]


###6-Madrak

ggplot(df[df$outcome==0,]) +
  geom_bar(aes(x=factor(Madrak)),fill = "olivedrab2")+ 
  scale_x_discrete(labels = c("فاقد مدرک","ابتدایی","راهنمایی","دبیرستان","دیپلم","فوق دیپلم","لیسانس","ارشد","سایر و غیررسمی"))+
  ggtitle('نمودار فراوانی خانوار های کم درامد بر اساس آخرین مدرک تحصیلی سرپرست خانوار')+
  labs(x='مدرک تحصیلی',y="تعداد")

ggplot(df[df$outcome==1,]) +
  geom_bar(aes(x=factor(Madrak)),fill = "olivedrab3")+ 
  scale_x_discrete(labels = c("فاقد مدرک","ابتدایی","راهنمایی","دیپلم","فوق دیپلم","لیسانس","ارشد"))+
  ggtitle('نمودار فراوانی خانوار های پر درامد بر اساس آخرین مدرک تحصیلی سرپرست خانوار')+
  labs(x='مدرک تحصیلی',y="تعداد")

###7-Faaliat
ggplot(df[df$outcome==0,]) +
  geom_bar(aes(x=factor(Faaliat)),fill = "gold")+ 
  scale_x_discrete(labels = c("شاغل","بیکار","دارای درامد بدون کار","خانه دار","سایر"))+
  ggtitle("نمودار فراوانی خانوار های کم درامد بر اساس وضعیت شغلی سرپرست خانوار")+
  labs(x='وضعیت فعالیت',y="تعداد")

ggplot(df[df$outcome==1,]) +
  geom_bar(aes(x=factor(Faaliat)),fill = "darkorange")+ 
  scale_x_discrete(labels = c("شاغل","بیکار","دارای درامد بدون کار","خانه دار"))+
  ggtitle("نمودار فراوانی خانوار های پر درامد براساس وضعیت شغلی سرپرست خانوار")+
  labs(x='وضعیت فعالیت',y="تعداد")

###8-nts
ggplot(df[df$outcome==0,]) +
  geom_bar(aes(x=factor(n.t.s)),fill = "darkorchid1")+ 
  scale_x_discrete(labels = c("ملکی عرصه و اعیان","اجاری","رهن","در برابر خدمت","رایگان"))+
  ggtitle("نمودار فراوانی خانوار های کم درامد بر اساس نحوه تصور ملک توسط خانوار")+
  labs(x='نحوه تصرف ملک',y="تعداد")

ggplot(df[df$outcome==1,]) +
  geom_bar(aes(x=factor(n.t.s)),fill = "darkorchid4")+ 
  scale_x_discrete(labels = c("ملکی عرصه و اعیان","اجاری","رهن","در برابر خدمت","رایگان"))+
  ggtitle("نمودار فراوانی خانوار های پر درامد براساس نحوه تصور ملک تصور خانوار")+
  labs(x='نحوه تصرف ملک',y="تعداد")

###9-internet
ggplot(df[df$outcome==0,]) +
  geom_bar(aes(x=factor(internet)),fill = "darkseagreen1")+ 
  scale_x_discrete(labels = c("دسترسی ندارد","دسترسی دارد"))+
  ggtitle("نمودار فراوانی خانوار های کم درامد بر اساس دسترسی به اینترنت")+
  labs(x='دسترسی به اینترنت',y="تعداد")

ggplot(df[df$outcome==1,]) +
  geom_bar(aes(x=factor(internet)),fill = "darkseagreen")+ 
  scale_x_discrete(labels = c("دسترسی ندارد","دسترسی دارد"))+
  ggtitle("نمودار فراوانی خانوار های پر درامد براساس دسترسی به اینترنت")+
  labs(x='دسترسی به اینترنت',y="تعداد")

###10-mobile
ggplot(df[df$outcome==0,]) +
  geom_bar(aes(x=factor(mobile)),fill = "plum2")+ 
  scale_x_discrete(labels = c("موبایل ندارد","موبایل دارد"))+
  ggtitle("نمودار فراوانی خانوار های کم درامد بر اساس داشتن تلفن همراه")+
  labs(x='داشتن موبایل',y="تعداد")

ggplot(df[df$outcome==1,]) +
  geom_bar(aes(x=factor(mobile)),fill = "orchid")+ 
  scale_x_discrete(labels = c("موبایل ندارد","موبایل دارد"))+
  ggtitle("نمودار فراوانی خانوار های پر درامد براساس داشتن نلفن همراه")+
  labs(x='داشتن موبایل',y="تعداد")
###########
options(scipen=10000)
ggplot(df)+
  geom_boxplot(aes(x=factor(outcome),y=H_Khorakivadokhani/10,fill=factor(outcome)),
                    outlier.color = 'plum3',outlier.size = 2)+
               labs(x='سطح درامد خانوار',y="هزینه ماهانه خورد و خوراک و دخانیات به تومان")+
                scale_x_discrete(labels = c("خانوار کم درامد","خانوار پر درامد"))+
               ggtitle("نمودار جعبه ای هزینه خورد و خوراک و دخانیات نسبت به سطح درامد خانوار")
             
ggplot(df)+
  geom_boxplot(aes(x=factor(outcome),y=H_Maskan/10,fill=factor(outcome)),
        outlier.color = 'plum3',outlier.size = 2)+
  labs(x='سطح درامد خانوار',y="هزینه ماهیانه مسکنبه تومان")+
  scale_x_discrete(labels = c("خانوار کم درامد","خانوار پر درامد"))+
  ggtitle("نمودار جعبه ای هزینه مسکن نسبت به سطح درامد خانوار")

###scatter plot

ggplot(df,aes(x=Age,y=H_Tafrihat/10,color=factor(outcome, labels = c("خانوار کم درامد","خانوار پر درامد"))))+
  geom_point()+
  labs(x='سن سرپرست خانوار',y="هزینه ماهیانه تفریخات به توامن",color = "سطح درامد")+
  ggtitle("نمودار پراکنش سن سرپرست خانوار و هزینه ماهانه تفریحات")


ggplot(df,aes(y=Tedad.a,x=S.Z,color=factor(outcome, labels = c("خانوار کم درامد","خانوار پر درامد"))))+
  geom_point()+
  labs(x='سطح زیربنای اقامتگاه',y="تعداد اعضای خانوار",color = "سطح درامد")+
  ggtitle("نمودار پراکنش سطح زیربنای اقامتگاه و تعداد اعضای خانوار")+
  coord_cartesian(xlim =c(0, 310), ylim = c(0, 7))+
  scale_y_continuous("تعداد اعضای خانوار",1:7)

###pie plot of cities

pie0<-df[df['outcome']==0,] %>%
  group_by(region.code) %>%
  summarise(count = n())

pie1<-df[df['outcome']==1,]%>%
  group_by(region.code) %>%
  summarise(count = n())

ggplot(pie0,aes(x="",y=count,fill=region.code)) +
  geom_bar(width = 1, stat = "identity", colour = "black")+
  coord_polar("y")+
  scale_fill_brewer(palette ="Set3")+
  theme_void()+
  ggtitle('نمودار دایره ای مناطق محل سکونت خانوار های کم درامد')
ggplot(pie1,aes(x="",y=count,fill=region.code)) +
  geom_bar(width = 1, stat = "identity", colour = "black")+
  coord_polar("y")+
  scale_fill_brewer(palette ="Set3")+
  theme_void()+
  ggtitle('نمودار دایره ای مناطق محل سکونت خانوار های پر درامد')



pairs.panels(df[,c(4,44,48,52)], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE) # show correlation ellipses


###correlation plot by heatmap

ggcorrplot(round(cor(df[, c(2,4,11,12,44:54)]),2),lab=T)

