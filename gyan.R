#Analyze and list the platforms with the most "Editor's Choice" awards? 
#data<-read.csv("C:\\Users\\SatyakiBh\\Desktop\\GyanMatrix\\20-years-of-games6b79d29\\ign.csv",header = T)
setwd("<Mention_the_path_of_the_dataset_here>")
data<-read.csv("ign.csv",header = T)
head(data)
summary(data)
lapply(data, class)
'''OUTPUT
$X [1] "integer"; $score_phrase [1] "factor"; $title [1] "factor"; $url [1] "factor"; $platform [1] "factor"; $score [1] "numeric"; $genre [1] "factor"; $editors_choice [1] "factor"; $release_year [1] "integer"; $release_month [1] "integer"; $release_day [1] "integer"; '''

library(sqldf)
data_new=subset(data,data[,"editors_choice"]=='Y') #data[,"editors_choice"=='Y']
ans<-sqldf('select platform, count(editors_choice) as cnt_edi from data_new group by platform order by cnt_edi desc')
ans
#Answer PC is the platform with most editor's choice
barplot(ans$cnt_edi,names.arg = ans$platform)
write.csv(ans,file = "platform_wise_analysis.csv")

#Does number of games by a platform in a given year have any effect on these awards?
#Platform, release_year, Num_games, editor's choice
ans2<-sqldf('select platform, release_year, count(title) as num_games, editors_choice from data group by release_year')
head(ans2)

aov(as.numeric(ans2$editors_choice)~ans2$num_games,data=ans2)
#Residual standard error: 0.421317
#Since, Residual standard error is high, so number of games by a platform in a given year does not have much effect on these awards





#What is Macintosh's average award count? 
data_mac_awd<-subset(data_new,data_new$platform=='Macintosh')
ans3<-nrow(data_mac_awd)/nrow(data)
ans3
#OUTPUT [1] 0.002147651


#What is the optimal month for releasing a game? 
t1<-sqldf('select release_month, count(release_month) as cnt_rm from data group by release_month order by cnt_rm desc')
t2<-sqldf('select release_month, count(release_month) as cnt_rm_yes from data_new group by release_month order by cnt_rm_yes desc')
t3<-sqldf('select t1.release_month, (t2.cnt_rm_yes*1.0/t1.cnt_rm) as avg_success from t1,t2 where t1.release_month=t2.release_month order by avg_success desc')
barplot(t3$avg_success,names.arg = t3$release_month)
write.csv(t3,file="Monthly_success_ratio.csv")
#Answer Month 9 is the optimal month for releasing a game




#Analyze the percentage growth in the gaming industry over the years.
t4<-sqldf('select release_year, count(release_year) as cnt_ry from data group by release_year order by release_year')
prev<-t4[1,2]
per<-c()
for (i in (2:nrow(t4))){
  curr<-t4[i,2]
  pert<-(curr-prev)/prev*100.0
  per<-c(per,pert)
  prev<-curr
}
yr_order<-2:21 #removing the first one as it is an outlier
percentage_growth<-per[2:length(per)]
plot(yr_order,percentage_growth,type = "l")
barplot(percentage_growth,names.arg = c("1996-1997","1997-1998","1998-1999","1999-2000","2000-2001","2001-2002","2002-2003","2003-2004","2004-2005","2005-2006","2006-2007","2007-2008","2008-2009","2009-2010","2010-2011","2011-2012","2012-2013","2013-2014","2014-2015","2015-2016"))
per_gr<-cbind(c("1996-1997","1997-1998","1998-1999","1999-2000","2000-2001","2001-2002","2002-2003","2003-2004","2004-2005","2005-2006","2006-2007","2007-2008","2008-2009","2009-2010","2010-2011","2011-2012","2012-2013","2013-2014","2014-2015","2015-2016"),percentage_growth)
write.csv(per_gr,file="percentage_growth.csv")

###Designing the predictive model
#During 1970 no computer games existed. So, it is an outlier. Need to remove it.
data_mod<-subset(data,data$release_year!=1970)

#URL, row_no, title is unique is every case so removing it
data_mod1<-data_mod[,c("score_phrase","platform", "score", "genre",	"editors_choice","release_year","release_month", "release_day")]
#score_phrase, platform,genre to factor convert
data_mod1$score_phrase_fac<-as.factor(data_mod1$score_phrase)
data_mod1$platform_fac<-as.factor(data_mod1$platform)
data_mod1$genre_fac<-as.factor(data_mod1$genre)
data_mod1$editors_choice_fac<-as.factor(data_mod1$editors_choice)
data_mod1$yr<-data_mod1$release_year-1995 #taking 1995 as reference
#dataframe_for_modelling
df_mod<-data.frame(cbind(data_mod1$score_phrase_fac,data_mod1$platform_fac,data_mod1$score,data_mod1$genre_fac,data_mod1$yr,data_mod1$editors_choice_fac))


library(randomForest)
model<-randomForest(as.factor(df_mod$X6)~.,data=df_mod,type=, importance=T)
model$importance


'''
OUTPUT
             1           2 MeanDecreaseAccuracy MeanDecreaseGini
X1 0.042050429 0.124009315          0.057477974        1153.8634
X2 0.007622197 0.033261296          0.012459098         273.2554
X3 0.135633531 0.509492206          0.206218183        3184.0430
X4 0.001885926 0.006601839          0.002776377         205.4941
X5 0.015011046 0.057975831          0.023118635         400.4079
'''
#Thus the strongest predictor is X3 i.e. Score
