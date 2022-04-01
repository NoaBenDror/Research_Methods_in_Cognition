#### class exercise ###

#Q1

N1=34; N2=38

time_sd_1=4.4; time_sd_2=11.2
time_unbias_var_1=(time_sd_1^2)*(N1/(N1-1))
time_unbias_var_2=(time_sd_2^2)*(N2/(N2-1))
time_avg_1=10.8; time_avg_2=23.2

means=c(time_avg_1,time_avg_2)

N=c(N1,N2)


df = data.frame(means, groups = c("Time limit","no Time limit"))
graph1 = ggplot(df, aes(x=groups, fill=groups, y=means))
graph1 + geom_bar(stat="identity")


#Q2
ChickWeight
library(plyr)
last_day=ChickWeight[ChickWeight$Time==max(ChickWeight$Time),]
chick_weight_summ <- ddply(last_day, "Diet", summarise,
                           N = length(weight),
                           mean = mean(weight),
                           sd_estimator = sd(weight))
head(chick_weight_summ)
data_Diet_1=last_day[last_day$Diet==1,]$weight
data_Diet_2=last_day[last_day$Diet==2,]$weight


df2 = data.frame(weight = c(data_Diet_1, data_Diet_2), diet = c('diet1', 'diet2'))
graph2 = ggplot(df2, aes(x = diet, y = weight, fill = diet ))
graph2 + geom_boxplot()



