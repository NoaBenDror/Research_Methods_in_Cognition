#Q1

x=data.frame(InsectSprays)
ttest = t.test(x[x$spray=="A",]$count, x[x$spray=="B",]$count, var.equal = TRUE, alternative = "greater")
anova_x = aov(count~spray, data=x)
summary(anova_x)

#Q2
my_data=read.csv("C:\\Users\\206mi\\Desktop\\ex5.csv",,header = TRUE)
my_data
my_data$sport_type=factor(my_data$sport_type,
                          levels = 1:4,
                          labels = c("Basketball", "Football",
                                     "bicycle","Running"))
my_data

#ANOVA test
anova_sport=aov(lactic_acid ~ sport_type, data=my_data)
summary(anova_mosq)

levels(my_data$sport_type)

#the order here is according to the levels order
weights_cont1=c(-1,-1,1,1) 
weights_cont2=c(-1,1,0,0) 
weights_cont3=c(0,0,-1,1) 

install.packages("lsmeans")
library(lsmeans)
Weights=list(weights_cont1,weights_cont2,weights_cont3)
leastsquare = lsmeans(anova_sport,"sport_type")
leastsquare
contrast(leastsquare, Weights, adjust="none") 
