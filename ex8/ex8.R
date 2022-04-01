#Q1
#A
body_temp_x = c(36.5, 36.1, 35.9, 35.6, 37.2, 36.5, 37.4, 35.4, 37.8, 39, 36.7, 37.2, 37, 36.9)
social_warmness_y = c(4, 5.3, 6.8, 3.5, 5, 3.6, 4, 6.8, 4, 1.4, 3.8, 4.3, 5, 3)
my_data_frame = data.frame("body_temp"=body_temp_x,"social_warmness"= social_warmness_y)
plot(body_temp_x, social_warmness_y, main="scatter plot of body temperature and social warmness", xlab="body temperature", ylab="social warmness")

#B
R_2 = cor(social_warmness_y,body_temp_x)^2

#C
corr = cor.test(body_temp_x, social_warmness_y)
corr$p.value

#D
regression = function(X,Y,x)
{
  b = cov(X,Y)/var(X)
  a = mean(Y)-b*mean(X)
  return (a+b*x)
}

#E
regression(body_temp_x, social_warmness_y, 1)

#F
linear1 = lm(social_warmness_y ~ body_temp_x)
summary(linear1)

#G
library("ggpubr")
x11()
ggscatter(my_data_frame, x="body_temp", y="social_warmness", 
          add = 'reg.line', conf.int = T, main = " The realtion between body temperature and social warmness",
          cor.coef = TRUE, cor.method = "pearson", xlab="body temperature", ylab="social warmness")

#H
shapiro.test(social_warmness_y)
my_med = median(body_temp_x)
var.test(subset(my_data_frame, body_temp < my_med)$social_warmness,
         subset(my_data_frame, body_temp > my_med)$social_warmness)

test1 = t.test(subset(my_data_frame, body_temp < my_med)$social_warmness,
               subset(my_data_frame, body_temp > my_med)$social_warmness,
               var.equal=TRUE, alternative = "two.sided")

#Q2.1
my_data=read.csv("C:\\Users\\206mi\\Desktop\\forklift_acoustic.csv",header = TRUE)
View(my_data)
colnames(my_data)
attach(my_data)
linear=lm(annoyance ~ fluc)
summary(linear)

x11()
plot(fluc, annoyance,
     pch = 16, cex = 1.1, col = "blue", 
     main = "The relation between the annoyance level \n and the fluctuations strength in vasil  ",
     xlab = "fluctuations strength(vasil)", ylab = "level of annoyance",
     )
abline(linear)

#Q2.2
my_data$measurement_time = 0
my_data[c(26:50),]$measurement_time = 1

linear2=lm(annoyance ~ measurement_time, my_data)
summary(linear2)

