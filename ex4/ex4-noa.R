#Q4
men = c(67, 43, 175, 34, 74, 102, 44, 45, 115, 71)
women = c(132, 84, 88, 207, 103, 97, 183, 159, 181, 214)
meanMen = mean(men)
meanWomen = mean(women)
s2Women = (sd(women)^2)*(10/9)
s2Men = (sd(men)^2)*(10/9)
ttest = t.test(women, men, var.equal = TRUE, alternative = "greater")

#Confidence Interval
womenMinusMen = meanWomen - meanMen
tVal = qt(0.975, 10+10-2)
s_pooled = sqrt(((9*s2Women)+(9*s2Men))/18)
confidence_down = womenMinusMen-tVal*s_pooled*(sqrt((1/10)+(1/10)))
confidence_up = womenMinusMen+tVal*s_pooled*(sqrt((1/10)+(1/10)))

#graph
barplot(c(meanMen, meanWomen), main= "Likes on profile pic on Facebook", names.arg=c("Men", "Women"), ylim=c(0,180))

#permutation
mean_dif = meanWomen - meanMen
my_data <- data.frame( 
  group = rep(c("Woman", "Man"), each = 10),
  likes = c(women, men)
)
my_data2 = my_data

mean_dif_distribution =c()
perm_p_val = c()
for (i in 1:10000){
  my_data2$likes = sample(my_data2$likes)
  temp_mean_dif = mean(my_data2[my_data2$group == "Woman",]$likes) - mean(my_data2[my_data2$group == "Man",]$likes)
  mean_dif_distribution[i] = temp_mean_dif
  perm_p_val[i] = ifelse(temp_mean_dif >= mean_dif, 1, 0)
}
hist(mean_dif_distribution)
abline(v = mean(mean_dif_distribution), col = "red", lwd = 2)
abline(v = mean_dif, col = "blue", lwd = 2)
mean(perm_p_val)


#bootstrapping 

bootstrap_distribution = c()
for (i in 1:10000){
  randRows = sample(length(women), replace = T)
  bootstrap_distribution[i] = mean(women[randRows]) - mean(men[randRows])
}

hist(bootstrap_distribution)
abline(v= mean_dif, col = "blue", lwd=2)
quantile(bootstrap_distribution, c(.025, .975))
