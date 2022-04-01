#Q1
load("C://Users//206mi//Desktop//population_data.R")
library(ggplot2)

#Q1.1
hist(population, breaks = 50)
mean(population) 
sd(population) 


#Q1.2
sample_3_1 = sample(x=population,3)
hist(sample_3_1,breaks = 50)
mean(sample_3_1) 
sd(sample_3_1) 
sample_3_2 = sample(x=population,3)
hist(sample_3_2,breaks = 50)
mean(sample_3_2) 
sd(sample_3_2)
sample_3_3 = sample(x=population,3)
hist(sample_3_3,breaks = 50)
mean(sample_3_3) 
sd(sample_3_3) 

sample_30_1 = sample(x=population,30)
hist(sample_30_1,breaks = 50)
mean(sample_30_1) 
sd(sample_30_1) 
sample_30_2 = sample(x=population,30)
hist(sample_30_2,breaks = 50)
mean(sample_30_2) 
sd(sample_30_2)
sample_30_3 = sample(x=population,30)
hist(sample_30_3,breaks = 50)
mean(sample_30_3) 
sd(sample_30_3) 


sample_1000_1 = sample(x=population,1000)
hist(sample_1000_1,breaks = 50)
mean(sample_1000_1) 
sd(sample_1000_1) 
sample_1000_2 = sample(x=population,1000)
hist(sample_1000_2,breaks = 50)
mean(sample_1000_2) 
sd(sample_1000_2)
sample_1000_3 = sample(x=population,1000)
hist(sample_1000_3,breaks = 50)
mean(sample_1000_3) 
sd(sample_1000_3) 

#Q1.3
f1 = function(n) {

  sampleDist=c()
  for(i  in (1:5000)){
    sampleTake=sample(population,n)
    sampleDist=rbind(sampleDist,mean(sampleTake))
  }
  return(sampleDist)
}
result= f1(1000)
mean(result)
sd(result)
hist(result,breaks = 50)


#Q1.4
f2 = function(n) {
  sampleDist=c()
  for(i  in (1:5000)){
    sampleTake=sample(population,n)
    sampleDist=rbind(sampleDist,min(sampleTake))
  }
  return(sampleDist)
}

hist(f2(1000),breaks = 50)


iqDataFrame= read.csv("C:\\Users\\noabe\\Documents\\iq.csv", header=TRUE)
#Q2.1
myfunc1 <- function(data1)
{
  meanx = mean(data1[,2])
  sum = 0
  for (i in 1:nrow(data1)){
    sum = sum + (data1[i,2]-meanx)^2
  }
  sum = sum/ (nrow(data1)-1)
  return(sum)
}

#Q2.2
myfunc2 <- function(data2){
  return((myfunc1(data2))^0.5)
}

#Q2.3
meanx = mean(iqDataFrame[,2])
standardScore= c()
for (i in 1:nrow(iqDataFrame)){
  standardScore[i] = (iqDataFrame[i,2]-meanx)/sd(iqDataFrame[,2])
}
meanz = mean(standardScore)
sdz = sd(standardScore)
result = c(meanz, sdz, standardScore)

#Q2.4
graphOriginals = ggplot(iqDataFrame, aes(x=IQ1))
graphOriginals + geom_histogram(binwidth = 1, fill="skyblue", color="royalblue")

#Q2.5
sScoreDataFrame = data.frame(Sscore=standardScore)
graphSscore = ggplot(q42_df, aes(x=standardScore))
graphSscore + geom_histogram(binwidth =0.01, fill="skyblue", color="royalblue")

#Q5
q5 = pnorm(0.46, 0.5, sqrt(0.04))


