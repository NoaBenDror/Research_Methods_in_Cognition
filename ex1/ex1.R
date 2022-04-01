#Q1
barplot(c(rowSums(WorldPhones)), main= "Number of phones in the world, years 1951-1961", names.arg=c("1951", "1956", "1957", "1958", "1959", "1960", "1961"), xlab = "Year", ylab = "Number of phones")
v <- c(WorldPhones[,3])
t <- c(WorldPhones[,4])
years = c("1951", "1956", "1957", "1958", "1959", "1960", "1961")

plot(years,v,type = "o",col = "red", xlab = "Year", ylab = "Number of phones", 
     main = "Asia and Sout-America")
legend("topleft", legend=c("Sout-America", "Asia"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
lines(years,t, type = "o", col = "blue")

#Q2
barplot(c(42790,30232), main="Average yearly income", names.arg = c("College graduate", "High school graduate"), ylim= c(0,50000), ylab= "Income in USD", xlab= "Education", col=c("pink","darkcyan"))

barplot(men, main="Average salary for men undergraduates ", names.arg=education, xlab="Field", ylab = "Salary in NIS", ylim=c(0,14000), cex.name =0.75)


#Q3
dietsDataFrame = read.csv("C:\\Users\\206mi\\Desktop\\diets.csv", header=TRUE)
dietsDataFrame$treatment= factor(dietsDataFrame$treatment, levels=c(0,1,2), labels=c("control", "A","B"))
summary(dietsDataFrame)


sbFemale = subset(dietsDataFrame, sex=='F')
meanFemale = mean(sbFemale$ï..age)
sbMale = subset(dietsDataFrame, sex=='M')
meanMale = mean(sbMale$ï..age)

sbChangeWeight= subset(dietsDataFrame, treatment=='control')
changeWeight = sbChangeWeight$weight.after -sbChangeWeight$weight.before 
meanChangeWeightControl = mean(abs(changeWeight))

sbChangeWeight= subset(dietsDataFrame, treatment=='A')
changeWeight = sbChangeWeight$weight.after -sbChangeWeight$weight.before 
meanChangeWeightA = mean(abs(changeWeight))

sbChangeWeight= subset(dietsDataFrame, treatment=='B')
changeWeight = sbChangeWeight$weight.after -sbChangeWeight$weight.before 
meanChangeWeightB = mean(abs(changeWeight))

dietsDataFrame$bmiBefore = dietsDataFrame$weight.before/((dietsDataFrame$height/100)^2)
dietsDataFrame$bmiAfter = dietsDataFrame$weight.after/((dietsDataFrame$height/100)^2)

dietsDataFrame$smoking.category= cut(dietsDataFrame$smoking, c(-Inf,1,6,21,Inf),labels = c("none","light","medium","heavy"))

boxplot(weight.after ~ treatment, data=dietsDataFrame, ylim=c(0,150), main="Diets effectiveness", xlab= "Treatment", ylab= "Weight after treatment")

