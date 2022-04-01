#ex0

studentsCourses <- data.frame(maths=c(1,1,0,0),philosphy=c(1,1,0,0),biology=c(0,1,0,1),photography=c(1,1,1,1))
rownames(studentsCourses) = c("bob","hannah","babette","arnold")

func1 <- function(dataList){
  sumDate = 0 
  for(i in 1:10000){
    sumDate = sumDate + sample(dataList,1)
  }
  return(sumDate)
}

v3 <- apply(studentsCourses,1,func1)
studentsCourses$sim_suc_rate <- c(v3/10000)
#ex0

studentsCourses <- data.frame(maths=c(1,1,0,0),philosphy=c(1,1,0,0),biology=c(0,1,0,1),photography=c(1,1,1,1))
rownames(studentsCourses) = c("bob","hannah","babette","arnold")

func1 <- function(dataList){
  sumDate = 0 
  for(i in 1:10000){
    sumDate = sumDate + sample(dataList,1)
  }
  return(sumDate)
}

v3 <- apply(studentsCourses,1,func1)
studentsCourses$sim_suc_rate <- c(v3/10000)

v4 <- apply(studentsCourses[,1:4],1,sum)/4
studentsCourses$suc_rate <- c(v4)

