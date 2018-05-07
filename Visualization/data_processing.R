#DATA PREPROCESSING AND CLEANUP

#Remove old data
rm(list=ls())

source("data_functions.R")

#Load dataset
data = read.csv("../new_data/data.csv")

#removes all classes where enrollment is below 10 or NA
data.concise <- data[(data$Enrolled > 10) & !(is.na(data$Enrolled)), ]

#removes classes where there are no grades
data.concise <- data.concise[as.numeric(as.character(data.concise$W)) + 
                               as.numeric(as.character(data.concise$X)) + 
                               as.numeric(as.character(data.concise$I)) < 100 ,]

#removes classes where reported values are not close to 100%
data.concise <- data.concise[as.numeric(as.character(data.concise$A)) + 
                               as.numeric(as.character(data.concise$B)) + 
                               as.numeric(as.character(data.concise$C)) +
                               as.numeric(as.character(data.concise$D)) +
                               as.numeric(as.character(data.concise$F)) +
                               as.numeric(as.character(data.concise$Pass)) +
                               as.numeric(as.character(data.concise$Fail)) +
                               as.numeric(as.character(data.concise$I)) + 
                               as.numeric(as.character(data.concise$W)) + 
                               as.numeric(as.character(data.concise$X)) > 90 ,]

count <- nrow(data.concise)
data.concise[nchar(data.concise$Number) < 4 & nchar(data.concise$Number) > 2,2] <- paste(data.concise[nchar(data.concise$Number) < 4 & nchar(data.concise$Number) > 2, 2], "0", sep="")

#calculate class average for each class
data.concise$ClassAverage <- with(data.concise, ( 
    ((Enrolled * (as.numeric(as.character(A))/100)) 
      / (Enrolled * ((100-(as.numeric(as.character(W))+as.numeric(as.character(I))+as.numeric(as.character(Fail))+as.numeric(as.character(X)))) / 100))) * 4
    + ((Enrolled * (as.numeric(as.character(B))/100)) 
      / (Enrolled * ((100-(as.numeric(as.character(W))+as.numeric(as.character(I))+as.numeric(as.character(Fail))+as.numeric(as.character(X)))) / 100))) * 3
    + ((Enrolled * (as.numeric(as.character(C))/100)) 
      / (Enrolled * ((100-(as.numeric(as.character(W))+as.numeric(as.character(I))+as.numeric(as.character(Fail))+as.numeric(as.character(X)))) / 100))) * 2
    + ((Enrolled * (as.numeric(as.character(D))/100)) 
      / (Enrolled * ((100-(as.numeric(as.character(W))+as.numeric(as.character(I))+as.numeric(as.character(Fail))+as.numeric(as.character(X)))) / 100))) * 1
    + ((Enrolled * (as.numeric(as.character(Pass))/100)) 
      / (Enrolled * ((100-(as.numeric(as.character(W))+as.numeric(as.character(I))+as.numeric(as.character(X))))/100))) * 4 
  ))

#use less significant digits
for (row in data.concise) {
  data.concise$ClassAverage <- round(data.concise$ClassAverage, digits=2)
}

#build list of departments and corresponding college
deptList = data.frame(data.concise$Department, data.concise$College)
deptList = unique(deptList)
collList = data.frame(data.concise$College)
collList = unique(collList)
