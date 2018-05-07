#FUNCTIONS TO ENABLE EASY RETRIEVAL OF SPECIFIC DATA SETS FOR VISUALIZATION

#UNIVERSITY WIDE
get_Grades_Colleges <- function(data.concise) {
  library(reshape2)
  unique_colleges <- unique(data.concise$College)
  unique_terms <- unique(data.concise$Term)
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = length(levels(unique_colleges))))
  colnames(points.data) <- replace(levels(unique_colleges), length(levels(unique_colleges)), "Terms")
  

  #calculates class average for every college each term
  b <- 1
  c <- 1
  for (i in unique_colleges) {
    
    for (j in unique_terms) {
      sum_enrolled <- sum(data.concise[data.concise$College == i & data.concise$Term == j,5])
      #sum_averages <- sum(data.concise[data.concise$College == i & data.concise$Term == j,19])
      #checks to make sure there is at least 1 person enrolled
      if (sum_enrolled > 0) {
        sum.weighted <- sum(data.concise[data.concise$College == i & data.concise$Term == j,19] *
                              data.concise[data.concise$College == i & data.concise$Term == j,5])
        
        #calculate class average
        points.data[b, c] <- 
          sum.weighted / sum_enrolled

        points.data[b, length(levels(unique_colleges))] <- j
      }
      b <- b + 1
      
      
      #}
    }
    c <- c + 1
    b <- 1
  }
  

  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}
get_Enrollment_Colleges <- function(data.concise) {
  library(reshape2)
  unique_colleges <- unique(data.concise$College)
  unique_terms <- unique(data.concise$Term)
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = length(levels(unique_colleges))))
  colnames(points.data) <- replace(levels(unique_colleges), length(levels(unique_colleges)), "Terms")
  
  
  #calculates enrollment for every college each term
  b <- 1
  c <- 1
  for (i in unique_colleges) {
    
    for (j in unique_terms) {
      sum_enrolled <- sum(data.concise[data.concise$College == i & data.concise$Term == j,5])
      #sum_averages <- sum(data.concise[data.concise$College == i & data.concise$Term == j,19])
      #checks to make sure there is at least 1 person enrolled
      #if (sum_enrolled > 0) {
        #sum.weighted <- sum(data.concise[data.concise$College == i & data.concise$Term == j,19] *
                              #data.concise[data.concise$College == i & data.concise$Term == j,5])
        
        #calculate enrollment
        points.data[b, c] <- sum_enrolled
        
        points.data[b, length(levels(unique_colleges))] <- j
      #}
      b <- b + 1
      
      
      #}
    }
    c <- c + 1
    b <- 1
  }
  
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}

get_Grades_Schools <- function(data.concise) {
  library(reshape2)
  unique_departments <- unique(data.concise$Department)
  unique_terms <- unique(data.concise$Term)
  
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = length(levels(unique_departments))))
  colnames(points.data) <- replace(levels(unique_departments), length(levels(unique_departments)), "Terms")

  #calculates class average for every college each term
  b <- 1
  c <- 1
  for (i in unique_departments) {
    
    for (j in unique_terms) {
      sum_enrolled <- sum(data.concise[data.concise$Department == i & data.concise$Term == j,5])
      sum_averages <- sum(data.concise[data.concise$Department == i & data.concise$Term == j,19])
      #checks to make sure there is at least 1 person enrolled
      if (sum_enrolled > 0) {
        sum.weighted <- sum(data.concise[data.concise$Department == i & data.concise$Term == j,19] *
                              data.concise[data.concise$Department == i & data.concise$Term == j,5])
        
        #calculate class average
        points.data[b, c] <- 
          sum.weighted / sum_enrolled
        
        points.data[b, length(levels(unique_departments))] <- j
      }
      b <- b + 1
      
      
      #}
    }
    c <- c + 1
    b <- 1
  }
  
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}
get_Enrollment_Schools <- function(data.concise) {
  library(reshape2)
  unique_departments <- unique(data.concise$Department)
  unique_terms <- unique(data.concise$Term)
  
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = length(levels(unique_departments))))
  colnames(points.data) <- replace(levels(unique_departments), length(levels(unique_departments)), "Terms")
  
  #calculates class average for every college each term
  b <- 1
  c <- 1
  for (i in unique_departments) {
    
    for (j in unique_terms) {
      sum_enrolled <- sum(data.concise[data.concise$Department == i & data.concise$Term == j,5])
      #sum_averages <- sum(data.concise[data.concise$Department == i & data.concise$Term == j,19])
      #checks to make sure there is at least 1 person enrolled
      #if (sum_enrolled > 0) {
        #sum.weighted <- sum(data.concise[data.concise$Department == i & data.concise$Term == j,19] *
                              #data.concise[data.concise$Department == i & data.concise$Term == j,5])
        
        #calculate class average
        points.data[b, c] <- sum_enrolled
        
        points.data[b, length(levels(unique_departments))] <- j
      #}
      b <- b + 1
      
      
      #}
    }
    c <- c + 1
    b <- 1
  }
  
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}

get_Grades_Majors <- function(data.concise) {
  library(reshape2)
  
  unique_majors <- unique(data.concise$Course)
  unique_terms <- unique(data.concise$Term)
  
  
  #creates data frame for each major and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = length(levels(unique_majors))))
  colnames(points.data) <- replace(levels(unique_majors), length(levels(unique_majors)), "Terms")
  

  #calculates class average for every major each term
  b <- 1
  c <- 1
  for (i in unique_majors) {
    
    for (j in unique_terms) {
      sum_enrolled <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,5])
      sum_averages <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19])
      #checks to make sure there is at least 1 person enrolled
      if (sum_enrolled > 0) {
        sum.weighted <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19] *
                              data.concise[data.concise$Course == i & data.concise$Term == j,5])
        
        #calculate class average
        points.data[b, c] <- 
          sum.weighted / sum_enrolled
        
        points.data[b, length(levels(unique_majors))] <- j
      }
      b <- b + 1
      
      
      #}
    }
    c <- c + 1
    b <- 1
  }
  
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}
get_Enrollment_Majors <- function(data.concise) {
  library(reshape2)
  
  unique_courses <- unique(data.concise$Course)
  unique_terms <- unique(data.concise$Term)
  
  
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = length(levels(unique_courses))))
  colnames(points.data) <- replace(levels(unique_courses), length(levels(unique_courses)), "Terms")
  
  
  #calculates class average for every college each term
  b <- 1
  c <- 1
  for (i in unique_courses) {
    
    for (j in unique_terms) {
      sum_enrolled <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,5])
      #sum_averages <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19])
      #checks to make sure there is at least 1 person enrolled
      #if (sum_enrolled > 0) {
        #sum.weighted <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19] *
                              #data.concise[data.concise$Course == i & data.concise$Term == j,5])
        
        #calculate class average
        points.data[b, c] <- sum_enrolled
        
        points.data[b, length(levels(unique_courses))] <- j
      #}
      b <- b + 1
      
      
      #}
    }
    c <- c + 1
    b <- 1
  }
  
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}


#SPECIFIC
get_Grades_Single_Major <- function(data.concise, major_string) {
  library(reshape2)
  
  #unique_courses <- unique(data.concise$Course)
  unique_terms <- unique(data.concise$Term)
  
  
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = 2))
  colnames(points.data) <- replace(c(major_string, " "), 2, "Terms")
  
  #calculates class average for every college each term
  b <- 1
  c <- 1
  #for (i in unique_courses) {
  
  for (j in unique_terms) {
    sum_enrolled <- sum(data.concise[data.concise$Course == major_string & data.concise$Term == j,19])
    #sum_averages <- sum(data.concise[data.concise$Course == major_string & data.concise$Term == j,19])
    #checks to make sure there is at least 1 person enrolled
    #if (sum_enrolled > 0) {
    sum.weighted <- sum(data.concise[data.concise$Course == major_string & data.concise$Term == j,19] *
      data.concise[data.concise$Course == major_string & data.concise$Term == j,5])
    
    #calculate class average
    points.data[b, c] <- sum.weighted / sum_enrolled
    
    points.data[b, c+1] <- j
    #}
    b <- b + 1
    
    
    #}
  }
  #c <- c + 1
  #b <- 1
  #}
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}
get_Enrollment_Single_Major <- function(data.concise, major_string) {
  library(reshape2)
  
  #unique_courses <- unique(data.concise$Course)
  unique_terms <- unique(data.concise$Term)
  
  
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = 2))
  colnames(points.data) <- replace(c(major_string, " "), 2, "Terms")
  
  #calculates class average for every college each term
  b <- 1
  c <- 1
  #for (i in unique_courses) {
    
    for (j in unique_terms) {
      sum_enrolled <- sum(data.concise[data.concise$Course == major_string & data.concise$Term == j,5])
      #sum_averages <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19])
      #checks to make sure there is at least 1 person enrolled
      #if (sum_enrolled > 0) {
      #sum.weighted <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19] *
      #data.concise[data.concise$Course == i & data.concise$Term == j,5])
      
      #calculate class average
      points.data[b, c] <- sum_enrolled
      
      points.data[b, c+1] <- j
      #}
      b <- b + 1
      
      
      #}
    }
    #c <- c + 1
    #b <- 1
  #}
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}

get_Grades_Single_Course <- function(data.concise, major_string, num_string) {
  library(reshape2)
  
  #unique_courses <- unique(data.concise$Course)
  unique_terms <- unique(data.concise$Term)
  
  
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = 2))
  colnames(points.data) <- replace(c(paste(major_string, num_string, sep=" "), " "), 2, "Terms")
  
  #calculates class average for every college each term
  b <- 1
  c <- 1
  #for (i in unique_courses) {
  
  for (j in unique_terms) {
    sum_enrolled <- sum(data.concise[data.concise$Course == major_string & 
                                       data.concise$Term == j &
                                       data.concise$Number == num_string,5])
    #sum_averages <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19])
    #checks to make sure there is at least 1 person enrolled
    #if (sum_enrolled > 0) {
    sum.weighted <- sum(data.concise[data.concise$Course == major_string & 
                                       data.concise$Term == j &
                                       data.concise$Number == num_string,19] *
                        data.concise[data.concise$Course == major_string & 
                                       data.concise$Term == j &
                                       data.concise$Number == num_string,5])
    
    #calculate class average
    points.data[b, c] <- sum.weighted / sum_enrolled
    
    points.data[b, c+1] <- j
    #}
    b <- b + 1
    
    
    #}
  }
  #c <- c + 1
  #b <- 1
  #}
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}
get_Enrollment_Single_Course <- function(data.concise, major_string, num_string) {
  library(reshape2)
  
  #unique_courses <- unique(data.concise$Course)
  unique_terms <- unique(data.concise$Term)
  
  
  #creates data frame for each college and term
  points.data = data.frame(matrix(nrow = length(unique_terms), ncol = 2))
  colnames(points.data) <- replace(c(paste(major_string, num_string, sep=" "), " "), 2, "Terms")
  
  #calculates class average for every college each term
  b <- 1
  c <- 1
  #for (i in unique_courses) {
  
  for (j in unique_terms) {
    sum_enrolled <- sum(data.concise[data.concise$Course == major_string & 
                                       data.concise$Term == j &
                                       data.concise$Number == num_string,5])
    #sum_averages <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19])
    #checks to make sure there is at least 1 person enrolled
    #if (sum_enrolled > 0) {
    #sum.weighted <- sum(data.concise[data.concise$Course == i & data.concise$Term == j,19] *
    #data.concise[data.concise$Course == i & data.concise$Term == j,5])
    
    #calculate class average
    points.data[b, c] <- sum_enrolled
    
    points.data[b, c+1] <- j
    #}
    b <- b + 1
    
    
    #}
  }
  #c <- c + 1
  #b <- 1
  #}
  
  points.data.long<-melt(points.data,id.vars="Terms")
  points.data.long <- points.data.long[complete.cases(points.data.long), ]
  
  return(points.data.long)
  
}
get_Professors_Single_Course <- function(data.concise, major_string, num_string) {
  library(reshape2)
  
  prof_data <- data.concise[data.concise$Course == major_string &
                              data.concise$Number == num_string, ]
  
  prof_data$Professor <- sub('(^\\w+)(,*)\\s.+','\\1', prof_data$Professor)
  #prof_data.concise <- prof_data[order(prof_data$Enrolled,decreasing=TRUE)[1:5],]
  unique_profs <- unique(prof_data$Professor)
  pass_fail <- FALSE
  #determine if pass/fail or A-F
  if (nrow(prof_data[as.numeric(as.character(prof_data$Pass)) + as.numeric(as.character(prof_data$Fail)) > 10, ]) > 
      nrow(prof_data) / 2) {
    pass_fail <- TRUE
  }
  
  if (!pass_fail) {
    prof.points.data = data.frame(matrix(nrow = length(unique_profs), ncol = 7))
    colnames(prof.points.data) <- c("Professor", "A", "B", "C", "D", "F", "Enrolled")
    
    j <- 1
    for (i in unique_profs) {
      prof.points.data[j, 1] <- i
      prof.points.data[j, 2] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 6])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) 
      prof.points.data[j, 3] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 7])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 4] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 8])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 5] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 9])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 6] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 10])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 7] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      j <- j + 1
    }
  }
  else {
    prof.points.data = data.frame(matrix(nrow = length(unique_profs), ncol = 4))
    colnames(prof.points.data) <- c("Professor", "Pass", "Fail", "Enrolled")    
    
    j <- 1
    for (i in unique_profs) {
      prof.points.data[j, 1] <- i
      prof.points.data[j, 2] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 14])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) 
      prof.points.data[j, 3] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 15])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 4] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      j <- j + 1
    }
  }
  prof_data.points.concise <- prof.points.data[order(prof.points.data$Enrolled,decreasing=TRUE)[1:5],]
  prof_data.points.concise$Enrolled <- NULL
  return(prof_data.points.concise)
  
}

get_Professors_Single_Course_Detailed <- function(data.concise, major_string, num_string) {
  library(reshape2)
  
  prof_data <- data.concise[data.concise$Course == major_string &
                              data.concise$Number == num_string, ]
  
  prof_data$Professor <- sub('(^\\w+)(,*)\\s.+','\\1', prof_data$Professor)
  #prof_data.concise <- prof_data[order(prof_data$Enrolled,decreasing=TRUE)[1:5],]
  unique_profs <- unique(prof_data$Professor)
  pass_fail <- FALSE
  #determine if pass/fail or A-F
  if (nrow(prof_data[as.numeric(as.character(prof_data$Pass)) + as.numeric(as.character(prof_data$Fail)) > 10, ]) > 
      nrow(prof_data) / 2) {
        pass_fail <- TRUE
  }
  
  if (!pass_fail) {
    prof.points.data = data.frame(matrix(nrow = length(unique_profs), ncol = 9))
    colnames(prof.points.data) <- c("Professor", "A", "B", "C", "D", "F", "W+I", "Enrolled", "Num_Sections")
    
    j <- 1
    for (i in unique_profs) {
      prof.points.data[j, 1] <- i
      prof.points.data[j, 2] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 6])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) 
      prof.points.data[j, 3] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 7])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 4] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 8])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 5] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 9])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 6] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 10])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 7] <- (sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 11])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) + sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 12])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 8] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 9] <- nrow(prof_data[prof_data$Professor == i,])
      j <- j + 1
    }
  }
  else {
    prof.points.data = data.frame(matrix(nrow = length(unique_profs), ncol = 5))
    colnames(prof.points.data) <- c("Professor", "Pass", "Fail", "W+I", "Enrolled")    
    
    j <- 1
    for (i in unique_profs) {
      prof.points.data[j, 1] <- i
      prof.points.data[j, 2] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 14])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) 
      prof.points.data[j, 3] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 15])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 4] <- (sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 11])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5]))) + sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 12])) * as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      prof.points.data[j, 5] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i, 5])))
      j <- j + 1
    }
  }
  prof_data.points.concise <- prof.points.data[order(prof.points.data$Enrolled,decreasing=TRUE)[1:5],]
  #prof_data.points.concise$Enrolled <- NULL
  return(prof_data.points.concise)
  
}

get_Professors_Single_Course_By_Semester <- function(data.concise, major_string, num_string) {
  library(reshape2)
  
  prof_data <- data.concise[data.concise$Course == major_string &
                              data.concise$Number == num_string, ]
  
  prof_data$Professor <- sub('(^\\w+)(,*)\\s.+','\\1', prof_data$Professor)
  #prof_data.concise <- prof_data[order(prof_data$Enrolled,decreasing=TRUE)[1:5],]
  unique_profs <- unique(prof_data$Professor)
  unique_terms <- unique(prof_data$Term)
  pass_fail <- FALSE
  #determine if pass/fail or A-F
  if (nrow(prof_data[as.numeric(as.character(prof_data$Pass)) + as.numeric(as.character(prof_data$Fail)) > 10, ]) > 
      nrow(prof_data) / 2) {
    pass_fail <- TRUE
  }
  
  if (!pass_fail) {
    prof.points.data = data.frame(matrix(nrow = length(unique_profs), ncol = 9))
    colnames(prof.points.data) <- c("Professor", "A", "B", "C", "D", "F", "W+I", "Enrolled", "Term")
    
    j <- 1
    for (i in unique_profs) {
      for (k in unique_terms) {
        if (sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) > 0) {
          prof.points.data[j, 1] <- i
          prof.points.data[j, 2] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 6])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) 
          prof.points.data[j, 3] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 7])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 4] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 8])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 5] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 9])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 6] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 10])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 7] <- (sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 11])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) + sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 12])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 8] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 9] <- k
          j <- j + 1
        }
      }
    }
  }
  else {
    prof.points.data = data.frame(matrix(nrow = length(unique_profs), ncol = 6))
    colnames(prof.points.data) <- c("Professor", "Pass", "Fail", "W+I", "Enrolled", "Term")    
    
    j <- 1
    for (i in unique_profs) {
      for (k in unique_terms) {
        if (sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) > 0) {
          prof.points.data[j, 1] <- i
          prof.points.data[j, 2] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 14])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) 
          prof.points.data[j, 3] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 15])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 4] <- (sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 11])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5]))) + sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 12])) * as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))) / sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 5] <- sum(as.numeric(as.character(prof_data[prof_data$Professor == i & prof_data$Term == k, 5])))
          prof.points.data[j, 6] <- k
          j <- j + 1
        }
      }
    }
  }
  #prof_data.points.concise <- prof.points.data[order(prof.points.data$Enrolled,decreasing=TRUE)[1:5],]
  #prof_data.points.concise$Enrolled <- NULL
  return(prof.points.data)
  
}