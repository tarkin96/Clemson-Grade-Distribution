#FUNCTIONS
source("data_functions.R")
source("data_processing.R")

shinyServer = function(input, output) {
  
  #VISUALIZATION
  library(ggplot2)
  library(reshape2)
  #define maximum visibility graph colors
  black <- '#000000'
  red <- '#ff0000'
  lime <- '#00ff00'
  blue <- '#0000ff'
  yellow <- '#ffff00'
  magenta <- '#ff00ff'
  teal <- '#00ffff'
  orange <- '#ffa500'
  purple <- '#800080'
  pink <- '#ffc0cb'
  green <- '#008000'
  
  #DATA  
  #colleges
  college_grades = get_Grades_Colleges(data.concise)
  college_grades_no_summer = college_grades[!grepl("Sum", college_grades$Terms), ]
  college_enroll = get_Enrollment_Colleges(data.concise)
  college_enroll_no_summer = college_enroll[!grepl("Sum", college_enroll$Terms), ]
  #schools
  school_grades = get_Grades_Schools(data.concise)
  school_enroll = get_Enrollment_Schools(data.concise)
  print("setup")
  
  #UI
  #observeEvent(input$navbar, {
  #  toggle("coll_sidebar", condition = input$navbar == "coll_panel")
  #  toggle("schl_sidebar", condition = input$navbar == "schl_panel")
  #  toggle("majr_sidebar", condition = input$navbar == "majr_panel")
  #})
  
  #GRAPHS
  #COLLEGE
  output$collLine = renderPlot({
    
    if(0 %in% input$coll_variable)
    {
      if(FALSE %in% input$coll_summer)
      {
        #ORGANIZE SCALE BY HIGHEST VALUES
        #most_recent_val = college_grades_no_summer[college_grades_no_summer$Terms == "Fall 2017",]
        #most_recent_val = most_recent_val[order(most_recent_val$value),] 
        
        ggplot(college_grades_no_summer, 
          aes(Terms, value, color=variable, group=variable)) + 
          
          geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
          theme(axis.text.x = element_text(size=7, angle=45, hjust=1),
                              axis.title.x = element_text(size = 12, face="bold"),
                              axis.title.y = element_text(size = 12, face="bold"),
                              panel.background = element_blank()) +
          scale_x_discrete(limits=rev(unique(college_grades_no_summer$Term))) +
          scale_colour_discrete(name = "College", c=150, l=70, h=c(0,275), h.start = 0) +
          ggtitle("Average Grade by College by Term w/o Summer") +
          xlab("Term") + ylab("Grade")
      }
      else if(TRUE %in% input$coll_summer)
      {
        ggplot(college_grades, 
          aes(Terms, value, color=variable, group=variable)) + 
          
          geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
          theme(axis.text.x = element_text(size=7, angle=45, hjust=1),
                              axis.title.x = element_text(size = 12, face="bold"),
                              axis.title.y = element_text(size = 12, face="bold"),
                              panel.background = element_blank()) +
          scale_x_discrete(limits=rev(unique(college_grades$Term))) +
          scale_colour_discrete(name = "College", c=150, l=70, h=c(0,275), h.start = 0) +
          ggtitle("Average Grade by College by Term") +
          xlab("Term") + ylab("Grade")
      }
    }
    else if(1 %in% input$coll_variable)
    {
      if(FALSE %in% input$coll_summer)
      {
        ggplot(college_enroll_no_summer, 
          aes(college_enroll_no_summer$Terms, value, color=variable, group=variable)) + 
          
          geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
          theme(axis.text.x = element_text( size=8, angle=45, hjust=1),
                              axis.title.x = element_text(size = 12, face="bold"),
                              axis.title.y = element_text(size = 12, face="bold"),
                              panel.background = element_blank()) +
          scale_x_discrete(limits=rev(unique(college_enroll_no_summer$Terms))) +
          scale_colour_discrete(name = "College", c=150, l=70, h=c(0,275), h.start = 0) +
          ggtitle("Total Class Enrollment by College by Term w/o Summer") +
          xlab("Term") + ylab("Enrollment")
      }
      else if(TRUE %in% input$coll_summer)
      {
        ggplot(college_enroll, 
          aes(college_enroll$Terms, value, color=variable, group=variable)) + 
          
          geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
          theme(axis.text.x = element_text( size=8, angle=45, hjust=1),
                              axis.title.x = element_text(size = 12, face="bold"),
                              axis.title.y = element_text(size = 12, face="bold"),
                              panel.background = element_blank()) +
          scale_x_discrete(limits=rev(unique(college_enroll$Terms))) +
          scale_colour_discrete(name = "College", c=150, l=70, h=c(0,275), h.start = 0) +
          ggtitle("Total Class Enrollment by College by Term") +
          xlab("Term") + ylab("Enrollment")
      }
    }
    
  })

  #SCHOOL
  output$schlLine = renderPlot({
    if(0 %in% input$schl_college) {major_string = collList[0]}
    if(1 %in% input$schl_college) {major_string = collList[1]}
    if(2 %in% input$schl_college) {major_string = collList[2]}
    if(3 %in% input$schl_college) {major_string = collList[3]}
    if(4 %in% input$schl_college) {major_string = collList[4]}
    if(5 %in% input$schl_college) {major_string = collList[5]}
    if(6 %in% input$schl_college) {major_string = collList[6]}
    if(7 %in% input$schl_college) {major_string = collList[7]}
    
    excludeList = deptList
    excludeList = excludeList[!grepl(major_string, excludeList$data.concise.College), ]
    
    school_grades.specific = school_grades
    school_enroll.specific = school_enroll
    
    for(i in 1:nrow(excludeList))
    {
      school_grades.specific = school_grades.specific[!grepl(excludeList[i,1], school_grades.specific$variable), ]
      school_enroll.specific = school_enroll.specific[!grepl(excludeList[i,1], school_enroll.specific$variable), ]
    }
    school_grades_no_summer = school_grades.specific[!grepl("Sum", school_grades.specific$Terms), ]
    school_enroll_no_summer = school_enroll.specific[!grepl("Sum", school_enroll.specific$Terms), ]
    
    if(0 %in% input$schl_variable)
    {
      if(FALSE %in% input$schl_summer)
      {
        ggplot(school_grades_no_summer, 
          aes(Terms, value, color=variable, group=variable)) + 
          
          geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
          theme(axis.text.x = element_text(size=7, angle=45, hjust=1),
                              axis.title.x = element_text(size = 12, face="bold"),
                              axis.title.y = element_text(size = 12, face="bold"),
                              panel.background = element_blank()) +
          scale_x_discrete(limits=rev(unique(school_grades_no_summer$Term))) +
          scale_colour_discrete(name = "School", c=150, l=70, h=c(0,275), h.start = 0) +
          ggtitle("Average Grade by School by Term w/o Summer") +
          xlab("Term") + ylab("Grade") 
      }
      else if(TRUE %in% input$schl_summer)
      {
        ggplot(school_grades.specific, 
          aes(Terms, value, color=variable, group=variable)) + 
          
          geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
          theme(axis.text.x = element_text(size=7, angle=45, hjust=1),
                              axis.title.x = element_text(size = 12, face="bold"),
                              axis.title.y = element_text(size = 12, face="bold"),
                              panel.background = element_blank()) +
          scale_x_discrete(limits=rev(unique(school_grades.specific$Term))) +
          scale_colour_discrete(name = "School", c=150, l=70, h=c(0,275), h.start = 0) +
          ggtitle("Average Grade by School by Term") +
          xlab("Term") + ylab("Grade") 
      }
    }
    else if(1 %in% input$schl_variable)
    {
      if(FALSE %in% input$schl_summer)
      {
        ggplot(school_enroll_no_summer, 
          aes(school_enroll_no_summer$Terms, value, color=variable, group=variable)) + 
          
          geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
          theme(axis.text.x = element_text( size=8, angle=45, hjust=1),
                              axis.title.x = element_text(size = 12, face="bold"),
                              axis.title.y = element_text(size = 12, face="bold"),
                              panel.background = element_blank()) +
          scale_x_discrete(limits=rev(unique(school_enroll_no_summer$Terms))) +
          scale_colour_discrete(name = "School", c=150, l=70, h=c(0,275), h.start = 0) +
          ggtitle("Total Class Enrollment by School by Term w/o Summer") +
          xlab("Term") + ylab("Enrollment")
      }
      else if(TRUE %in% input$schl_summer)
      {
        ggplot(school_enroll.specific, 
          aes(school_enroll.specific$Terms, value, color=variable, group=variable)) + 
          
          geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
          theme(axis.text.x = element_text( size=8, angle=45, hjust=1),
                              axis.title.x = element_text(size = 12, face="bold"),
                              axis.title.y = element_text(size = 12, face="bold"),
                              panel.background = element_blank()) +
          scale_x_discrete(limits=rev(unique(school_enroll.specific$Terms))) +
          scale_colour_discrete(name = "School", c=150, l=70, h=c(0,275), h.start = 0) +
          ggtitle("Total Class Enrollment by School by Term") +
          xlab("Term") + ylab("Enrollment")
      }
    }
  })
  
  #MAJOR
  output$majrLine = renderPlot({})

  #COURSE
  #output$instLine
  output$cursBar = renderPlot({
    course_dept = input$curs_dept
    course_num = input$curs_num
    
    professor_data.content <- c("Grades for Professors")
    professor_data <- get_Professors_Single_Course(data.concise, course_dept, course_num)                  
    professor_data.concise <- melt(professor_data, id.vars='Professor')
    
    ggplot(professor_data.concise, aes(Professor, value)) + 
      
      geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
      scale_colour_discrete(name = "Grade") +
      ggtitle(paste(professor_data.content, collapse = '')) +
      xlab("Professors") + ylab("Grade Percentage") + theme_minimal() + 
      theme(axis.text.x = element_text(size=10),
                          axis.title.x = element_text(size = 12, face="bold"),
                          axis.title.y = element_text(size = 12, face="bold"),
                          axis.text.y = element_text(size=10),
                          panel.background = element_blank())
  })
  output$cursArea = renderPlot({
    course_dept = input$curs_dept
    course_num = input$curs_num
    
    professor_data.content <- c("Grades for Professors")
    ##USE MORE DETAILED FUNCTION WITH TERMS
    professor_data <- get_Professors_Single_Course(data.concise, course_dept, course_num)                  
    professor_data.concise <- melt(professor_data, id.vars='Professor')
    
    ggplot(professor_data.concise, aes(x=Term, y=value, colour = y)) + 
      geom_area() +
      scale_colour_discrete(name = "Grade", c=150, l=70, h=c(0,275), h.start = 0)
  })
  #output$cursSctr = renderPlot({})
  output$cursTable = renderPlot({
    course_dept = input$curs_dept
    course_num = input$curs_num
    
    professor_data.content <- c("Grades for Professors")
    professor_data <- get_Professors_Single_Course(data.concise, course_dept, course_num)                  
    professor_data.concise <- melt(professor_data, id.vars='Professor')
    
    table(professor_data)
  })

}
