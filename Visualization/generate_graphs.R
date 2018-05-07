#CREATE GRAPHS AND DATA VISUALIZATIONS FOR PRESENTATION

source("data_processing.R")

library(ggplot2)

black <- '#000000'
green_Dark <- '#43a047'
purple <- '#7b1fa2'
yellow <- '#ffea00'
red <- '#d50000'
teal <- '#18ffff'
blue <- '#1565c0'
orange <- '#ff6f00'
green_Light <- '#76ff03'

#graph grades per semester per college
college_grades <- get_Grades_College(data.concise)

ggplot(college_grades, 
  aes(Terms, value, color=variable, group=variable)) + 
  
  geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
  theme(axis.text.x = element_text(size=7, angle=45, hjust=1),
                      axis.title.x = element_text(size = 12, face="bold"),
                      axis.title.y = element_text(size = 12, face="bold"),
                      panel.background = element_blank()) +
  scale_x_discrete(limits=rev(unique(college_grades$Term))) +
  scale_color_manual(values=c(purple, yellow, green_Dark, red, teal, blue, green_Light, orange), name = "College") +
  ggtitle("Average Grade by Term") +
  xlab("Term") + ylab("Grade")

#graph non-summer enrollment
college_grades <- get_Enrollment_College(data.concise)
no_summer <- college_grades[!grepl("Sum", college_grades$Terms), ]

ggplot(no_summer, 
  aes(no_summer$Terms, value, color=variable, group=variable)) + 
  
  geom_line(size=1) + geom_point(size=2) + theme_minimal() + 
  theme(axis.text.x = element_text( size=8, angle=45, hjust=1),
                      axis.title.x = element_text(size = 12, face="bold"),
                      axis.title.y = element_text(size = 12, face="bold"),
                      panel.background = element_blank()) +
  scale_x_discrete(limits=rev(unique(no_summer$Terms))) +
  scale_color_manual(values=c(purple, black, green_Dark, red, teal, blue, green_Light, orange), name = "College") +
  ggtitle("Total Class Enrollment by College by Term") +
  xlab("Term") + ylab("Enrollment")

#enrollment vs withdrawl
cpsc_withdrawal <- data.concise[data.concise$Course == "CPSC", ]

ggplot(cpsc_withdrawal, 
  aes(x=as.numeric(as.character(cpsc_withdrawal$Enrolled)), 
  y=((as.numeric(as.character(cpsc_withdrawal$W)) + as.numeric(as.character(cpsc_withdrawal$I))) * cpsc_withdrawal$Enrolled /100), 
  group = 1, color = cpsc_withdrawal$Number)) +
  
  geom_point(
    aes(color = cut(as.numeric(as.character(cpsc_withdrawal$Number)), 
      c(0, 2000, 3000, 4000, 5000, Inf), right = FALSE))) +
  theme_minimal() + 
  theme(axis.text.x = element_text(size=7, angle=45, hjust=1),
                      axis.title.x = element_text(size = 12, face="bold"),
                      axis.title.y = element_text(size = 12, face="bold"),
                      panel.background = element_blank()) +
  scale_color_manual(name = "Course Number",
                     values = c(orange, red, purple, green_Dark, blue),
                     labels = c("1000s", "2000s", "3000s", "4000s", "Grads")) +
  ggtitle("Enrollment vs. Withdrawal by CPSC Course Number") +
  xlab("Enrollment") + ylab("Withdrawals") + theme_minimal() + 
  theme(axis.text.x = element_text(size=7, angle=45, hjust=1),
                      axis.title.x = element_text(size = 12, face="bold"),
                      axis.title.y = element_text(size = 12, face="bold"),
                      axis.text.y = element_text(size=7),
                      panel.background = element_blank())

#create bar graph comparing professor grades
professor_data.content <- c("Grades for ", "CPSC", "2120", " Professors")
professor_data <- get_Professors_Single_Course(data.concise, professor_data.content[2], professor_data.content[3])                  
professor_data.concise <- melt(professor_data, id.vars='Professor')

ggplot(professor_data.concise, aes(Professor, value)) + 
  
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(name = "Course Number", 
                    values = c(green_Dark, blue, yellow, orange, red)) +
  ggtitle(paste(professor_data.content, collapse = '')) +
  xlab("Professors") + ylab("Grade Percentage") + theme_minimal() + 
  theme(axis.text.x = element_text(size=10),
                      axis.title.x = element_text(size = 12, face="bold"),
                      axis.title.y = element_text(size = 12, face="bold"),
                      axis.text.y = element_text(size=10),
                      panel.background = element_blank())

