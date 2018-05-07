library(shiny)

shinyUI(
  navbarPage(
    "Clemson Grade Distributions",
    #theme = "bootstrap.css", #Cosmo https://bootswatch.com/cosmo/
    
    #PAGE
    tabPanel("Colleges", 
      id="coll_panel",
      value="coll_panel",
      fluidPage(
        titlePanel("Colleges"),
        sidebarLayout(
          sidebarPanel(
            id="coll_sidebar",
            h3("Data"),
            radioButtons(
              "coll_variable", 
              label = "Variable:",
              choices = list(
                "Average Grade" = 0, 
                "Enrollment" = 1),
              selected = 0
            ),
            h3("Display"),
            checkboxInput(
              "coll_summer",
              label = "Include Summer Classes?",
              FALSE
            ),
            p("Including summer classes leads to higher variability due to a many factors")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Line Graph", plotOutput(outputId = "collLine", width = "1200", height = "800"))
          ))))),
    
    #PAGE
    tabPanel("Schools", 
      id="schl_panel",
      value="schl_panel",
      fluidPage(
        titlePanel("Schools"),
        sidebarLayout(
          sidebarPanel(
            id="schl_sidebar",
            h3("Data"),
            selectInput(
              "schl_college", 
              label = "College:",
              choices = list(
                "CAAH" = 0,
                "CAFLS" = 1,
                "CBSHS" = 2,
                "CBUS" = 3,
                "CECAS" = 4,
                "CED" = 5,
                "CSCI" = 6,
                "LBRY" = 7),
              selected = 0
            ),
            radioButtons(
              "schl_variable", 
              label = "Variable:",
              choices = list(
                "Average Grade" = 0, 
                "Enrollment" = 1),
              selected = 0
            ),
            h3("Display"),
            checkboxInput(
              "schl_summer",
              label = "Include Summer Classes?",
              FALSE
            ),
            p("Including summer classes leads to higher variability due to a many factors")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Line Graph", plotOutput(outputId = "schlLine", width = "1200", height = "800"))
          ))))),
    
      tabPanel("Majors", 
      id="majr_panel",
      value="majr_panel",
      fluidPage(
        titlePanel("Major"),
        sidebarLayout(
          sidebarPanel(
            id="majr_sidebar",
            h3("Data"),
            selectInput(
              "majr_college", 
              label = "College:",
              choices = list(
                "CAAH" = 0,
                "CAFLS" = 1,
                "CBSHS" = 2,
                "CBUS" = 3,
                "CECAS" = 4,
                "CED" = 5,
                "CSCI" = 6,
                "LBRY" = 7),
              selected = 0
            ),
            #SELECT SCHOOL
            radioButtons(
              "majr_variable", 
              label = "Variable:",
              choices = list(
                "Average Grade" = 0, 
                "Enrollment" = 1),
              selected = 0
            ),
            h3("Display"),
            checkboxInput(
              "majr_summer",
              label = "Include Summer Classes?",
              FALSE
            ),
            p("Including summer classes leads to higher variability due to a many factors")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Line Graph", plotOutput(outputId = "majrLine", width = "1200", height = "800"))
          ))))),
    
    #PAGE
    tabPanel("Courses", 
      fluidPage(
        titlePanel("Courses"),
        sidebarLayout(
          sidebarPanel(
            h3("Data"),
            #DYNAMICALLY POPULATE SUGGESTIONS
            textInput("curs_dept", 
              label = "Course Department", 
              value = "CPSC"
            ),
            numericInput("curs_num", 
              label = "Course Number", 
              value = 2120
            )
            #EXCLUDE SUMMER
          ),
          mainPanel(
            tabsetPanel(
              #tabPanel("Line Graph", plotOutput(outputId = "cursLine", width = "1200", height = "800")),
              tabPanel("Bar Graph", plotOutput(outputId = "cursBar", width = "1200", height = "800")),
              #tabPanel("Area Graph", plotOutput(outputId = "cursArea", width = "1200", height = "800")),
              #tabPanel("Scatterplot", plotOutput(outputId = "cursSctr"))
              tabPanel("Table", plotOutput(outputId = "cursTable", width = "1200", height = "800"))
          ))))),
    
    #PAGE
    tabPanel("About", 
      fluidPage(
        titlePanel("About"),
        sidebarLayout(
          sidebarPanel(
            h2("Clemson University"),
            h3("Dr. Herzog"),
            h4("CPSC 4300"),
            h4("Spring 2018")
          ),
          mainPanel(
            h4("Created by:"),
            h2("Harold Hyte, Sean Myers, Wade King, Josh Moore"),
            br(),br(),
            h4("Idea"),
            p("Clemson’s current grade distribution website, which aggregates student’s grades according to courses and professors, has several key drawbacks including in the visualization and correlation of the data. The website simply displays a large table listing the values of different grades and course statuses. This is unintuitive, especially for students who use this as a tool to decide what classes and professors to take when they register."),
            h4("Methodology"),
            p("Initially, we had to build a webscraper to get the data from Clemson’s Grade Distribution website. This was done using a web-interface through Python 3.4 that was able to scrape the data from a given HTML document. This data was placed in a CSV file for further use. We then used R, a programmign language for statistics and mathematical computing, to load the CSV file and separate and mash together the data that we needed. This data was then plotted using a combination of R libraries called ggplot2 and R Shiny. Ggplot2 creates the graphs and charts, which are then displayed by R Shiny. R Shiny further provides the ability to interact with the graphs to better see and understand the data."),
            h4("Difficulties"),
            p("Clemson’s Office of Institutional Research’s website has very inconsistent labeling of data and does not require grades to be reported correctly. There are multiple instances where values did not add up correctly, professor names were mispelled, or majors changed names."),
            p("Many classes are unreported due to having fewer than 10 students. This skews the data for higher-level and graduate classes, because they typically have smaller class sizes."),
            p("Some schools, departments, and courses have no data at all, mainly due to the class size issue, so comparisons can be difficult to make."),
            p("The grade distirbution website is protected by Duo and a Clemson login, so doing an HTTP request on the website was impossible for us to perform. This forced us to use a web browser to pull the data.")
          )
        )
      )
    )
  )
)