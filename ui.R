shinyUI(navbarPage("Institutional Records Explorer",
  
  tabPanel("Course",
           sidebarLayout(
             sidebarPanel(
               selectInput('subject', 'Subject', c(as.character(NA)), width = "8em"),
               selectInput('number', 'Number', c(as.character(NA)), width = "8em"),
               width = 2),
             mainPanel(
              gradeDistributionUI("GradeDist"),
             fluidRow(column(4, courseWidgetUI("CoursesBefore", "Taken Before")),
               column(4, courseWidgetUI("CoursesWith", "Taken With")),
               column(4, courseWidgetUI("CoursesAfter", "Taken After"))),
             verbatimTextOutput("status"),
      DT::dataTableOutput("DegreesAfter"),
      width = 10))), 
  tabPanel("Degree", h1("Degree"))
  )
)