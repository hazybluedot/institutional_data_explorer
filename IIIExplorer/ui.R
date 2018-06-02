library(shinythemes)

shinyUI(navbarPage("Institutional Records Explorer", theme = "superhero",
  
  tabPanel("Course",
           sidebarLayout(
             sidebarPanel(
               selectInput("subject", "Subject", c(as.character(NA)), width = "8em"),
               selectInput("number", "Number", c(as.character(NA)), width = "8em"),
               sliderInput("dateRange", "Date Range", min = Sys.Date() - 1, 
                           max = Sys.Date(), 
                           value = c(Sys.Date() - 1, Sys.Date()), 
                           dragRange = TRUE,
                           timeFormat = "%b %Y"),
               selectInput("groupBy", "Compare Across", c(as.character(NA)), width = "12em"),
               verbatimTextOutput("status"),
               width = 3),
             mainPanel(
              gradeDistributionUI("GradeDist"),
              tabsetPanel(
                tabPanel("Taken Before", courseWidgetUI("CoursesBefore", "Taken Before")),
                tabPanel("Taken With", courseWidgetUI("CoursesWith", "Taken With")),
                tabPanel("Taken After", courseWidgetUI("CoursesAfter", "Taken After"))
                ),
      DT::dataTableOutput("DegreesAfter"),
      width = 9))), 
  tabPanel("Degree", h1("Degree"))
  )
)