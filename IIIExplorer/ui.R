library(shiny)
library(shinysky)
library(shinythemes)

source("init.R", local = TRUE)

college_majors <- read.csv(file_names$college_majors, stringsAsFactors = FALSE)
college_choices <-
  (college_majors %>% distinct(college_desc))$college_desc
major_choices <-
  (college_majors %>% arrange(major) %>% distinct(major))$major

currentYear <- as.numeric(format(Sys.Date(), "%Y"))

#shinyUI(
function(request) {
  fluidPage(
    tags$head(
      tags$style(HTML("
    .shiny-output-error-validation {
      height: 2ex;
    }

    #loadmessage {
	    position: fixed;
	    top: 0px;
          left: 0px;
          width: 100%;
          padding: 5px 0px 5px 0px;
          text-align: center;
          font-weight: bold;
          font-size: 100%;
          color: #000000;
          background-color: #91bfdb;
          z-index: 105;
    }

    .datatables {
        min-height: 7em;
    }
    ")),
      #includeScript(system.file('www', 'prettyterm.js'))
      tags$link(rel = "styelsheet", type = "text/css", href = "stylesheet.css"),
      tags$script(src = "prettyterms.js", type = "text/javascript")),

    title = "Institutional Records Explorer",
    theme = shinytheme("united"),
    #tabPanel("Course",
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                         tags$div("Loading Data...", id = "loadmessage")),
        #selectInput("subject", "Subject", c(as.character(NA)), width = "8em"),
        #selectInput("number", "Number", c(as.character(NA)), width = "8em"),
        tags$style(
          type = "text/css",
          ".typeahead,
          .tt-query,
          .tt-hint {
          font-size: 14px;
          border: 1px solid #cccccc;
          /*! -webkit-border-radius: 4px; */
          /*! -moz-border-radius: 4px; */
          /*! border-radius: 4px; */
          }"
),
wellPanel(
  h3("Analysis"),
  textInput.typeahead(
    id = "profile_course",
    placeholder = "loading...",
    local = data.frame(),
    valueKey = "",
    tokens = c(),
    template = ""
  ),
  helpText("Select a course to examine"),
  selectInput("groupBy", "Compare Across", c("None" = "none"), width = "12em"),
  conditionalPanel(
    "input.groupBy == 'group'",
    textInput.typeahead(
      id = "compareCourse",
      placeholder = "loading...",
      local = data.frame(),
      valueKey = "",
      tokens = c(),
      template = ""
    ),
    helpText(
      "Type a course here, or select one from the 'taken before' tab to the right."
    )
  )
),
wellPanel(
  h3("Filter By"),
  #           checkboxGroupInput("demographicFilter", "Demographics", inline = TRUE,
  #                              choices = c("All"), selected = c("All"))),
  #actionButton("showFilter", "Filter by ..."),
  select2Input(
             "collegeFilter",
             "College",
             choices = college_choices,
             selected = c("Engineering"),
             multiple = TRUE),
  helpText("Leave blank for all. Multiple selections are additive, i.e. logical OR"),
  selectInput("filterBoolean", NULL, choices = c(AND = "&", OR = "|"), selected = "AND", width = "5em"),
  helpText("combine college and major filter with either logical AND or OR."),
  select2Input(
    "majorFilter",
    "Major",
    choices = major_choices,
    multiple = TRUE),
  helpText("Leave blank for all. Multiple selections are additive, i.e. logical OR"),
  checkboxInput("hasDegree", "Has Degree"),
  helpText("When checked only students who earned a degree in one of the filtered colleges or majors will be included."),
  sliderInput(
    "termRange",
    "Date Range",
    min = NA,
    max = NA,
    value = c(NA, NA),
    dragRange = TRUE,
    sep = "",
    step = 0.5
  ),
  actionButton("applyFilter", "Apply Filter"),
  div(
    "Filtered dataset contains ",
    br(),
    textOutput("nCourses", inline = TRUE),
    " course records for ",
    br(),
    textOutput("nStudents", inline = TRUE),
    " distinct students."
  )
),
actionButton("resetInputs", "Reset"),
#bookmarkButton(),
verbatimTextOutput("status"),
width = 3
        ),
mainPanel(
  h1(textOutput("CourseTitle")),
  fluidRow(tabsetPanel(
    id = "profile",
    tabPanel(
      "Grade Distribution",
      gradeDistributionUI("GradeDist"),
      value = "grades"
    ),
    tabPanel("Grade Matrix", 
             gradeMatrixUI("GradeMatrix"), 
             value = "grade_matrix")
    #tabPanel("Predictors of Success", successAnalysisUI("SuccessAnalysis"), value = "success")
  )),
  fluidRow(tabsetPanel(
    tabPanel(
      "Taken Before",
      courseWidgetUI("CoursesBefore", "Taken Before")
    ),
    tabPanel("Taken With", courseWidgetUI("CoursesWith", "Taken With")),
    tabPanel("Taken After", courseWidgetUI("CoursesAfter", "Taken After"))
  ),
  DT::dataTableOutput("DegreesAfter"),
  tableOutput("memUsage"),
  width = 9
))
    )
      )
}