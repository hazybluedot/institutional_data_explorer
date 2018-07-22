library(shiny)

# Course List and quick preview module

courseWidgetUI <- function(id, label = "Course Preview", header = NULL) {
  ns <- NS(id)
  
  tagList(
    h2(textOutput(ns("Title"))),
    hr(),
    fluidRow(
      column(4, DT::dataTableOutput(ns("CourseTable"))),
      column(8, gradeDistributionUI(ns("GradeDist")))
      #column(8, plotOutput(ns("GradeDist")))
      )
    #,
    #verbatimTextOutput(ns("status"))
  )
}

# Module Server Function
courseWidget <- function(input, output, session, course_data, .when, profile_course, groupingVar) {
  ns <- session$ns
  #vals = reactiveValues(selected = NULL, courseInstances = NULL, profileIDS = c())
  #reset = reactiveValues(sel = "")
  
  selected <- reactive({ course_table()[input$CourseTable_rows_selected,]$course })
  
  courses_when <- reactive({
    profileIDS <- unique(profile_course()$id)
    filter(course_data(), 
           when == .when,
           id %in% profileIDS)
  })
  
  course_table <- reactive({
    profileIDS <- unique(profile_course()$id)
    Ntotal <- length(profileIDS)
    
    courses_when() %>%
      group_by(course) %>%
      dplyr::summarize(Title = dplyr::first(grade_title),
                N = n_distinct(id),
                pct = N / Ntotal) %>%
      arrange(-N) %>% filter(pct > 0.10)    
  })
  
  selected_course_first_instances <- reactive({
    req(selected())  
    first_instance(course_instances(courses_when(), selected()))
  })
  
  output$CourseTable <- DT::renderDataTable({
    DT::datatable(
      course_table(),
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE
      ),
      rownames = FALSE,
      selection = list(mode = "single")
    ) %>% DT::formatPercentage('pct', 2)
  },
  server = FALSE)

  callModule(gradeDistribution, "GradeDist", selected_course_first_instances, groupingVar)

  return(selected)
}