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
  
  selected <- reactive({ course_table()[input$CourseTable_rows_selected,]$Grade_Course })
  
  courseInstances <- reactive({
    req(selected())  
    profileIDS <- unique(profile_course()$IDS)
    filter(course_data(), 
                          Grade_Course == selected(), 
                          when == .when,
                          IDS %in% profileIDS,
                          !is.null(Grade_Final_Grade), 
                          Grade_Final_Grade %in% valid_grades)
  })
  
  course_table <- reactive({
    profileIDS <- unique(profile_course()$IDS)
    Ntotal <- length(profileIDS)
    
    course_data() %>%
      filter(when == .when,
             IDS %in% profileIDS) %>%
      group_by(Grade_Course) %>%
      dplyr::summarize(Title = dplyr::first(Grade_Course_Title),
                N = n_distinct(IDS),
                pct = N / Ntotal) %>%
      arrange(-N) %>% filter(pct > 0.10)    
  })
  
  output$CourseTable <- DT::renderDataTable({
    datatable(
      course_table(),
      options = list(
        pageLength = 5,
        searching = FALSE,
        lengthChange = FALSE
      ),
      rownames = FALSE,
      selection = list(mode = "single")
    ) %>% formatPercentage('pct', 2)
  },
  server = FALSE)

  callModule(gradeDistribution, "GradeDist", courseInstances, groupingVar)

  return(selected)
}