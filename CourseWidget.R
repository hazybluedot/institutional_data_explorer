library(shiny)

# Course List and quick preview module

courseWidgetUI <- function(id, label = "Course Preview", header = NULL) {
  ns <- NS(id)
  
  tagList(
    h2(textOutput(ns("Title"))),
    hr(),
    fluidRow(
      column(4, DT::dataTableOutput(ns("CourseTable"))),
      column(8, plotOutput(ns("GradeDist")))
      )
    #,
    #verbatimTextOutput(ns("status"))
  )
  
  #tagList(
  #  DT::dataTableOutput(ns("CourseTable")),
  #  verbatimTextOutput(ns("status"))
  #)
}

# Module Server Function
courseWidget <- function(input, output, session, course_data, .when, profile_course) {
  ns <- session$ns
  vals = reactiveValues(selected = NULL, courseInstances = NULL)

  #selected <- eventReactive(input$CourseTable_rows_selected, {
  #  s <- input$CourseTable_rows_selected
  #  course_table()[s,]$Grade_Course
  #})
  
  courseInstances <- eventReactive(
    length(input$CourseTable_rows_selected) > 0, 
    {
      vals$selected <- course_table()[input$CourseTable_rows_selected,]$Grade_Course
      filter(course_data(), 
                                  Grade_Course == vals$selected, 
                                  !is.null(Grade_Final_Grade), 
                                  Grade_Final_Grade %in% valid_grades)

    #message("Ignoring date range ", paste(vals$dateRange, collapse =", "))
    #showModal(modalDialog(
    #  plotOutput(ns("GradeDist")),
    #  footer = actionButton(ns("dismiss_modal"), label = "Dismiss")
    #))
  })
  
  observeEvent(input$dismiss_modal, {
    removeModal()
    #vals$selected <- NA
    #vals$courseInstances <- NA
  })
  
  course_table <- eventReactive({
    isTruthy(course_data()) & isTruthy(profile_course())
  }, {
    message("calculating course_table with ", nrow(course_data()), " rows of course data and ", n_distinct(profile_course()), " unique IDS in the profile course.")
    profileIDS <- distinct(profile_course(), IDS)$IDS
    Ntotal <- length(profileIDS)
    course_data() %>%
      filter(when == .when,
             IDS %in% profileIDS) %>%
      group_by(Grade_Course) %>%
      summarize(Title = first(Grade_Course_Title),
                N = n_distinct(IDS),
                pct = N / Ntotal) %>%
      arrange(-N) %>% filter(pct > 0.10)    
  })
  
  output$CourseTable <- {
    DT::renderDataTable({
      message("Rendering output$CourseTable for ", session$id, " when = ", .when)
      if (is.null(course_data())) return()
      datatable(course_table(),
                                   options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
                                   rownames = FALSE,
                                   selection = "single") %>% formatPercentage('pct', 2)},
                                            server = FALSE)
  }
  #output$status <- renderPrint(selectedCourse())
  
  #output$status <- renderPrint(paste0(selected(), " selected"))
  
  output$GradeDist <- renderPlot({
    data <- courseInstances()
    if (!is.null(data) & nrow(data) > 0) {
      grade_distribution(data)
    }
  })
    
  return(reactive(vals$selected))
}