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
  
  courseInstances <- eventReactive({
    if (length(input$CourseTable_rows_selected) > 0) TRUE
    else return()
    #(length(input$CourseTable_rows_selected) > 0)
    }, {
      vals$selected <- course_table()[input$CourseTable_rows_selected,]$Grade_Course
      profileIDS <- distinct(profile_course(), IDS)$IDS
      filter(course_data(), 
                          Grade_Course == vals$selected, 
                          when == .when,
                          IDS %in% profileIDS,
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
      if (is.null(course_data())) return()
      datatable(course_table(),
                                   options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE),
                                   rownames = FALSE,
                                   selection = "single") %>% formatPercentage('pct', 2)},
                                            server = FALSE)
  }
  #output$status <- renderPrint(selectedCourse())
  
  #output$status <- renderPrint(paste0(selected(), " selected"))
  
  callModule(gradeDistribution, "GradeDist", courseInstances, reactive(NA))
  #output$GradeDist <- renderPlot({
  #  grade_distribution(courseInstances())
  #})
    
  return(reactive(vals$selected))
}