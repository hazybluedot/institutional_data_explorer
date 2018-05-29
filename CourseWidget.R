library(shiny)

# Course List and quick preview module

courseWidgetUI <- function(id, label = "Course Preview", header = NULL) {
  ns <- NS(id)
  
  tagList(
    h2(textOutput(ns("Title"))),
    hr(),
    DT::dataTableOutput(ns("CourseTable"))#,
    #verbatimTextOutput(ns("status"))
  )
  
  #tagList(
  #  DT::dataTableOutput(ns("CourseTable")),
  #  verbatimTextOutput(ns("status"))
  #)
}

# Module Server Function
courseWidget <- function(input, output, session, title, course_data, course_table, term_range) {
  ns <- session$ns
  vals = reactiveValues(selected = NULL, courseInstances = NULL)
  #vals$course_data <- course_data
  
  #observe({
  #  vals$course_table <- course_table()
  #})
  
  selected <- eventReactive(input$CourseTable_rows_selected, {
    s <- input$CourseTable_rows_selected
    message("Number of rows selected: ", length(s))
    if (length(s)) {
      course_table()[s,]$Grade_Course
    } else {
      NA
    }
  })
  
    observeEvent(input$CourseTable_rows_selected, {
    if (length(input$CourseTable_rows_selected) == 1) {
      vals$selected <- course_table()[input$CourseTable_rows_selected,]$Grade_Course
    } else {
      vals$selected <- "none"
    }
    termRange <- term_range()
    vals$courseInstances = filter(course_data, Grade_Course == vals$selected, 
                                  !is.null(Grade_Final_Grade), 
                                  Grade_Final_Grade %in% valid_grades,
                                  Banner_Term >= termRange[1],
                                  Banner_Term <= termRange[2])

    #message("Ignoring date range ", paste(vals$dateRange, collapse =", "))
    showModal(modalDialog(
      plotOutput(ns("GradeDist")),
      footer = actionButton(ns("dismiss_modal"), label = "Dismiss")
    ))
  })
  
  observeEvent(input$dismiss_modal, {
    removeModal()
    #vals$selected <- NA
    #vals$courseInstances <- NA
  })
  
  output$CourseTable <- {
     DT::renderDataTable(datatable(course_table(), 
                                   options = list(searching = FALSE, lengthChange = FALSE),
                                   rownames = FALSE,
                                   selection = "single") %>% formatPercentage('pct', 2), 
                                            server = FALSE)
  }
  #output$status <- renderPrint(selectedCourse())
  
  output$Title <- renderText(title)
  output$status <- renderPrint(paste0(selected(), " selected"))
  
  output$GradeDist <- renderPlot({
    if (!is.na(vals$courseInstances) & nrow(vals$courseInstances) > 0) grade_distribution(vals$courseInstances)
  })
    
  return(selected)
}