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
  
  selected_course_first_instances <- reactive({
    req(selected())  
    profileIDS <- unique(profile_course()$id)
    instances <- filter(course_data(), 
                          course == selected(), 
                          when == .when,
                          id %in% profileIDS,
                          !is.null(final_grade), 
                          final_grade %in% valid_grades)
    first_instance(instances)
  })
  
  course_table <- reactive({
    profileIDS <- unique(profile_course()$id)
    Ntotal <- length(profileIDS)
    
    course_data() %>%
      filter(when == .when,
             id %in% profileIDS) %>%
      group_by(course) %>%
      dplyr::summarize(Title = dplyr::first(grade_title),
                N = n_distinct(id),
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

  callModule(gradeDistribution, "GradeDist", selected_course_first_instances, groupingVar)

  return(selected)
}