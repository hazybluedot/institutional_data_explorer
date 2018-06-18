# Grade Distribution Widget

library(shiny)

gradeDistributionUI <- function(id, label = "Grade Distribution") {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("GradeDist")),
    uiOutput(ns("status"))
  )
}


gradeDistribution <-
  function(input,
           output,
           session,
           course_data,
           groupBy = NULL,
           grouping_table = NULL) {
    vals <- reactiveValues(invalid = 0)
    noteID <- NULL
    
    course_first_instance <- reactive({
      req(course_data())
      #message("GradeDist names(course_data): ", paste(names(course_data()), collapse = ", "))
      data <-
        left_join(course_data(),
                  first_instance(course_data()),
                  by = "IDS") %>% filter(Banner_Term == First_Taken) %>%
        mutate(Grade = collapse_letter_grade(Grade_Final_Grade))
      
      groupVar <- groupBy()
      if (groupVar == "group") {
        if (is.reactive(grouping_table)) {
          data <- left_join(data, grouping_table(), by = "IDS")
        }
      }
      
      if (isTruthy(groupVar) & groupVar %in% names(data)) {
        idx <- data[, groupBy()]
        vals$invalid <- sum(is.na(idx))
        valid <-
          which(!is.na(idx))
        data <- data[valid, ]
      } else {
        vals$invalid = 0
      }
      # if (groupVar == "group" &
      #     "group" %in% names(data) &
      #     length(unique(data$group)) < 2) {
      #   noteID <-
      #     showNotification(
      #       "No course selected. Select a course from the 'Taken Before' tab to compare.",
      #       type = "warning",
      #       duration = 5
      #     )
      # } else {
      #   if (!is.null(noteID))
      #     removeNotification(noteID)
      # }
      
      data
    })
    
    output$status <- renderUI({
      if (vals$invalid) {
        div(
          class = "shinyalert alert alert-warning",
          paste0(
            "Removed ",
            vals$invalid,
            " rows where ",
            groupBy(),
            " data was missing."
          )
        )
      }
    })
    
    output$GradeDist <- renderPlot({
      # shiny::validate(need(nrow(course_first_instance()) > 0, "Need a valid course table"))
      # groupingVar <- if (is.reactive(groupBy)) {
      #   groupBy()
      # } else {
      #   NULL
      # }
      grade_distribution(course_first_instance(), groupBy())
    })
    
    #return(reactive(input$DateRange))
  }