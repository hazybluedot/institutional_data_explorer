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
      grouping_vars <- c("Grade")
      if (groupBy() != "none") {
        grouping_vars <- c(grouping_vars, groupBy())
      }
      grouped_data <- course_first_instance() %>% group_by_at(grouping_vars)
      shiny::validate(need(min(attr(grouped_data, 'group_sizes')) >= 10, 
                           "Smallest group size is less than 10. Increase the filter scope to view the grade distribution."))
      grade_distribution(grouped_data)
    })
    
    #return(reactive(input$DateRange))
  }