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
      #message("GradeDist names(course_data): ", paste(names(first_instance(course_data())), collapse = ", "))
      data <-
        first_instance(course_data()) %>%
        #course_data() %>%
        mutate(grade = collapse_letter_grade(final_grade)) %>%
        filter(grade %in% c("A", "B", "C", "D", "F", "W", "T"))
        
      
      groupVar <- groupBy()
      if (groupVar == "group") {
        if (is.reactive(grouping_table)) {
          data <- left_join(data, grouping_table(), by = "id")
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
      grouping_vars <- c("grade")
      if (groupBy() != "none" & groupBy() %in% names(course_first_instance())) {
        grouping_vars <- c(grouping_vars, groupBy())
      }
      
      #message("using grouping variable ", groupBy(), " and grouping data by c(", paste(grouping_vars, collapse = ", "), ").")
      grouped_data <- course_first_instance() %>% group_by_at(grouping_vars)
      
      #message("group_sizes: ", paste(attr(grouped_data, 'group_sizes'), collapse = ", "))
      # dplyr conveniently calculates group sizes as part of group_by, and adds
      # this information as an attribute to the data frame.
      shiny::validate(need(min(attr(grouped_data, 'group_sizes')) >= params$min_bin_size,
                          paste0("Smallest group size is less than ", params$min_bin_size, ". Increase the filter scope to view the grade distribution.")))
      # TODO: we should move '10' into a config file to avoid magic numbers sprinkled around the codebase
      grade_distribution(grouped_data)
    })
  }