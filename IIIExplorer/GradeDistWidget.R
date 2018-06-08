# Grade Distribution Widget

library(shiny)

gradeDistributionUI <- function(id, label = "Grade Distribution") {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("GradeDist"))#,
    #verbatimTextOutput(ns("status"))
  )
}

gradeDistribution <- function(input, output, session, course_data, groupBy = NULL) {
  vals <- reactiveValues(courseInstances = NULL, groupingVar = NULL)
  
  course_first_instance <- eventReactive(course_data(),
                                         {
                                           #message("GradeDist names(course_data): ", paste(names(course_data()), collapse = ", "))
                                           left_join(course_data(), 
                                                     first_instance(course_data()),
                                                     by = "IDS") %>% filter(Banner_Term == First_Taken) %>%
                                             mutate(Grade = collapse_letter_grade(Grade_Final_Grade))
                                         })
  
  output$status <- renderPrint(paste0("grouping by ", groupBy()))
  
  output$GradeDist <- renderPlot({
    shiny::validate(need(nrow(course_first_instance()) > 0, "Need a valid course table"))
    groupingVar <- if (is.reactive(groupBy)) {
      groupBy()
    } else {
      NULL
    }

    grade_distribution(course_first_instance(), groupingVar)
  })
  
  #return(reactive(input$DateRange))
}