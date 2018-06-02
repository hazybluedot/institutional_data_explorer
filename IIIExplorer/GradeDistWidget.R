# Grade Distribution Widget

library(shiny)

gradeDistributionUI <- function(id, label = "Grade Distribution") {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("GradeDist")),
    verbatimTextOutput(ns("status"))
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
                                             mutate(Grade = convert_grades_letter(Grade_Final_Grade))
                                         })

  output$status <- renderPrint(paste0("grouping by ", groupBy()))
  
  output$GradeDist <- renderPlot({
    groupingVar <- if (is.reactive(groupBy)) {
      groupBy()
    } else {
      NULL
    }
    message("GradeDist nrow(course_first_instance): ", nrow(course_first_instance()), ", names(course_first_instance): ", paste(names(course_first_instance()), collapse = ", "))
    grade_distribution(course_first_instance(), groupingVar)
  })
  
  #return(reactive(input$DateRange))
}