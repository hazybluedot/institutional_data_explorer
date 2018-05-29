# Grade Distribution Widget

library(shiny)

gradeDistributionUI <- function(id, label = "Grade Distribution") {
  ns <- NS(id)
  
  minTerm <- Sys.Date() - 10
  maxTerm <- Sys.Date()
  
  tagList(
    plotOutput(ns("GradeDist")),
    verbatimTextOutput(ns("status"))
  )
}

gradeDistribution <- function(input, output, session, course_data, groupBy) {
  vals <- reactiveValues(courseInstances = NULL, groupingVar = NULL)
  
  course_first_instance <- eventReactive(course_data(),
                                         {    
                                           left_join(course_data(), 
                                                     first_instance(course_data()),
                                                     by = "IDS") %>% filter(Banner_Term == First_Taken)
                                           })

  #output$status <- renderPrint(paste0("grouping by ", paste(groupedIDS()$IDS, collapse = ", ")))
  
  output$GradeDist <- renderPlot({
    grade_distribution(course_first_instance(), NULL)
  })
  
  #return(reactive(input$DateRange))
}