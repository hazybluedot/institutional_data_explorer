# Grade Distribution Widget

library(shiny)

gradeDistributionUI <- function(id, label = "Grade Distribution") {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("GradeDist"))#,
    #verbatimTextOutput(ns("status"))
  )
}


gradeDistribution <-
  function(input,
           output,
           session,
           course_data,
           groupBy = NULL) {
    vals <- reactiveValues(courseInstances = NULL, groupingVar = NULL)
    noteID <- NULL
    
    course_first_instance <- eventReactive({
      #if (isTruthy(course_data()) & (is.null(groupBy) | isTruthy(groupBy()))) TRUE
      #else return()
      course_data()
      groupBy()
      },
                                           {
                                             req(course_data())
                                             #message("GradeDist names(course_data): ", paste(names(course_data()), collapse = ", "))
                                             groupVar <- groupBy()
                                             data <- left_join(course_data(),
                                                       first_instance(course_data()),
                                                       by = "IDS") %>% filter(Banner_Term == First_Taken) %>%
                                               mutate(Grade = collapse_letter_grade(Grade_Final_Grade))
                                             if (isTruthy(groupVar) & groupVar != "NA" & groupVar %in% names(data)) {
                                               idx <- data[,groupVar]
                                               retVal <- which(!is.na(idx))
                                               data <- data[retVal,]
                                               if (length(retVal) > 0) {
                                                 vals$message = paste0("Filtered out ", length(retVal), " records with missing ", groupVar, " data.")
                                               }
                                             }
                                             if (groupVar == "group" &  
                                                 "group" %in% names(data) & 
                                                 length(unique(data$group)) < 2) {
                                               noteID <- showNotification("No course selected. Select a course from the 'Taken Before' tab to compare.", 
                                                                          type = "warning", duration = 5) 
                                             } else {
                                               if (!is.null(noteID)) removeNotification(noteID)
                                             }
                                             
                                             data
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