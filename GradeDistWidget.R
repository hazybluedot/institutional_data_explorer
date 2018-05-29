# Grade Distribution Widget

library(shiny)

gradeDistributionUI <- function(id, label = "Grade Distribution") {
  ns <- NS(id)
  
  minTerm <- Sys.Date() - 10
  maxTerm <- Sys.Date()
  
  tagList(
    plotOutput(ns("GradeDist")),
    verbatimTextOutput(ns("status")),
    sliderInput(ns("DateRange"), "Date Range", min = minTerm, 
                max = maxTerm, 
                value = c(minTerm, maxTerm), 
                dragRange = TRUE,
                timeFormat = "%b %Y")
  )
}

gradeDistribution <- function(input, output, session, course_data, groupedIDS) {
  vals <- reactiveValues(courseInstances = NULL, groupingVar = NULL)
  
  observeEvent(course_data(), {
    if (is.data.frame(course_data())) {
      terms <- unique(course_data()$Banner_Term)
      minTerm <- min(terms, na.rm = TRUE)
      maxTerm <- max(terms, na.rm = TRUE)
      if (!is.na(minTerm) & !is.na(maxTerm)) {
        updateSliderInput(session, "DateRange", 
                          min = minTerm, 
                          max = maxTerm,
                          value = c(minTerm, maxTerm))
      }
    }
  })
  
  observeEvent(groupedIDS(), {
    .groupedBy <- groupedIDS()
    message("adding grouping variable ", paste(.groupedBy$name, collapse = ", "))
    vals$courseInstances <- vals$courseInstances %>% 
      mutate(group = if_else(IDS %in% .groupedBy$IDS, .groupedBy$name, "other"))
    vals$groupingVar = "group"
  })
  
  observeEvent(input$DateRange, {
    vals$courseInstances <- filter(course_data(), 
            Banner_Term >= input$DateRange[1],
            Banner_Term <= input$DateRange[2])
  })
  
  #output$status <- renderPrint(paste0("grouping by ", paste(groupedIDS()$IDS, collapse = ", ")))
  
  output$GradeDist <- renderPlot({
    .courseInstances <- vals$courseInstances
    #if (length(.groupedBy$IDS)) {
      #.courseInstances <- .courseInstances %>% mutate(
      #  group = if_else(IDS %in% .groupedBy$IDS, .groupedBy$name, "other")
      #)
    #}
    if (nrow(.courseInstances) > 0) {
      grade_distribution(.courseInstances, vals$groupingVar)
    }
  })
  
  return(reactive(input$DateRange))
}