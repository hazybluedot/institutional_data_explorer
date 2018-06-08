shinyServer(function(input, output, session) {
  vals <- reactiveValues(profile_course = NULL, 
                         groups = list(IDS = c(), names = c("", "")),
                         course_instances = NULL,
                         courses_with_profile = NULL,
                         Ntotal = NA,
                         valid = FALSE)
  
  observe({
    subjects <- course_list %>% 
      distinct(Course_Subject)
    updateSelectInput(session, "subject", choices = subjects)
    grouping_vars <- student_data %>% select_if(~(is.factor(.) & length(levels(.)) <= 3)) %>% names()
    updateSelectInput(session, "groupBy", choices = c("None" = as.character(NA), "Selected Course" = "group", grouping_vars))
    date_range <- as.numeric(format(range(course_data$Banner_Term), "%Y"))
    message('updating slider to range ', paste(date_range, collapse = " - "))
    updateSliderInput(session, "termRange", 
                      min = date_range[1], 
                      max = date_range[2], 
                      value = c(date_range[1], date_range[2])) 
  })
  
  observeEvent(input$subject, {
    updateSelectInput(session, "number", choices = course_list %>% 
                        filter(Course_Subject == input$subject) %>% 
                        distinct(Course_Number))
  })
  
  dateRange <- eventReactive(input$termRange, {
    numeric_to_term(input$termRange)
  })
  
  observeEvent({ 
    input$number
  }, {
    vals$profile_course <- paste(input$subject, input$number, sep = "_")
  }, ignoreNULL = TRUE)
  
  observeEvent({
    if (vals$profile_course %in% unique(course_data$Grade_Course) & isTruthy(dateRange())) TRUE
    else return()
  }, {
    vals$course_title <- first(as.character(filter(course_data, Grade_Course == vals$profile_course)$Grade_Course_Title))
    message("User selected ", vals$profile_course, ", ", vals$course_title)
    
    
    vals$course_instances <- filter(course_data, 
                                    Grade_Course == vals$profile_course,
                                    Banner_Term >= dateRange()[1],
                                    Banner_Term <= dateRange()[2],
                                    !is.null(Grade_Final_Grade),
                                    Grade_Final_Grade %in% valid_grades) %>%
      mutate(group = "none") %>%
      left_join(student_data, by = "IDS")
    
    vals$first_instance <- first_instance(vals$course_instances)
    
    vals$courses_with_profile <- add_neighbor_courses(course_data %>% 
                                                        filter(Grade_Course != vals$profile_course,
                                                               Banner_Term >= dateRange()[1], 
                                                               Banner_Term <= dateRange()[2]),
                                                      vals$first_instance) %>% select(-First_Taken) %>%
      left_join(student_data, by = "IDS")
    s <- vals$courses_with_profile %>% group_by(when) %>% 
      dplyr::summarize(n = n())
    
    vals$Ntotal <- n_distinct(vals$course_instances$IDS)
  })
  
  observeEvent({
    if (isTruthy(vals$groups) & !is.null(vals$course_instances)) TRUE
    else return()
  },{
    .groupBy <- vals$groups
    vals$course_instances <- vals$course_instances %>%
      mutate(group = if_else(IDS %in% .groupBy$IDS, .groupBy$names[1], .groupBy$names[2]))
  })
  
  observeEvent({
    if (length(neighbor_before()) > 0) TRUE
    else return ()
  }, {
    #message("neighbor_before is ", neighbor_before(), " type/class: ", typeof(neighbor_before()), "/", class(neighbor_before()))
    course <- neighbor_before()
    #if (!is.na(course)) {
    took_selected_before <- vals$courses_with_profile %>%
      filter(Grade_Course == course, when == "before")
    #vals$course_instances <- vals$course_instances %>% mutate(
    #  group = if_else(IDS %in% took_selected_before$IDS, "Took before", "Did not take")
    #)
    vals$groups <- list(IDS = took_selected_before$IDS, names = c(paste0("Took ", course, " before"), "Did not take before"))
    vals$course_instances <- vals$course_instances %>%
      mutate(group = if_else(IDS %in% vals$groups$IDS, vals$groups$names[1], vals$groups$names[2]))
    #}
  })
  
  output$CourseTitle <- renderText(vals$course_title)
  
  #output$status <- renderPrint(if (!is.na(input$groupBy)) paste0("Grouping by ", input$groupBy))
  
  callModule(gradeDistribution, "GradeDist", reactive(vals$course_instances), reactive(input$groupBy))
  #callModule(successAnalysis, "SuccessAnalysis", reactive(vals$courses_with_profile), reactive(vals$course_instances), reactive(input$groupBy), neighbor_before)
  
  neighbor_before <- callModule(courseWidget,
                                "CoursesBefore",
                                reactive(vals$courses_with_profile),
                                "before",
                                reactive(vals$first_instance),
                                reactive(input$groupBy))
  neighbor_with <- callModule(courseWidget,
                              "CoursesWith",
                              reactive(vals$courses_with_profile),
                              "with",
                              reactive(vals$first_instance),
                              reactive(input$groupBy))
  neighbor_after <- callModule(courseWidget,
                               "CoursesAfter",
                               reactive(input$courses_with_profile),
                               "after",
                               reactive(vals$first_instance),
                               reactive(vals$groupBy))
  
  #output$status  <- renderPrint(paste0(neighbor_before(), " selected"))
  
  output$DegreesAfter <- DT::renderDataTable({
    if (is.null(vals$course_instances)) return()
    message("Rendering degree table for ", nrow(vals$course_instances), " rows.")
    profile_course_first_instance <- first_instance(vals$course_instances)
    degrees_first_instance_summary <- degrees %>%
      filter(IDS %in% profile_course_first_instance$IDS,
             Degree_category == "Primary Major",
             grepl("^B", Degree)) %>%
      group_by(Degree_major) %>%
      #semi_join(degrees, course.ID.and.grade, by="IDS") %>% filter(grepl("B",Degree)) %>%
      dplyr::summarize(Ntotal=n_distinct(IDS)) %>%
      arrange(-Ntotal) %>%
      head(n = params$ndegrees)
  },
  options = list(searching = FALSE,
                 lengthChange = FALSE),
  selection = "single",
  server = FALSE)
})