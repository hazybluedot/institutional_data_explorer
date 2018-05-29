shinyServer(function(input, output, session) {
  vals <- reactiveValues(profile_course = NULL, course_instances = NULL, valid = FALSE)
  
  observe({
    subjects <- course_list %>% 
      distinct(Course_Subject)
    updateSelectInput(session, "subject", choices = subjects)
    
  })
  
  observeEvent(input$subject, {
    updateSelectInput(session, "number", choices = course_list %>% 
                        filter(Course_Subject == input$subject) %>% 
                        distinct(Course_Number))
  })
  
  observeEvent(input$number, {
      vals$profile_course <- paste(input$subject, input$number, sep = "_")
      vals$course_title <- first(filter(course_data, Grade_Course == vals$profile_course)$Grade_Course_Title)
      vals$course_instances <- filter(course_data, Grade_Course == vals$profile_course,
                                      !is.null(Grade_Final_Grade),
                                      Grade_Final_Grade %in% valid_grades)
      message("User selected ", vals$profile_course, ", ", vals$course_title)
  }, ignoreNULL = TRUE)
  
  observeEvent(term_range(), {
    term_range <- term_range()
    message("updated date range: ", paste0(term_range, collapse = ", "))
    vals$courses_with_profile <- with_neighbor_courses(vals$course_instances %>% 
                                                         filter(Banner_Term >= term_range[1], 
                                                                Banner_Term <= term_range[2]))
    Ntotal <- n_distinct(first_instance(vals$course_instances)$IDS)
    vals$neighbor_courses <- fetch_neighbor_courses(vals$courses_with_profile, vals$profile_course, Ntotal, params$ncourses)
  })
  
  groupedIDS <- eventReactive(neighbor_before(), {
    course <- neighbor_before()
    if (!is.na(course)) {
      took_selected_before <- vals$courses_with_profile %>% 
        filter(Grade_Course == course, when == "before")
      #vals$course_instances <- vals$course_instances %>% mutate(
      #  group = if_else(IDS %in% took_selected_before$IDS, "Took before", "Did not take")
      #)
      list(IDS = took_selected_before$IDS, name = paste0("Took ", course, " before"))
    }
  })
  

  output$CourseTitle <- renderPrint(vals$course_title)
  
  term_range <- callModule(gradeDistribution, "GradeDist", reactive(vals$course_instances), groupedIDS)

  neighbor_before <- callModule(courseWidget, "CoursesBefore", "Taken Before", course_data, reactive(vals$neighbor_courses$before), term_range)
  neighbor_with <- callModule(courseWidget, "CoursesWith", "Taken With", course_data, reactive(vals$neighbor_courses$with), term_range)
  neighbor_after <- callModule(courseWidget, "CoursesAfter", "Taken After", course_data, reactive(vals$neighbor_courses$after), term_range)
  
  output$status  <- renderPrint(paste0(neighbor_before(), " selected"))

    output$DegreesAfter <- DT::renderDataTable({
    profile_course_first_instance <- first_instance(vals$course_instances)
    degrees_first_instance_summary <- degrees %>% 
      filter(IDS %in% profile_course_first_instance$IDS,
             Degree_category == "Primary Major",
             grepl("^B", Degree)) %>%
      group_by(Degree_major) %>%
      #semi_join(degrees, course.ID.and.grade, by="IDS") %>% filter(grepl("B",Degree)) %>% 
      summarize(Ntotal=n_distinct(IDS)) %>%
      arrange(-Ntotal) %>%
      head(n = params$ndegrees)
  }, 
  options = list(searching = FALSE, 
                 lengthChange = FALSE), 
  selection = "single",
  server = FALSE)
})