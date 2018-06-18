course_data <- reactiveFileReader(10000, NULL, course_fname, read_course_file)
student_data <- reactiveFileReader(10000, NULL, student_fname, read_student_file)
degree_data <- reactiveFileReader(10000, NULL, degree_fname, read_degree_file)

shinyServer(function(input, output, session) {
  vals <- reactiveValues(profile_course = NULL, 
                         groups = list(IDS = c(), names = c("", "")),
                         dateRange = c(),
                         collegeFilter = NULL,
                         majorFilter = NULL,
                         course_instances = NULL,
                         courses_with_profile = NULL,
                         valid = FALSE)

  resetDateSlider <- function(session) {
    date_range <- as.numeric(format(range(course_data()$Banner_Term), "%Y"))
    updateSliderInput(session, "termRange", 
                      min = date_range[1], 
                      max = date_range[2], 
                      value = c(date_range[1], date_range[2]))
    return(date_range)
  }
  
  observeEvent(course_data(), {
    ## Set min and max date based on data
    resetDateSlider(session)
  })
  
  observeEvent(input$termRange, {
    vals$dateRange <- numeric_to_term(input$termRange)
  })

  observeEvent(input$collegeFilter, {
    vals$collegeFilter <- input$collegeFilter
  })
  
  observeEvent(input$majorFilter, {
    vals$majorFilter <- input$majorFilter
  })

  filtered_course_data <- eventReactive(c(
    course_data(),
    vals$collegeFilter,
    vals$majorFilter,
    vals$dateRange
    ), {
      filtered_data <- course_data() %>% filter(Banner_Term >= vals$dateRange[1],
                                                Banner_Term <= vals$dateRange[2])
      
      if (isTruthy(vals$collegeFilter)) {
        .IDS <- (course_data() %>% filter(College_desc %in% vals$collegeFilter) %>% distinct(IDS))$IDS
        filtered_data <- filtered_data %>% filter(IDS %in% .IDS)
      }
      
      if (isTruthy(vals$majorFilter)) {
        .IDS <- (course_data() %>% filter(Major %in% vals$majorFilter) %>% distinct(IDS))$IDS
        filtered_data <- filtered_data %>% filter(IDS %in% .IDS)
      }
      message("Filtered data contains ", n_distinct(filtered_data$IDS), " distinct students")
      return(filtered_data)
  })
  
  observeEvent(filtered_course_data(), {
    vals$course_list <-  filtered_course_data() %>%
      filter(Registered_credit_hours > 0) %>%
      separate(Grade_Course, c("Course_Subject", "Course_Number"), remove = FALSE) %>% 
      select(Grade_Course, Course_Subject, Course_Number, Grade_Course_Title) %>% 
      na.omit() %>% distinct() %>% mutate(Course_ID = stringr::str_replace(Grade_Course, "_", " ")) %>% arrange(Course_Subject, Course_Number) 
    #subjects <- vals$course_list %>% 
    #  distinct(Course_Subject)
    #updateSelectInput(session, "subject", choices = subjects)
    #tokens <- 
    updateTextInput.typeahead(session, "profile_course", 
                              vals$course_list, 
                              valueKey = "Grade_Course",
                              tokens = paste(vals$course_list$Course_Subject, vals$course_list$Course_Number, sep = ""),
                              placeholder = "e.g. ESM 2204",
                              template = "<p class = 'repo-language'>{{Course_Subject}} {{Course_Number}}<p> 
                              <p class = 'repo-description'>{{Grade_Course_Title}}</p>"
                              )
  })
  
  observeEvent(student_data(), {
    grouping_vars <- student_data() %>% select_if(~(is.factor(.) & length(levels(.)) <= 3)) %>% names()
    updateSelectInput(session, "groupBy", choices = c("None" = as.character(NA), "Selected Course" = "group", grouping_vars))
    #updateCheckboxGroupInput(session, "demographicFilter", choices = c("All", grouping_vars))
  })

  observeEvent({
    if (input$profile_course %in% unique(filtered_course_data()$Grade_Course) & isTruthy(vals$dateRange)) TRUE
    else return()
  }, {
    vals$course_title <- first(as.character(filter(filtered_course_data(), Grade_Course == input$profile_course)$Grade_Course_Title))
    message("User selected ", input$profile_course, ", ", vals$course_title)
    
    
    vals$course_instances <- filter(filtered_course_data(), 
                                    Grade_Course == input$profile_course,
                                    !is.null(Grade_Final_Grade),
                                    Grade_Final_Grade %in% valid_grades) %>%
      mutate(group = "none") %>%
      left_join(student_data(), by = "IDS")
    
    vals$first_instance <- first_instance(vals$course_instances)
    
    vals$courses_with_profile <- add_neighbor_courses(filtered_course_data() %>% 
                                                        filter(Grade_Course != input$profile_course),
                                                      vals$first_instance) %>% select(-First_Taken) %>%
      left_join(student_data(), by = "IDS")
    s <- vals$courses_with_profile %>% group_by(when) %>% 
      dplyr::summarize(n = n())
  })
  
  observeEvent(neighbor_before(), {
    course <- neighbor_before()
    if (length(course) > 0) { 
    #message("neighbor_before is ", neighbor_before(), " type/class: ", typeof(neighbor_before()), "/", class(neighbor_before()))
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
    } else {
      vals$course_instances <- vals$course_instances %>%
        mutate(group = "none")
    }
  })
  
  observeEvent(input$resetInputs, {
    updateSelect2Input(session, "majorFilter", "is this uses?", choices = c())
    updateSelect2Input(session, "collegeFilter", "is this uses?", choices = c())
    updateSelectInput(session, "groupBy", selected = "none")
    
    vals$majorFilter = NULL
    vals$collegeFilter = NULL
    vals$dateRange <- numeric_to_term(resetDateSlider(session))
  })

  output$nCourses <- renderText(nrow(filtered_course_data()))
  
  output$nStudents <- renderText(n_distinct(filtered_course_data()$IDS))
  
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
                               reactive(vals$courses_with_profile),
                               "after",
                               reactive(vals$first_instance),
                               reactive(input$groupBy))
  
  #output$status  <- renderPrint(paste0(neighbor_before(), " selected"))
  
  output$DegreesAfter <- DT::renderDataTable({
    if (is.null(vals$course_instances)) return()
    #message("Rendering degree table for ", nrow(vals$course_instances), " rows.")
    profile_course_first_instance <- first_instance(vals$course_instances)
    degrees_first_instance_summary <- degree_data() %>%
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