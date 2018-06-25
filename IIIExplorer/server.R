#library(rlang)

course_data <- reactiveFileReader(10000, NULL, course_fname, read_course_file)
student_data <- reactiveFileReader(10000, NULL, student_fname, read_student_file)
degree_data <- reactiveFileReader(10000, NULL, degree_fname, read_degree_file)

shinyServer(function(input, output, session) {
  vals <- reactiveValues(profile_course = NULL, 
                         dateRange = c(),
                         collegeFilter = NULL,
                         majorFilter = NULL,
                         course_instances = NULL,
                         courses_with_profile = NULL,
                         grouping_course = NULL,
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

  # reactiveValues is used here because the updateSelect2Input, used for reset
  # button action, does not seem to let shiny see that the input$collegeFilter
  # has changed.
  observe({ vals$collegeFilter <- input$collegeFilter })
  observe({ vals$majorFilter <- input$majorFilter })

  filtered_course_data <- reactive({
    req(as.logical(all(input$filterBoolean %in% c("&", "|"))))
    filtered_data <-
      course_data() %>% filter(Banner_Term >= vals$dateRange[1],
                               Banner_Term <= vals$dateRange[2])
    
    filter_parts <- c()
    if (isTruthy(vals$collegeFilter)) {
      filter_parts <-
        c(filter_parts, "College_desc %in% vals$collegeFilter")
    }
    
    if (isTruthy(vals$majorFilter)) {
      filter_parts <- c(filter_parts, "Major %in% vals$majorFilter")
    }
    
    if (length(filter_parts) > 0) {
      .IDS <-
        (filter(course_data(),!!!rlang::parse_exprs(
          paste(filter_parts, collapse = input$filterBoolean)
        )))$IDS
      filtered_data <- filtered_data %>% filter(IDS %in% .IDS)
    }
    
    if (input$hasDegree) {
      filtered_degrees <-
        degree_data() %>% filter(IDS %in% filtered_data$IDS)
      if (length(filter_parts) > 0) {
        message("filtering degrees")
        filtered_degrees <-
          filtered_degrees %>% filter(!!!rlang::parse_exprs(paste(filter_parts, collapse = " | ")))
      }
      filtered_data <-
        filter(filtered_data, IDS %in% filtered_degrees$IDS)
      }
    
    return(filtered_data)
  })
  
  course_list <- reactive({
    filtered_course_data() %>%
      filter(Registered_credit_hours > 0) %>%
      separate(Grade_Course, c("Course_Subject", "Course_Number"), remove = FALSE) %>% 
      select(Grade_Course, Course_Subject, Course_Number, Grade_Course_Title) %>% 
      na.omit() %>% distinct() %>% mutate(Course_ID = stringr::str_replace(Grade_Course, "_", " ")) %>% arrange(Course_Subject, Course_Number) 
  })
  
  observe({
    updateTextInput.typeahead(session, "profile_course", 
                              course_list(), 
                              valueKey = "Grade_Course",
                              tokens = paste(course_list()$Course_Subject, course_list()$Course_Number, sep = ""),
                              placeholder = "e.g. ESM 2204",
                              template = "<p class = 'repo-language'>{{Course_Subject}} {{Course_Number}}<p> 
                              <p class = 'repo-description'>{{Grade_Course_Title}}</p>"
                              )
    updateTextInput.typeahead(session, "compareCourse",
                              course_list(),
                              valueKey = "Grade_Course",
                              tokens = paste(course_list()$Course_Subject, course_list()$Course_Number, sep = ""),
                              placeholder = "e.g. ESM 2104",
                              template = "<p class = 'repo-language'>{{Course_Subject}} {{Course_Number}}<p> 
                              <p class = 'repo-description'>{{Grade_Course_Title}}</p>")
  })
  
  observe({
    grouping_vars <- student_data() %>% select_if(~(is.factor(.) & length(levels(.)) <= 3)) %>% names()
    updateSelectInput(session, "groupBy", choices = c("None" = "none", "Selected Course" = "group", grouping_vars))
    #updateCheckboxGroupInput(session, "demographicFilter", choices = c("All", grouping_vars))
  })

  course_instances <- reactive({
    req(input$profile_course)
    vals$course_instances <- filter(
      filtered_course_data(),
      Grade_Course == input$profile_course,
      !is.null(Grade_Final_Grade),
      Grade_Final_Grade %in% valid_grades
    ) %>%
      left_join(student_data(), by = "IDS")
  })
  
  profile_course_first_instance <- reactive({ 
    first_instance(course_instances()) 
  })
  
  course_grouping <- reactive({ #})
  #observe({
    req(vals$grouping_course)
    
    took_selected_before <- courses_with_profile() %>%
      filter(Grade_Course == vals$grouping_course, when == "before")
    .IDS <- took_selected_before$IDS
    
    course_instances() %>%
      mutate(group = if_else(IDS %in% .IDS,
                             paste0("Took ", vals$grouping_course, " before"),
                             paste0("Did not take ", vals$grouping_course, " before"))) %>%
      select(IDS, group)
  })

  courses_with_profile <- reactive({
    req(input$profile_course)
    add_neighbor_courses(filtered_course_data() %>% 
                           filter(Grade_Course != input$profile_course),
                         profile_course_first_instance()) %>% select(-First_Taken) %>%
      left_join(student_data(), by = "IDS")
  })
  
  observe({ vals$grouping_course <- input$compareCourse })
  observe({ 
    vals$grouping_course <- neighbor_before() 
    #updateTextInput(session, "compareCourse", value = vals$grouping_course)
  })
  
  observeEvent(input$resetInputs, {
    updateSelect2Input(session, "majorFilter", "is this uses?", choices = NULL)
    updateSelect2Input(session, "collegeFilter", "is this uses?", choices = NULL)
    updateSelectInput(session, "groupBy", selected = "none")
    updateCheckboxInput(session, "hasDegree", value = FALSE)
    
    vals$majorFilter = character(0)
    vals$collegeFilter = character(0)
    resetDateSlider(session)
  })

  output$nCourses <- renderText(nrow(filtered_course_data()))
  
  output$nStudents <- renderText(n_distinct(filtered_course_data()$IDS))
  
  output$CourseTitle <- renderText({
    ## technically this only needs to recompute on input$profile_course change, so it could be a 
    ## eventReactive(input$profile_course, {...})
    req(input$profile_course)
    first(as.character(
      filter(
        filtered_course_data(),
        Grade_Course == input$profile_course
      )$Grade_Course_Title
    ))
  })
  
  #output$status <- renderPrint(if (!is.na(input$groupBy)) paste0("Grouping by ", input$groupBy))
  
  callModule(gradeDistribution, "GradeDist", course_instances, reactive(input$groupBy), course_grouping)
  #callModule(successAnalysis, "SuccessAnalysis", reactive(vals$courses_with_profile), reactive(vals$course_instances), reactive(input$groupBy), neighbor_before)
  
  neighbor_before <- callModule(courseWidget,
                                "CoursesBefore",
                                courses_with_profile,
                                "before",
                                profile_course_first_instance,
                                reactive(input$groupBy))
  neighbor_with <- callModule(courseWidget,
                              "CoursesWith",
                              courses_with_profile,
                              "with",
                              profile_course_first_instance,
                              reactive(input$groupBy))
  neighbor_after <- callModule(courseWidget,
                               "CoursesAfter",
                               courses_with_profile,
                               "after",
                               profile_course_first_instance,
                               reactive(input$groupBy))
  
  #output$status  <- renderPrint(paste0(neighbor_before(), " selected"))
  
  output$DegreesAfter <- DT::renderDataTable({
    req(vals$course_instances)
    #message("Rendering degree table for ", nrow(vals$course_instances), " rows.")
    profile_course_first_instance <- first_instance(vals$course_instances)
    degrees_first_instance_summary <- degree_data() %>%
      filter(IDS %in% profile_course_first_instance$IDS,
             Degree_category == "Primary Major",
             grepl("^B", Degree)) %>%
      group_by(Major) %>%
      #semi_join(degrees, course.ID.and.grade, by="IDS") %>% filter(grepl("B",Degree)) %>%
      dplyr::summarize(Ntotal=n_distinct(IDS)) %>%
      arrange(-Ntotal) %>%
      head(n = params$ndegrees)
  },
  options = list(searching = FALSE,
                 lengthChange = FALSE),
  selection = "single",
  server = FALSE)
  
  # env <- environment()  # can use globalenv(), parent.frame(), etc
  # output$memUsage <- renderTable({
  #   data.frame(
  #     object = ls(env),
  #     size = unlist(lapply(ls(env), function(x) {
  #       object.size(get(x, envir = env, inherits = FALSE))
  #     }))
  #   ) %>% arrange(-size)
  # })
})