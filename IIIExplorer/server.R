#library(rlang)
library(deepr) # course_instances, first_instances, add_neighbor_courses

course_data <- read_csv(file_names$course_data, 
                        col_types = col_types$course_data, 
                        progress = FALSE) %>%
  mutate(term = racadia::as_term(term))

student_data <- read_csv(file_names$student_data, 
                         col_types = col_types$student_data) %>%
  mutate(first_enrolled_term = racadia::as_term(first_enrolled_term))

degree_data <- read_csv(file_names$degree_data) %>%
  mutate(term = racadia::as_term(term))

resetDateSlider <- function(session) {
  date_range <- range(as.numeric(course_data$term) %/% 100)
  #message("updateSliderInput: ", paste(date_range, collapse = " thru "))
  updateSliderInput(session, "termRange", 
                    min = date_range[1], 
                    max = date_range[2], 
                    value = c(date_range[1], date_range[2]))
  return(date_range)
}

shinyServer(function(input, output, session) {
  vals <- reactiveValues(dateRange = c(NULL, NULL),
                         collegeFilter = NULL,
                         majorFilter = NULL,
                         grouping_course = NULL,
                         valid = FALSE,
                         applyFilter = NULL,
                         filterEnable = FALSE)
  
  
  observe({
    ## Set min and max date based on data
    termRange <- resetDateSlider(session)
    vals$dateRange <- numeric_to_term(termRange)
  })
  
  observe({
    req(!is.null(input$termRange), length(input$termRange == 2), all(!is.na(input$termRange)))
    if (length(req(input$termRange)) == 2 & all(!is.na(input$termRange))) {
      vals$dateRange <- numeric_to_term(input$termRange)
      if (is.null(vals$applyFilter)) {
        vals$applyFilter <- 0
      } 
    }
  })

  # reactiveValues is used here because the updateSelect2Input, used for reset
  # button action, does not seem to let shiny see that the input$collegeFilter
  # has changed.
  observe({ vals$collegeFilter <- input$collegeFilter })
  observe({ vals$majorFilter <- input$majorFilter })
  observeEvent(input$applyFilter, { vals$applyFilter <- vals$applyFilter + 1 })
  
  filtered_course_data <- eventReactive(vals$applyFilter, {
    message("filtered_course_data triggered by ", vals$applyFilter)
    req(as.logical(all(input$filterBoolean %in% c("&", "|"))), 
        vals$dateRange)
    #message("filter stage 0 -- names: ", paste(names(course_data), collapse = ", "))
    #message("filter stage 0 -- nrows: ", nrow(course_data))
    filtered_data <- filter(course_data, between(term, vals$dateRange[1], vals$dateRange[2]))
    filteredIDs <- filtered_data %>% distinct(id)
    #message("filter stage 1 -- nrows: ", nrow(filtered_data))
    
    filter_parts <- c()
    if (isTruthy(vals$collegeFilter)) {
      filter_parts <-  c(filter_parts, "(college_desc %in% vals$collegeFilter)")
    }
    
    if (isTruthy(vals$majorFilter)) {
      filter_parts <- c(filter_parts, "(major %in% vals$majorFilter)")
    }
    
    if (length(filter_parts) > 0) {
      #message("filter stage 2 -- ", paste(filter_parts, collapse = input$filterBoolean))
      #message("names(course_data): ", paste(names(course_data), collapse = ", "))
      .IDS <-
        (filter(course_data,!!!rlang::parse_exprs(
          paste(filter_parts, collapse = input$filterBoolean)
        )))$id
      filtered_data <- filter(filtered_data, id %in% .IDS)
      #message("filter stage 2 -- nrows: ", nrow(filtered_data))
    }
    
    if (length(filteredIDs) > 0) {
      filtered_data <-filter(filtered_data, id %in% filteredIDs$id)
      #message("filter stage 3 -- nrows: ", nrow(filtered_data))
    }
    
    if (input$hasDegree) {
      filtered_degrees <-
        degree_data %>% filter(id %in% filtered_data$id)
      if (length(filter_parts) > 0) {
        #message("filtering degrees")
        filtered_degrees <-
          filtered_degrees %>% filter(!!!rlang::parse_exprs(paste(filter_parts, collapse = " | ")))
      }
      filtered_data <-
        filter(filtered_data, id %in% filtered_degrees$id)
      }
    
    return(filtered_data)
  })
  
  course_list <- reactive({
    filtered_course_data() %>%
      select(course, subject, number, title = grade_title) %>%
      na.omit() %>% distinct() %>% 
      mutate(Course_ID = stringr::str_replace(course, "_", " ")) %>% 
      arrange(subject, number) 
  })
  
  observe({
    #message("filtered_course_data has ", nrow(filtered_course_data()), " rows.")
    #message("course_list has ", nrow(course_list()), " rows.")
    updateTextInput.typeahead(session, "profile_course", 
                              course_list(), 
                              valueKey = "course",
                              tokens = paste(course_list()$subject, course_list()$number, sep = ""),
                              placeholder = "e.g. ESM2204",
                              template = "<p class = 'repo-language'>{{subject}} {{number}}<p> 
                              <p class = 'repo-description'>{{title}}</p>"
                              )
    updateTextInput.typeahead(session, "compareCourse",
                              course_list(),
                              valueKey = "course",
                              tokens = paste(course_list()$subject, course_list()$number, sep = ""),
                              placeholder = "e.g. ESM2104",
                              template = "<p class = 'repo-language'>{{subject}} {{number}}<p> 
                              <p class = 'repo-description'>{{title}}</p>")
  })
  
  observe({
    grouping_vars <- student_data %>% 
      select_if(~(is.factor(.) & length(levels(.)) <= 3)) %>% 
      names()
    updateSelectInput(session, "groupBy", 
                      choices = c("None" = "none", "Selected Course" = "group", 
                      grouping_vars))
    #updateCheckboxGroupInput(session, "demographicFilter", choices = c("All", grouping_vars))
  })
  
  profile_course_first_instance <- reactive({ 
    req(input$profile_course, filtered_course_data())
    ci <- course_instances(filtered_course_data(), input$profile_course)
    fi <- first_instance(ci) %>%
      left_join(student_data, by = "id")
    attr(fi, "profile_course") <- input$profile_course
    attr(fi, "distinct_IDS") <- unique(fi$id)
    fi
  })
  
  course_grouping <- reactive({ #})
  #observe({
    req(vals$grouping_course)
    
    took_selected_before <- courses_with_profile() %>%
      filter(course == vals$grouping_course, when == "before")
    .IDS <- took_selected_before$id
    
    profile_course_first_instance() %>%
      mutate(group = if_else(id %in% .IDS,
                             paste0("Took ", vals$grouping_course, " before"),
                             paste0("Did not take ", vals$grouping_course, " before"))) %>%
      select(id, group)
  })

  courses_with_profile <- reactive({
    req(input$profile_course)
    fi <- profile_course_first_instance()
    profile_course <- attr(fi, "profile_course")
    add_neighbor_courses(filtered_course_data() %>% 
                         filter(course != profile_course),
                         fi) %>%
      left_join(student_data, by = "id")
  })
  
  observe({ vals$grouping_course <- input$compareCourse })
  observe({ 
    vals$grouping_course <- neighbor_before() 
    #updateTextInput(session, "compareCourse", value = vals$grouping_course)
  })
  
  observeEvent(input$resetInputs, {
    req(all(!is.na(input$termRange)) & length(input$termRange) == 2)
    message("resetting DateSlider via input$resetInputs (", input$resetInputs, ")")
    updateSelect2Input(session, "majorFilter", "is this uses?", choices = NULL)
    updateSelect2Input(session, "collegeFilter", "is this uses?", choices = NULL)
    updateSelectInput(session, "groupBy", selected = "none")
    updateCheckboxInput(session, "hasDegree", value = FALSE)
    
    vals$majorFilter = character(0)
    vals$collegeFilter = character(0)
    vals$dateRange <- numeric_to_term(resetDateSlider(session))
    vals$applyFilter <- vals$applyFilter + 1
  })

  output$nCourses <- renderText(nrow(filtered_course_data()))
  
  output$nStudents <- renderText(n_distinct(filtered_course_data()$id))
  
  output$CourseTitle <- renderText({
    ## technically this only needs to recompute on input$profile_course change, so it could be a 
    ## eventReactive(input$profile_course, {...})
    req(input$profile_course)
    first(as.character(
      filter(
        filtered_course_data(),
        course == input$profile_course
      )$grade_title
    ))
  })
  
  #output$status <- renderPrint(if (!is.na(input$groupBy)) paste0("Grouping by ", input$groupBy))
  #callModule(classificationTree, "SuccessAnalysis", courses_with_profile, profile_course_instances)
  
  callModule(gradeDistribution, "GradeDist", profile_course_first_instance, reactive(input$groupBy), course_grouping)
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
    req(profile_course_first_instance())
    #message("Rendering degree table for ", nrow(vals$course_instances), " rows.")
    #profile_course_first_instance <- first_instance(vals$course_instances)
    degrees_first_instance_summary <- degree_data %>%
      filter(id %in% profile_course_first_instance()$id,
             category == "Primary Major",
             grepl("^B", degree)) %>%
      group_by(major) %>%
      #semi_join(degrees, course.ID.and.grade, by="IDS") %>% filter(grepl("B",Degree)) %>%
      dplyr::summarize(nTotal=n_distinct(id)) %>%
      arrange(-nTotal) %>%
      head(n = params$ndegrees)
  },
  options = list(searching = FALSE,
                 lengthChange = FALSE),
  selection = "single",
  server = FALSE)
})