grade_matrix <- function(course_data, profile_course, cross_course) {
  dd <- course_data %>%
    filter(course %in% c(profile_course, cross_course),
           final_grade %in% valid_grades) %>%
    group_by(id, course) %>%
    dplyr::summarize(first_grade = first(final_grade, order_by = "term")) %>%
    ungroup() %>%
    mutate(first_grade = paste0(course, " ", collapse_letter_grade(first_grade))) %>%
    droplevels() %>%
    spread(course, first_grade)
  #message("Filtered data contains ", n_distinct(dd$id), " unique ids.")
  #print(str(dd))
  f <- paste("~", cross_course, " + ", profile_course)
  message("creating crosstable with formula ", f)
  xtabs(as.formula(f), data = dd)
  #with(dd, table(get(cross_course), get(profile_course)))
}

gradeMatrixUI <- function(id, label = "Grade Matrix") {
  ns <- NS(id)
  tagList(
    tableOutput(ns("GradeMatrix")),
    uiOutput(ns("DataDescription"))
  )
}

gradeMatrix <- function(input, output, session, courses_with_profile, profile_course_fi, cross_course) {

  profile_course <- reactive({ attr(profile_course_fi(), "profile_course") })
  
  course_data <- reactive({
    cd <- bind_rows(
      courses_with_profile() %>% 
        dplyr::filter(course == cross_course(), when %in% c("before", "with")) %>%
        group_by(id, course) %>%
        # use grade from last attempt taken before profile
        dplyr::summarize(final_grade = last(final_grade, order_by = "term")) %>% 
        select(id, course, final_grade) %>% 
        ungroup(),
      profile_course_fi() %>% select(id, course, final_grade))
    cd
  })
  
  output$CrossCourse <- renderText(cross_course())
  
  output$ProfileCourse <- renderText(profile_course())
  output$DataDescription <- renderUI({
    if (!is.null(cross_course()) & !is.null(profile_course())) {
      p("These data are constructed by taking the last grade received in ", 
        cross_course(), " before the student's first attempt at ", 
        profile_course(), ".")
      
    }
  })
  
  output$GradeMatrix <- renderTable({
    shiny::validate(need(profile_course(), "Select a profile course to analyze."),
                    need(cross_course(), "Select a course to compare with."))
    
    #crossTable()
    tbl <- as.data.frame(grade_matrix(course_data(), profile_course(), cross_course()))
    totals <- tbl %>% group_by_at(profile_course()) %>% summarize(colTotal = sum(Freq))
    #totals <- colSums(tbl)
    
    #as.data.frame.matrix(tbl)
    # left_join(tbl, totals, by = "ESM_2204") %>% 
    #   mutate(pct = Freq/colTotal, disp = paste0(Freq, ' (', round(pct,2), ')')) %>% 
    #   select(-Freq, -colTotal, -pct) %>% spread(ESM_2204, disp) %>%
    #   rbind(totals %>% spread(ESM_2204, colTotal))
    
    tbl  %>% group_by_at(profile_course()) %>% 
      mutate(percent = paste0('(', format(round(100 * Freq / sum(Freq), 2), nsmall = 2), ')'),
             pdisp = "(%)") %>% 
      unite(profile_course(), profile_course(), pdisp, sep = " ") %>%
      unite(disp, Freq, percent, sep = " ") %>% 
      spread(2, disp) %>% 
      mutate_at(1, as.character) %>% 
      rbind(c("Total", t(totals)[2,]))
  })
}

#tbl <- as.data.frame(grade_matrix(course_data, "ESM_2204", "ESM_2104"))
#totals <- tbl %>% group_by_at("ESM_2204") %>% summarize(colTotal = sum(Freq))

