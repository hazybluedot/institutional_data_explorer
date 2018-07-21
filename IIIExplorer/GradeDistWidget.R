# Grade Distribution Widget

library(shiny)

grade_distribution <- function(course_instances) {
  # this is a somewhat ugly geom_blank hack to force enough extra space at the top of the plot to hold the 
  # bar labels. 
  #message("grade_distribution names(course_instances): ", paste(names(course_instances), collapse = ", "))
  top_bar <- function(groupingVar) {
    function(.data) {
      groupVar <- c()
      mult <- 1.15
      
      if (groupingVar %in% names(.data)) {
        groupVar <- groupingVar
        mult <- 1.2
      } 
      
      df <-  .data %>% 
        group_by_at(c("grade", groupVar)) %>% 
        dplyr::summarize(n = n()) %>% 
        group_by_at(c(groupVar)) %>%
        mutate(pct = mult * n / sum(n))
      df
    }
  }
  
  aes_now <- function(...) {
    structure(list(...),  class = "uneval")
  }
  
  # Work starts here
  course_title <- firstna(as.character(unique(course_instances$grade_title)))
  course_code <- paste0(as.character(firstna(course_instances$subject)), " ", as.character(firstna(course_instances$number)))
  # TODO: one of these days we need to go through and make naming consistent, i.e. name_case or camelCase
  groupingVar <- attr(course_instances, 'vars')[2]
  
  #message("grade_dist grouping by ", groupingVar)
  isGrouping <- (isTruthy(groupingVar) & groupingVar %in% names(course_instances))
  
  p <- if (length(isGrouping) & isGrouping) {
    ggplot(course_instances, aes_(x = ~grade, 
                                  y = ~..prop.., 
                                  group = as.name(groupingVar), 
                                  fill = as.name(groupingVar)))
  } else {
    ggplot(course_instances, aes(x = grade, 
                                 y = (..count.. / sum(..count..))),
           fill = "maroon")
  }
  
  text_aes <- if (isGrouping) {
    aes(vjust = 0,
        label = paste0(..count.., "\n(", eval(scales::percent(..prop..)), '',")"), y = ..prop.. + 0.025)
  } else {
    aes(vjust = 0, 
        label = paste0(..count.., "\n(", round((..count.. / sum(..count..)) * 100, 2), '%',")"), y = (..count.. / sum(..count..)) + 0.0125)
  }
  
  p  +
    scale_fill_brewer(type = "div", palette = 7, direction = 1) +
    #scale_fill_manual(values = c("maroon", "orange", "blue")) +
    (if (isGrouping) {
      geom_bar(position = "dodge") 
    } else {
      geom_bar(position = "dodge", fill = "#91bfdb") 
    }) +
    geom_text(text_aes,
              stat = 'count',
              position = position_dodge(width = 0.9),
              size = 5) +
    labs(x = 'Grade', y = 'N (%)') +
    scale_y_continuous(expand=c(0.05, 0.0)) +
    theme(text = element_text(size=20)) +
    geom_blank(data = top_bar(groupingVar), aes(x=grade, y = pct)) +
    ggtitle(paste0(course_code, ": ", course_title, " [", 
                   paste(format(range(course_instances$term)), collapse = ", "), 
                   "] (N = ", nrow(course_instances), ")"))
}

gradeDistributionUI <- function(id, label = "Grade Distribution") {
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("GradeDist")),
    uiOutput(ns("status"))
  )
}


gradeDistribution <-
  function(input,
           output,
           session,
           course_data,
           groupBy = NULL,
           grouping_table = NULL) {
    vals <- reactiveValues(invalid = 0)
    noteID <- NULL
    
    course_first_instance <- reactive({
      req(course_data())
      data <-
        #first_instance(course_data()) %>%
        course_data() %>%
        mutate(grade = collapse_letter_grade(final_grade)) %>%
        filter(grade %in% c("A", "B", "C", "D", "F", "W", "T"))
        
      
      groupVar <- groupBy()
      if (groupVar == "group") {
        if (is.reactive(grouping_table)) {
          data <- left_join(data, grouping_table(), by = "id")
        }
      }
      
      if (isTruthy(groupVar) & groupVar %in% names(data)) {
        idx <- data[, groupBy()]
        vals$invalid <- sum(is.na(idx))
        valid <-
          which(!is.na(idx))
        data <- data[valid, ]
      } else {
        vals$invalid = 0
      }
      #message("GradeDist names(data): ", paste(names(data), collapse = ", "))
      data
    })
    
    output$status <- renderUI({
      if (vals$invalid) {
        div(
          class = "shinyalert alert alert-warning",
          paste0(
            "Removed ",
            vals$invalid,
            " rows with missing ",
            groupBy(),
            " data."
          )
        )
      }
    })
    
    output$GradeDist <- renderPlot({
      grouping_vars <- c("grade")
      if (groupBy() != "none" & groupBy() %in% names(course_first_instance())) {
        grouping_vars <- c(grouping_vars, groupBy())
      }
      
      #message("using grouping variable ", groupBy(), " and grouping data by c(", paste(grouping_vars, collapse = ", "), ").")
      grouped_data <- course_first_instance() %>% group_by_at(grouping_vars)
      
      #message("group_sizes: ", paste(attr(grouped_data, 'group_sizes'), collapse = ", "))
      # dplyr conveniently calculates group sizes as part of group_by, and adds
      # this information as an attribute to the data frame.
      shiny::validate(need(min(attr(grouped_data, 'group_sizes')) >= params$min_bin_size,
                          paste0("Smallest group size is less than ", params$min_bin_size, ". Increase the filter scope to view the grade distribution.")))
      # TODO: we should move '10' into a config file to avoid magic numbers sprinkled around the codebase
      grade_distribution(grouped_data)
    })
  }