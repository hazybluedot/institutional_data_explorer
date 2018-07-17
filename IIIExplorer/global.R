library(shiny)
library(DT)
library(tidyverse)
library(deepr)

enableBookmarking(store = "server")

source("init.R", local = TRUE)

params <- list(ncourses = 5, 
               ndegrees = 8,
               min_bin_size = 1)

source("utils.R", local = TRUE)

#add_neighbor_courses <- (function(course_data) {
add_neighbor_courses <- function(course_data, profile_course_first_instance) {
  #<- first_instance(course_instances)
  #message("names(pcfi): ", paste(names(profile_course_first_instance), collapse = ", "))
  #message("attributes: ", paste(names(attributes(profile_course_first_instance)), collapse = ", "))
    right_join(course_data, 
               profile_course_first_instance %>% 
                 select(id, Profile_Term = term), 
               by = "id") %>% 
      filter(id %in% profile_course_first_instance$id) %>% # Filter out lab courses, which have credit hours set to 0
      mutate(when = case_when(term < Profile_Term ~ "before",
                              term == Profile_Term ~ "with",
                              term > Profile_Term ~ "after")) %>%
      select(-Profile_Term)
}

library(scales)

grade_distribution <- function(course_instances) {
  # this is a somewhat ugly geom_blank hack to force enough extra space at the top of the plot to hold the 
  # bar labels. 
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
    ggtitle(paste0(course_title, " grade distribution, ", 
                   paste(range(course_instances$term), collapse = " thru "), 
                   " (N = ", nrow(course_instances), ")"))
}

source("CourseWidget.R", local = TRUE)
source("GradeDistWidget.R", local = TRUE)
