library(shiny)
library(DT)
library(tidyverse)

enableBookmarking(store = "server")

local_dir <- "~/ENGE/workspace/III_Dashboard/IIIExplorer"

if (dir.exists(local_dir)) {
 setwd(local_dir)
} else {
 setwd("/root/IIIExplorer")
}

course_fname <- paste0(getwd(), "/../data/course_data.rda")
student_fname <- paste0(getwd(), "/../data/student_data.rda")
degree_fname <- paste0(getwd(), "/../data/degrees_dummyIDs.rda")

params <- list(ncourses = 5, 
               ndegrees = 8)

source("utils.R", local = TRUE)

read_student_file <- function(fname = student_fname) {
  load(fname)
  student_data %>% dplyr::rename(Tuition = Tuition_IO_Desc,
                          URM = UnderRepMin,
                         `First Time Freshman` = First_time_freshman,
                         `First Generation` = First_generation_yn,
                         `First Time Transfer` = First_time_transfer) %>%
    filter(Gender %in% c("M", "F"))
}

read_course_file <- function(fname = course_fname) {
  load(fname)
  stop_for_problems(course_data)
  course_data %>% mutate(Banner_Term = parse_date(as.character(Banner_Term), "%Y%m"),
                         Grade_Final_Grade = parse_factor(Grade_Final_Grade, grade_levels))
}

read_degree_file <- function(fname = degree_fname) {
  load(fname)
  mutate(degrees, Degree_term = parse_date(as.character(Degree_term), "%Y%m")) %>% 
    dplyr::rename(Degree_Term = Degree_term,
                  College_code = Degree_college_code,
                  College_desc = Degree_college_desc,
                  Major = Degree_major)
}

#add_neighbor_courses <- (function(course_data) {
add_neighbor_courses <- function(course_data, profile_course_first_instance) {
     #<- first_instance(course_instances)
  message("names(pcfi): ", paste(names(profile_course_first_instance), collapse = ", "))
  message("attributes: ", paste(names(attributes(profile_course_first_instance)), collapse = ", "))
    right_join(course_data, 
               profile_course_first_instance %>% 
                 select(IDS, First_Taken = Banner_Term), 
               by = "IDS") %>% 
      filter(Registered_credit_hours > 0,
             IDS %in% profile_course_first_instance$IDS) %>% # Filter out lab courses, which have credit hours set to 0
      mutate(when = case_when(Banner_Term < First_Taken ~ "before",
                              Banner_Term == First_Taken ~ "with",
                              Banner_Term > First_Taken ~ "after"))
}
#})(course_data)

#'
#' @return an object of the same type as .data with columns 
fetch_neighbor_courses <- function(courses_with_profile, profile_course, Ntotal, ncourses) {
  courses_with_profile %>% 
      filter(Grade_Course != profile_course) %>%
      group_by(when, Grade_Course) %>% 
      dplyr::summarize(Title = dplyr::first(Grade_Course_Title), N = n_distinct(IDS)) %>% 
      mutate( pct = N / Ntotal ) %>%
      arrange(when, -pct) %>% 
      ungroup() %>%
      split(.$when) %>% 
      purrr::map(head, n = ncourses) %>%
      purrr::map(select, -when)
}

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
        group_by_at(c("Grade", groupVar)) %>% 
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
  course_title <- firstna(as.character(unique(course_instances$Grade_Course_Title)))
  # TODO: one of these days we need to go through and make naming consistent, i.e. name_case or camelCase
  groupingVar <- attr(course_instances, 'vars')[2]
  
  isGrouping <- (isTruthy(groupingVar) & groupingVar %in% names(course_instances))
  
  p <- if (length(isGrouping) & isGrouping) {
    ggplot(course_instances, aes_(x = ~Grade, 
                                   y = ~..prop.., 
                                  group = as.name(groupingVar), 
                                  fill = as.name(groupingVar)))
  } else {
    ggplot(course_instances, aes(x = Grade, 
                                  y = (..count.. / sum(..count..))),
           fill = "maroon")
  }
  
  text_aes <- if (isGrouping) {
    aes(vjust = 0,
        label = paste0(..count.., "\n(", round(eval(..prop..) * 100, 2), '',")"), y = ..prop.. + 0.025)
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
    geom_blank(data = top_bar(groupingVar), aes(x=Grade, y = pct)) +
    ggtitle(paste0(course_title, " grade distribution, ", 
                   paste(range(course_instances$Banner_Term), collapse = " thru "), " (N = ", nrow(course_instances), ")"))
}

source("CourseWidget.R", local = TRUE)
source("GradeDistWidget.R", local = TRUE)
source("successAnalysis.R", local = TRUE)
source("TreeWidget.R", local = TRUE)
#source("ENGE/Studies/Investing in Instructors/III_Dashboard")
