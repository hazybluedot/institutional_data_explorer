library(shiny)
library(DT)
library(tidyverse)

local_dir <- "~/ENGE/Studies/Investing in Instructors/III_Dashboard/IIIExplorer"

if (dir.exists(local_dir)) {
 setwd("~/ENGE/Studies/Investing in Instructors/III_Dashboard/IIIExplorer")
} else {
 setwd("/root/IIIExplorer")
}

course_fname <- "../data/course_data.rda"
student_fname <- "../data/student_data.rda"
degree_fname <- "../data/degrees_dummyIDs.rda"

params <- list(ncourses = 5, 
               ndegrees = 8)

firstna <- function(x) {
  first(x[!is.na(x)])
}

numeric_to_term <- function(x) {
      year <- x %/% 1
      part <- x %% 1
      month <- case_when(part == 0 ~ 1,
                         part == 0.5 ~ 9,
                         TRUE ~ as.numeric(NA))
      as.Date(paste(year, month, 1, sep = "-"))
}

term_to_numeric <- function(x) {
  
}

grade_levels <- c("A", "A-",
                  "B+", "B", "B-",
                  "C+", "C", "C-",
                  "D+", "D", "D-",
                  "T",
                  "F", "NR", "NG", "F *", "W", "WG")

convert_grades_numeric <- function(x, Tvalue = 2.0) {
  A <- factor(x, levels = grade_levels)
  values <- c(4, 3.7, 
              3.3, 3, 2.7,
              2.3, 2, 1.7,
              1.3, 1, 0.7,
              Tvalue,
              0,0,0,0,0, 0)
  values[A]
}

collapse_letter_grade <- function(x) {
  factor(fct_collapse(x, A = c("A", "A-"),
               B = c("B+", "B", "B-"),
               C = c("C+", "C", "C-"),
               D = c("D+", "D", "D-"), 
               F = c("F", "NR", "NG", "F *"),
               T = "T",
               W = "W", 
               WG = "WG"), levels = c("A", "B", "C", "D", "F", "T", "W","WG"))
  #, exclude = c("AUD", "EQ", "I", "NS", "P", "PC", "RP", "S", "X"))
}

# Limit to cases where a final grade is assigned
valid_grades <- c("A", "A-", 
                  "B+", "B", "B-", 
                  "C+", "C", "C-", 
                  "D+", "D", "D-", 
                  "F", "F *",
                  "NR", "NG",
                  "W", "T")

first_instance <- function(course_instances) {
  if (n_distinct(course_instances$Grade_Course) != 1) {
    warning("first_instance assumes only one unique course, found ", n_distinct(course_instances$Grade_Course))
  }
  course_instances %>% 
    group_by(IDS) %>% 
    arrange(Banner_Term) %>% 
    dplyr::summarize(First_Taken = first(Banner_Term), 
              First_Grade = first(Grade_Final_Grade))
}


load(course_fname)
load(student_fname)
load(degree_fname)

student_data <- rename(student_data,
                       Tuition = Tuition_IO_Desc,
                       URM = UnderRepMin,
                       `First Time Freshman` = First_time_freshman,
                       `First Generation` = First_generation_yn,
                       `First Time Transfer` = First_time_transfer) %>%
  filter(Gender %in% c("M", "F"))

course_data <- mutate(course_data, 
                      Banner_Term = parse_date(as.character(Banner_Term), "%Y%m"),
                      Grade_Final_Grade = parse_factor(Grade_Final_Grade, grade_levels))

degrees <- mutate(degrees, Degree_term = parse_date(as.character(Degree_term), "%Y%m")) %>% 
  rename(Degree_Term = Degree_term)

course_list <- course_data %>% 
  filter(Registered_course_coll_code == 5, Registered_credit_hours > 0) %>%
  separate(Grade_Course, c("Course_Subject", "Course_Number")) %>% 
  select(Course_Subject, Course_Number) %>% 
  distinct() %>% arrange(Course_Subject, Course_Number)

#add_neighbor_courses <- (function(course_data) {
add_neighbor_courses <- function(course_data, profile_course_first_instance) {
     #<- first_instance(course_instances)
    
    right_join(course_data, profile_course_first_instance %>% select(IDS, First_Taken), by = "IDS") %>% 
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

grade_distribution <- function(course_instances, groupingVar = NULL) {
  course_title <- firstna(as.character(unique(course_instances$Grade_Course_Title)))
  # if (!is.null(groupingVar)) {
  #   message("Grouping grade distribution by ", groupingVar, " with values ", paste(unique(course_instances[[groupingVar]]), collapse = ", "))
  # }
  
  aes_now <- function(...) {
    structure(list(...),  class = "uneval")
  }
  
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
    aes(vjust = -1,
        label = paste0(..count.., " (", round(eval(..prop..) * 100, 2), '%',")"))
  } else {
    aes(vjust = -1, label = paste0(..count.., " (", round((..count.. / sum(..count..)) * 100, 2), '%',")"))
  }

  p  +
    scale_fill_brewer(type = "div", palette = 7, direction = 1) +
    #scale_fill_manual(values = c("maroon", "orange", "blue")) +
    (if (isGrouping) {
      geom_bar(position = "dodge") 
    } else {
      geom_bar(position = "dodge", fill = "#fc8d59") 
    }) +
    geom_text(text_aes,
              stat = 'count',
              position = position_dodge(0.9),
              size = 5) +
    labs(x = 'Grade', y = 'N (%)') +
    scale_y_continuous(expand=c(0.1, 0)) +
    ggtitle(paste0(course_title, " grade distribution, ", paste(range(course_instances$Banner_Term), collapse = " thru "), " (N = ", nrow(course_instances), ")"))
}

source("CourseWidget.R", local = TRUE)
source("GradeDistWidget.R", local = TRUE)
source("successAnalysis.R", local = TRUE)
#source("ENGE/Studies/Investing in Instructors/III_Dashboard")
