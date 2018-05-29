library(shiny)
library(DT)

setwd("~/ENGE/Studies/Investing in Instructors/III_Dashboard")

course_fname <- "../data/course_data.rda"
student_fname <- "../data/student_data.rda"
degree_fname <- "../data/degrees_dummyIDs.rda"

if(!require(tidyverse)) install.packages("tidyverse") # includes dplyr and tidyr
if(!require(reshape2)) install.packages("reshape2")
if(!require(ggalluvial)) install.packages("ggalluvial")

params <- list(ncourses = 5, 
               ndegrees = 8)

firstna <- function(x) {
  first(x[!is.na(x)])
}

convert_grades_numerical <- function(x) {
  A <- factor(x, levels=c("A", "A-",
                          "B+", "B", "B-",
                          "C+", "C", "C-",
                          "D+", "D", "D-",
                          "T",
                          "F", "NR", "NG", "F *", "W"))
  values <- c(4, 3.7, 
              3.3, 3, 2.7,
              2.3, 2, 1.7,
              1.3, 1, 0.7,
              2.0,
              0,0,0,0,0)
  values[A]
}

convert_grades_letter <- function(x) {
  fct_collapse(x, A = c("A", "A-"),
               B = c("B+", "B", "B-"),
               C = c("C+", "C", "C-"),
               D = c("D+", "D", "D-"), 
               F = c("F", "NR", "NG", "F *"),
               T = "T",
               W = "W", 
               WG = "WG")
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
  course_instances %>% 
    group_by(IDS) %>% 
    arrange(Banner_Term) %>% 
    summarize(First_Taken = first(Banner_Term), 
              First_grade = first(Grade_Final_Grade))
}


load(course_fname)
load(student_fname)
load(degree_fname)

course_data <- mutate(course_data, Banner_Term = parse_date(as.character(Banner_Term), "%Y%m"))
degrees <- mutate(degrees, Degree_term = parse_date(as.character(Degree_term), "%Y%m")) %>% 
  rename(Degree_Term = Degree_term)

course_list <- course_data %>% 
  filter(Registered_course_coll_code == 5, Registered_credit_hours > 0) %>%
  separate(Grade_Course, c("Course_Subject", "Course_Number")) %>% 
  select(Course_Subject, Course_Number) %>% 
  distinct() %>% arrange(Course_Subject, Course_Number)

with_neighbor_courses <- (function(course_data) {
  function(course_instances, profile_course, ncourses) {
    profile_course_first_instance <- first_instance(course_instances)
    
    courses_with_profile <- right_join(course_data, 
                                       profile_course_first_instance, by = "IDS") %>% 
      filter(Registered_credit_hours > 0) %>% # Filter out lab courses, which have credit hours set to 0
      mutate(when = case_when(Banner_Term < First_Taken ~ "before",
                              Banner_Term == First_Taken ~ "with",
                              Banner_Term > First_Taken ~ "after"))
  }
})(course_data)
  
fetch_neighbor_courses <- function(courses_with_profile, profile_course, Ntotal, ncourses) {
  courses_with_profile %>% 
      filter(Grade_Course != profile_course) %>%
      group_by(when, Grade_Course) %>% 
      summarize(Title = first(Grade_Course_Title), N = n_distinct(IDS)) %>% 
      mutate( pct = N / Ntotal ) %>%
      arrange(when, -pct) %>% 
      ungroup() %>%
      split(.$when) %>% 
      purrr::map(head, n = ncourses) %>%
      purrr::map(select, -when)
}

grade_distribution <- function(course_instances, groupingVar = NULL) {
    #counts <- table(convert_grades_letter(course_instances$Grade_Final_Grade))
  counts <- mutate(course_instances, Grade = convert_grades_letter(Grade_Final_Grade))
  
  course_title <- firstna(as.character(unique(course_instances$Grade_Course_Title)))
  if (!is.null(groupingVar)) {
    message("Grouping grade distribution by ", paste(unique(course_instances[[groupingVar]]), collapse = ", "))
  }
  
  aesthetics <- (if ("group" %in% names(counts)) {
    aes(x = Grade, y = ..prop.., group = group, fill = group, label = paste0(..count.., " (", round(..prop.. * 100, 2), '%',")"))
  } else {
    aes(x = Grade, y = (..count.. / sum(..count..)), label = paste0(..count.., " (", round((..count.. / sum(..count..)) * 100, 2), '%',")"))
  })

  ggplot(counts, aesthetics) + 
    geom_bar(position = "dodge") + 
    geom_text(aes(
                  vjust = -1),
              stat = 'count', 
              position = position_dodge(0.9), 
              size = 5) +
    labs(x = 'Grade', y = 'N (%)') +
    ggtitle(paste0(course_title, " grade distribution for ", paste(range(course_instances$Banner_Term), collapse = " - "), " (N = ", nrow(counts), ")"))
}

source("CourseWidget.R", local = TRUE)
source("GradeDistWidget.R", local = TRUE)
#source("ENGE/Studies/Investing in Instructors/III_Dashboard")
