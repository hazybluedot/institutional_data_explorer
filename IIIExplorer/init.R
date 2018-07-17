library(deepr)
library(tidyverse)

local_dir <- "~/ENGE/workspace/III_Dashboard/IIIExplorer"

if (dir.exists(local_dir)) {
  setwd(local_dir)
} else {
  setwd("/root/IIIExplorer")
}

source("consts.R", local = TRUE)

col_term <- function() {
  function(x) {
    collector("term")
    #as_term(x)
  }
}

parse_term <- function(x, na = c("", "NA"), locale = default_locale(), trim_ws = TRUE) {
  parse_vector(x, col_term(), na = na, locale = locale, trim_ws = trim_ws)
}

file_names <- list(student_data = "../data/student_data.csv",
                  course_data = "../data/course_data.csv",
                  degree_data = "../data/degree_data.csv",
                  college_majors = "../data/college_majors.csv")

col_types <- list(student_data = readr::cols_only(id = col_integer(),
                                              zip5 = col_character(),
                                              first_enrolled_term = col_integer(),
                                              Gender = readr::col_factor(c("Male", "Female")),
                                              URM = readr::col_factor(c("Y", "N")),
                                              `First Generation` = readr::col_factor(c("Y", "N")),
                                              `First Time Freshman` = readr::col_factor(c("Y", "N")),
                                              `Tuition` = readr::col_factor(c("In-State", "Out-of-State")),
                                              `Math Readiness` = readr::col_factor(c("Ready", "Not Ready", "Not Evaluated"))),
                  course_data = readr::cols_only(id = col_integer(),
                                                 term = col_integer(),
                                                 student_level = col_double(),
                                                 college_code = col_integer(),
                                                 college_desc = col_character(),
                                                 major = col_character(),
                                                 subject = col_character(),
                                                 number = col_character(),
                                                 course = col_character(),
                                                 registered_subject = col_character(),
                                                 registered_number = col_character(),
                                                 grade_title = col_character(),
                                                 final_grade = col_character()))

load_student_data <- function() {
  message("loading student_data")
  data(student_data)
  student_data <- student_data %>% 
    #mutate_at(c("gender", "first_generation", "urm", "tuition", "first_time_transfer"), as.factor) %>%
    dplyr::rename(Tuition = tuition,
                  URM = urm,
                  Gender = gender,
                  Tuition = tuition,
                  `First Time Freshman` = first_time_freshman,
                  `First Generation` = first_generation,
                  `First Time Transfer` = first_time_transfer,
                  `Math Readiness` = math_readiness) %>%
    filter(Gender %in% c("Male", "Female"))
  message("writing student_data")
  readr::write_csv(student_data, file_names$student_data)
}

load_course_data <- function() {
  message("loading merged_course_data")
  data("merged_course_data")
  #data("transfer_courses")
  #data("orphan_courses")
  
  stop_for_problems(course_data)
  
  course_data <- merged_course_data %>% 
    mutate(final_grade = parse_factor(final_grade, grade_levels),
           subject = if_else(is.na(subject), registered_subject, subject),
           number = if_else(is.na(number), registered_number, number)) %>%
    filter(credit_hours > 0 | is.na(credit_hours),
           !is.na(subject), !is.na(number)) %>%
    unite(course, subject, number, remove = FALSE)
# %>%
  # bind_rows(transfer_courses %>% rename(subject = grade_subject, 
  #                                       number = grade_number)),
  #           orphen_courses %>% rename(grade_title = title, grade_crn = crn))
  message("writing course_data.csv")
  readr::write_csv(course_data, file_names$course_data)
  
  college_majors <- select(merged_course_data, college_code, college_desc, major) %>% distinct()
  
  message("writing college_majors.csv")
  readr::write_csv(college_majors, file_names$college_majors)
}

load_degree_data <- function(fname = degree_fname) {
  message("loading degree_data")
  data("degree_data")
  #degree_data
  message("writing degree_data.csv")
  readr::write_csv(degree_data, file_names$degree_data)
}

init_data <- function() {
  load_course_data()
  load_student_data()
  load_degree_data()
}

benchmark <- function() {
  res <- microbenchmark::microbenchmark(
    load("../data/student_data.rda"),
    readr::read_csv("../data/student_data.csv", col_types = col_types$student_data),
  times=100L)
}
