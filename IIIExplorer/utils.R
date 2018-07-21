library(data.table)
library(racadia)

source("consts.R", local = TRUE)

firstna <- function(x) {
  first(x[!is.na(x)])
}

numeric_to_term <- function(x) {
  year <- x %/% 1
  part <- x %% 1
  month <- case_when(part == 0 ~ "01",
                     part == 0.5 ~ "09",
                     TRUE ~ as.character(NA))
  as_term(as.integer(paste0(year, month)))
}

convert_grades_numeric <- function(x, Tvalue = 2.0) {
  A <- factor(x, levels = grade_levels)
  values <- c(4, 3.7, 
              3.3, 3, 2.7,
              2.3, 2, 1.7,
              1.3, 1, 0.7,
              Tvalue,
              0,NA,NA,0,NA, 0)
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

course_instances <- function(.data, profile_course) {
  .data <- as.data.table(.data)
  instances <- .data[course == profile_course, unique(term), by = id
                     ][, unit:=1
                       ][, attempt:=cumsum(unit), by = id
                         ][.data[course == profile_course], on = c("id", V1 = "term")]
  setnames(instances, "V1", "term")
  
  #message("names(instances): c(", paste(names(instances), collapse = ", "), ")")
  structure(instances,
            first_instances = which(instances$attempt == 1),
            profile_course = profile_course,
            distinct_IDS = unique(instances$id)
            )
}

first_instance <- function(course_instances, old.method = FALSE) {
  # if (n_distinct(course_instances$course) != 1) {
  #   warning("first_instance assumes only one unique course, found ", n_distinct(course_instances$course))
  # }
  if (!is.null(attr(course_instances, "first_instances")) & old.method == FALSE) {
    idx <- attr(course_instances, "first_instances")
    return(course_instances[idx,])
  }

  #message("first_instance: attributes(course_instances): ", paste(names(attributes(course_instances)), collapse = ", "))
  message("first_instance: using old method.") # names: ", paste(names(course_instances), collapse = ", "))
  fi <- course_instances %>% 
    group_by(id) %>% 
    arrange(term) %>% 
    dplyr::summarize(First_Taken = first(term))
  #message("names of fi: ", paste(names(fi), collapse = ", "))
  left_join(course_instances, fi, by = "id") %>% 
    filter(term == First_Taken) %>%
    select(-First_Taken)
}

#add_neighbor_courses <- (function(course_data) {
add_neighbor_courses <- function(course_data, profile_course_first_instance) {
  left_join(course_data, 
             profile_course_first_instance %>% 
               select(id, Profile_Term = term), 
             by = "id") %>% 
  filter(id %in% profile_course_first_instance$id) %>% # Filter out lab courses, which have credit hours set to 0
      mutate(when = case_when(term < Profile_Term ~ "before",
                            term == Profile_Term ~ "with",
                            term > Profile_Term ~ "after")) %>%
    select(-Profile_Term)
}

