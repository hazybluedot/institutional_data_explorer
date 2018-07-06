library(data.table)
library(racadia)

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

grade_levels <- c("A", "A-",
                  "B+", "B", "B-",
                  "C+", "C", "C-",
                  "D+", "D", "D-",
                  "T",
                  "F", "NR", "NG", "F *", "W", "WG", 
                  "I", "P", "RP", "AUD", "X", "EQ", "S", "NS")

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

# Limit to cases where a final grade is assigned
valid_grades <- c("A", "A-", 
                  "B+", "B", "B-", 
                  "C+", "C", "C-", 
                  "D+", "D", "D-", 
                  "F", "F *",
                  "NR", "NG",
                  "W", "T")

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
  message("first_instance: using old method. names: ", paste(names(course_instances), collapse = ", "))
  fi <- course_instances %>% 
    group_by(id) %>% 
    arrange(term) %>% 
    dplyr::summarize(First_Taken = first(term))
  #message("names of fi: ", paste(names(fi), collapse = ", "))
  left_join(course_instances, fi, by = "id") %>% 
    filter(term == First_Taken) %>%
    select(-First_Taken)
}
