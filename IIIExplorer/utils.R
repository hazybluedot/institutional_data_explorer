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
                  "F", "NR", "NG", "F *", "W", "WG", 
                  "I", "P", "RP", "AUD", "X", "EQ", "S", "NS")

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

course_instances <- function(.data, profile_course) {
  # instances <- .data %>%
  #   filter(
  #     Grade_Course == profile_course,
  #     !is.null(Grade_Final_Grade),
  #     Grade_Final_Grade %in% valid_grades)
  
  .data <- as.data.table(.data)
  instances <- .data[Grade_Course == profile_course, unique(Banner_Term), by = IDS
                     ][, unit:=1
                       ][, attempt:=cumsum(unit), by = IDS
                         ][.data[Grade_Course == profile_course], on = c("IDS", V1 = "Banner_Term")]
  setnames(instances, "V1", "Banner_Term")
  
  #message("names(instances): c(", paste(names(instances), collapse = ", "), ")")
  structure(instances,
            first_instances = which(instances$attempt == 1),
            profile_course = profile_course,
            distinct_IDS = unique(instances$IDS)
            )
  # attr(instances, "first_instances") <- which(instances$attempt == 1)
  # attr(instances, "profile_course") <- profile_course
  # attr(instances, "distinct_IDS") <- unique(instances$IDS)
  # instances
}

first_instance <- function(course_instances, old.method = FALSE) {
  # if (n_distinct(course_instances$Grade_Course) != 1) {
  #   warning("first_instance assumes only one unique course, found ", n_distinct(course_instances$Grade_Course))
  # }
  if (!is.null(attr(course_instances, "first_instances")) & old.method == FALSE) {
    idx <- attr(course_instances, "first_instances")
    return(course_instances[idx,])
  }

  message("first_instance: attributes(course_instances): ", paste(names(attributes(course_instances)), collapse = ", "))
  message("first_instance: using old method")
  course_instances %>% 
    group_by(IDS) %>% 
    arrange(Banner_Term) %>% 
    dplyr::summarize(First_Taken = first(Banner_Term), 
                     First_Grade = first(Grade_Final_Grade))
}
