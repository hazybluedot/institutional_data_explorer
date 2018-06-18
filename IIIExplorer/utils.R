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
  # if (n_distinct(course_instances$Grade_Course) != 1) {
  #   warning("first_instance assumes only one unique course, found ", n_distinct(course_instances$Grade_Course))
  # }
  course_instances %>% 
    group_by(IDS) %>% 
    arrange(Banner_Term) %>% 
    dplyr::summarize(First_Taken = first(Banner_Term), 
                     First_Grade = first(Grade_Final_Grade))
}
