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
  factor(fct_collapse(factor(x, grade_levels), A = c("A", "A-"),
                      B = c("B+", "B", "B-"),
                      C = c("C+", "C", "C-"),
                      D = c("D+", "D", "D-"), 
                      F = c("F", "NR", "NG", "F *"),
                      T = "T",
                      W = "W", 
                      WG = "WG"))
  #, exclude = c("AUD", "EQ", "I", "NS", "P", "PC", "RP", "S", "X"))
}


