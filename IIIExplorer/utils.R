library(data.table)
library(racadia)

source("consts.R", local = TRUE)

paste_list <- function(.collapsable) {
  paste0("(", paste(.collapsable, collapse =", "), ")")
}

# firstna <- function(x) {
#   first(x[!is.na(x)])
# }

numeric_to_term <- function(x) {
  year <- x %/% 1
  part <- x %% 1
  month <- case_when(part == 0 ~ "01",
                     part == 0.5 ~ "09",
                     TRUE ~ as.character(NA))
  as_term(as.integer(paste0(year, month)))
}


