library(shiny)
library(DT)
library(tidyverse)

enableBookmarking(store = "server")
params <- list(ncourses = 5, 
               ndegrees = 8,
               min_bin_size = 1)

source("init.R", local = TRUE)
source("utils.R", local = TRUE)

source("CourseWidget.R", local = TRUE)
source("GradeDistWidget.R", local = TRUE)
