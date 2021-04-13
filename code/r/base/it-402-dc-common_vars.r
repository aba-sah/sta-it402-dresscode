library(tidyverse)

focus_subject <- "computing" # use a vector for multiple patterns
default_highlights <- c("focus" = "maroon", "other" = "black")
default_highlights

gender_options <- c("male-", "female-", "NotKnown-", "NA-", "NotApplicable-")


redundant_column_flags <- c("-Passes", "-percentage*", "-COMP", "-PassesUngradedCourses")

path_to_file_store <- "sta_it_402/data"

# todo - complete and include levels - 
sqa_qualification_list <- read_csv("sta_it_402/data/demographic_data/sqa_qualifications.csv", trim_ws = T) %>%
                            suppressMessages()
sqa_qualification_list

base_year <- 1986


