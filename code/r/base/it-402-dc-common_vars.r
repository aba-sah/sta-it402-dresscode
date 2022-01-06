library(tidyverse)

focus_subject <- "computing" # use a vector for multiple patterns
default_focus_subject <- c("comput", "information systems", "data science")
focus_subject <- default_focus_subject


science_subjects <- c("bio", "chem", "physics")
stem_subjects <- c("math", "statistic", "engineering", "comput", "technical", "bio", "chem", "physics", "physiology")

default_highlights <- c("focus" = "maroon", "other" = "black")
default_highlights

gender_options <- c("male-", "female-", "NotKnown-", "NA-", "NotApplicable-")
gender_options_formatted <- c("male", "female", "NotKnown", "NA", "NotApplicable")

# definitions moved into common_interaction
#gender_colour_scheme <- c("female" = "purple", "male" = "darkgreen")
#gender_shape_icons <- c("female" = -0x2640L, "male" = -0x2642L) # \u2640 and \u2642 )
# see also https://www.telegraph.co.uk/women/business/women-mean-business-interactive

redundant_column_flags <- c("-Passes", "-percentage*", "-COMP", "-PassesUngradedCourses")

path_to_file_store <- "sta_it_402/data"

# todo - complete and include levels - 
sqa_qualification_list <- read_csv("sta_it_402/data/demographic_data/sqa_qualifications.csv", trim_ws = T) %>%
                            suppressMessages()
sqa_qualification_list


# default file for normalising pupil counts across years
school_rolls_from_1966 <- read_csv("base/index_factors_census_pupils_from_1966.csv", trim_ws = T) %>%
                            suppressMessages()
base_year <- 1986


