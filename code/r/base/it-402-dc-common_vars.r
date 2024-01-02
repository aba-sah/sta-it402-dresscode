library(tidyverse)

focus_subject <- "computing" # use a vector for multiple patterns
default_focus_subject <- c("comput", "information systems", "information and communications technology")
focus_subject <- default_focus_subject
other_computing_subject_flag <- "OtherComputing"


science_subjects <- c("bio", "chem", "physics")
stem_subjects <- c("math", "statistic", "engineering", "comput", "technical", "bio", "chem", "physics", "physiology")

default_highlights <- c("focus" = "maroon", "other" = "black")
default_highlights

gender_options <- c("male-", "female-", "NotKnown-", "NA-", "NotApplicable-")
gender_options_formatted <- c("male", "female", "NotKnown", "NA", "NotApplicable")

gender_options_iso <- c("0" = "Not known", "1" = "Male", "2" = "Female", "9" = "Not applicable")
# https://www.iso.org/standard/81682.html
# https://www.iso.org/obp/ui/#iso:std:iso-iec:5218:ed-2:v1:en

# definitions moved into common_interaction
#gender_colour_scheme <- c("female" = "purple", "male" = "darkgreen")
#gender_shape_icons <- c("female" = -0x2640L, "male" = -0x2642L) # \u2640 and \u2642 )
# see also https://www.telegraph.co.uk/women/business/women-mean-business-interactive

redundant_column_flags <- c("-Passes", "-percentage*", "-COMP", "-PassesUngradedCourses")

path_to_file_store <- "sta_it_402/data"
default_output_lib_folder <- "libs"

base_year <- 1986


abbreviations <- c("RC", "FTE", "CS", "ASN", "ESL", "SIMD", "FSM", "P4-P7/S1-S6/SP", "SCQF", "SQA", "SVQ", "NQGA", "CBQ")

excel_date_origin <- "1899-12-30"

InfInt <- .Machine$integer.max
