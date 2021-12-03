source("base/it-402-dc-common_vars.r")

# library(tidyverse) - called in common_vars
library(assertr)




# check focus subject (typically, but not necessarily, Computing) in list of subjects

checkFocusSubjectListed <- 
    function(awardFile, glimpseContent = FALSE, listSubjects = FALSE) {
        awardData <- read_csv(awardFile, trim_ws = TRUE) %>% #, skip_empty_rows = T) # NOT skipping empty rows... :(
                            filter(rowSums(is.na(.)) != ncol(.)) %>%
                            suppressMessages
         
        print(awardFile)
        if (!exists("focus_subject") || is_null(focus_subject) || (str_trim(focus_subject) == "")) {
            focus_subject <- "computing"
            print(paste("No focus subject specified; defaulting to subjects containing: ", focus_subject))
            
        } else 
            print(paste("Search on focus subject (containing term) '", focus_subject, "'", sep = ""))
        
        if (glimpseContent)
            print(glimpse(awardData))
        
        result <- awardData %>%
            select(Subject) %>%

            filter(str_detect(Subject, regex(focus_subject, ignore_case = TRUE))) %>%
            verify(nrow(.) > 0, error_fun = just_warn) 
        
        if (!listSubjects)    
            return(nrow(result)) # comment out this row to list subject names
        else
            return(result)
    }

# check for data stored as percentages only

checkDataAsPercentageOnly <- 
    function(awardFile, glimpseContent = FALSE) {
        awardData <- read_csv(awardFile, trim_ws = TRUE) %>% #, skip_empty_rows = T) # NOT skipping empty rows... :(
                            filter(rowSums(is.na(.)) != ncol(.)) %>%
                            suppressMessages
        
        print(awardFile)
        if (glimpseContent)
            print(glimpse(awardData))
        
        if (!exists("redundant_column_flags") || is.null(redundant_column_flags)) 
            redundant_column_flags <- c("-percentage*", "-COMP", "-PassesUngradedCourses")
        
        awardData %>%
            select(-matches(c(redundant_column_flags, "all-Entries"))) %>% # "-percentage")) %>%
            select(matches(gender_options)) %>%
            verify(ncol(.) > 0, error_fun = just_warn) %>%
        
            #head(0) - comment in and next line out to list headers remaining
            summarise(data_as_counts = (ncol(.) > 0))
    }

# error checking - need to manually correct data if mismatch between breakdown by gender and totals found
# this case, if found, is relatively easy to fix

#TODO -include NotKnown and NA

checkDistributionByGenderErrors <- 
    function(awardFile, glimpseContent = FALSE) {
        awardData <- read_csv(awardFile, trim_ws = TRUE) %>% #, skip_empty_rows = T) # NOT skipping empty rows... :(
                            filter(rowSums(is.na(.)) != ncol(.)) %>%
                            suppressMessages
        
        print(awardFile)
        if (glimpseContent)
            print(glimpse(awardData))

        
        if (awardData %>%
                select(matches(gender_options)) %>%
                verify(ncol(.) > 0, error_fun = just_warn) %>%

                summarise(data_as_counts = (ncol(.) == 0)) == TRUE) { 
            
            awardData <- awardData %>%
                select(-NumberOfCentres) %>%
                pivot_longer(!c(Subject), names_to = "grade", values_to = "PercentageOfStudents") %>%
                separate("grade", c("gender", "grade"), extra = "merge") %>%
                mutate_at(c("gender", "grade"), as.factor) %>%
                filter((gender %in% c("all")) & (grade %in% c("Entries"))) 
        
            # building parallel structure
            return(awardData %>%
                       group_by(Subject) %>%            
                       mutate(total = -1) %>%
                       summarise(total = sum(total)) %>%
                       mutate(DataError = TRUE) # confirmation only - comment out to print al            
            )
        }
        
        
        awardData <- awardData %>%
            mutate_at(vars(starts_with("male-") | starts_with("female-") | starts_with("all-")), as.character) %>%
            mutate_at(vars(starts_with("male-") | starts_with("female-") | starts_with("all-")), parse_number) %>%
            suppressWarnings


        data_as_counts <- awardData %>%
                    select(-matches(redundant_column_flags)) %>% # "-percentage")) %>%
                    select(matches(c("male-", "female-"))) %>%

                    summarise(data_as_counts = (ncol(.) > 0)) %>%
                    as.logical


        if (data_as_counts) {

            awardData <- awardData %>%

                select(-NumberOfCentres) %>%
                mutate_at(vars(starts_with("male")), ~(. / `all-Entries`)) %>%
                mutate_at(vars(starts_with("female")), ~(. / `all-Entries`)) %>%
                select(-(starts_with("all") & !ends_with("-Entries"))) %>%

                pivot_longer(!c(Subject), names_to = "grade", values_to = "PercentageOfStudents") %>%
                separate("grade", c("gender", "grade"), extra = "merge") %>%
                mutate_at(c("gender", "grade"), as.factor) %>%
                filter(!(gender %in% c("all")) & (grade %in% c("Entries")))


        } else { # dataAsPercentageOnly

            awardData <- awardData %>%

                select(Subject, ends_with("-percentage")) %>%
                mutate_at(vars(ends_with("-percentage")), ~(. / 100)) %>%


                pivot_longer(!c(Subject), names_to = "grade", values_to = "PercentageOfStudents") %>%
                separate("grade", c("gender", "grade"), extra = "merge") %>%
                mutate_at(c("gender", "grade"), as.factor)

        } # end if-else - check for data capture approach
        

        awardData %>%

            group_by(Subject) %>%
            summarise(total = sum(PercentageOfStudents, na.rm = TRUE)) %>%
            verify((total == 1.0) | (total == 0), error_fun = just_warn) %>% 

            mutate(DataError = if_else(((total == 1.0) | (total == 0)), FALSE, TRUE)) %>%
            filter(DataError == TRUE) %>% # confirmation only - comment out to print all
            suppressMessages # ungrouping messages

}


 
# warning only - document if necessary
# double-check for subjects with values all NA - does this mean subject being excluded or no one took it?

checkSubjectsWithNoEntries <- 
    function(awardFile, glimpseContent = FALSE) {
        awardData <- read_csv(awardFile, trim_ws = TRUE) %>% #, skip_empty_rows = T) # NOT skipping empty rows... :(
                            filter(rowSums(is.na(.)) != ncol(.)) %>%
                            suppressMessages
        
        print(awardFile)
        if (glimpseContent)
            print(glimpse(awardData))
        
        bind_cols(
            awardData %>%
                mutate(row_id = row_number()) %>%
                select(row_id, Subject), 
                  
            awardData %>%
                select(-c(Subject, NumberOfCentres)) %>%
                mutate_at(vars(starts_with("male-") | starts_with("female-") | starts_with("all-")), as.character) %>%
                mutate_at(vars(starts_with("male-") | starts_with("female-") | starts_with("all-")), parse_number) %>%
                suppressWarnings %>%
 
                assert_rows(num_row_NAs, 
                    within_bounds(0, length(colnames(.)), include.upper = F), everything(), error_fun = just_warn) %>% 
                    # comment out just_warn to stop execution on fail
                summarise(column_count = length(colnames(.)),
                          count_no_entries = num_row_NAs(.)) 
                  
        ) %>% # end bind_cols
            
        filter(count_no_entries == column_count) # comment out to print all
    }

## call using any of the options below
## where files_to_verify is a vector containing (paths to) files to check


### checkFocusSubjectListed
#lapply(files_to_verify, checkFocusSubjectListed, listSubjects = TRUE)
#Map(checkFocusSubjectListed, files_to_verify, listSubjects = TRUE)

#as.data.frame(sapply(files_to_verify, checkFocusSubjectListed)) # call without as.data.frame if listing values


### checkDataAsPercentageOnly
#sapply(files_to_verify, checkDataAsPercentageOnly)
#Map(checkDataAsPercentageOnly, files_to_verify) #, T)


### checkDistributionByGenderErrors
#data.frame(sapply(files_to_verify, checkDistributionByGenderErrors))


### checkSubjectsWithNoEntries
#data.frame(sapply(files_to_verify, checkSubjectsWithNoEntries))


