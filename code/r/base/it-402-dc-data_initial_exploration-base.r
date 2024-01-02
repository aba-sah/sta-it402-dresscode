source("base/it-402-dc-common_vars.r")
source("base/common.r")


#library(tidyverse) - called in common_vars
#library(DBI) - called in common.r
library(ggtext)



#sessionInfo() 
#
#if ("package:scales" %in% search())
#    detach("package:scales", unload = TRUE)
#
#sessionInfo()







# themes commented out only because not currently needed or swapping between values

dressCodeTheme <- 
        theme_bw() +
        #theme_void() + 
        theme(panel.grid.major.y = element_line(), #panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(), #panel.grid.minor.x = element_blank(), 
                axis.title = element_text(face = "bold", size = 16),
                axis.text.x = element_text(size = 12), # angle = 0, vjust = 0.3),
                #axis.text.y = element_markdown(size = 12), #element_text(size = 14),
                axis.text.y = element_text(size = 12), 
                plot.title = element_text(lineheight = 0.8, size = 22, face = "bold", margin = margin(t = 10, b = 10)),
                legend.title = element_text(size = 14), legend.text = element_text(size = 14),
                strip.text = element_text(size = 18),
                text = element_text(family = "Helvetica")
            ) 


# evaluation will fail if focus_subject not set in advance
#if (!exists("focus_subject") || is_null(focus_subject) || (str_trim(focus_subject) == ""))
    focus_subject <- c("comput", "information systems")


formatted_subject_labels_as_expr <- expression(
    formatted_subject_labels <- paste0(
        "<span style = 'color: ",
             if_else(str_detect(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)), default_highlights["focus"], default_highlights["other"]),
         "; font-weight: ",
             if_else(str_detect(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)), "bold", "normal"), # has no effect altho' docs say it's recognised :(...
         ";'>",
             if_else(str_detect(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)), "<b>", ""),

                 Subject,
             if_else(str_detect(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)), "</b>", ""),
         "</span>")
    )
formatted_subject_labels_as_expr



select_focus_subjects_as_expr <- expression(
    select_focus_subjects <- 
        as.factor(if_else(str_detect(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)), "focus", "other"))
    )
select_focus_subjects_as_expr





# being a bit lazy here and dumping pivoted structure into a simple file-based sqlite DB 
# TODO - complete data schema - to feed in to data store properly
# doesn't include file cleaning and checks - done separately (code in separate source file) and verified in advance

## loadAndParseFile
## wrangleData
## writeToDataStore
## generateVisualisations

loadAndParseFile <-
    function(pathTofile, glimpseContent = FALSE, qualificationType = NULL, qualificationYear = NULL) {
        
        print(paste0("Parsing qualifications file '", pathTofile, "'"))
        
        if (is_null(qualificationType) || (str_trim(qualificationType) == "")) {
            
            qualificationType <- basename(pathTofile)

            if (!str_detect(qualificationType, regex("(?>.*_)(?>.*\\.)"))) # order matters
                stop("You must specify qualification type!")
                
            qualificationType <- unlist(str_split(qualificationType, "_"))[2]
            qualificationType <- unlist(str_split(qualificationType, "\\."))[1]
            
            if (tolower(qualificationType) %in% tolower(sqa_qualification_list$QualificationId)) #relax on case only
                message(paste0("Qualification type '", qualificationType, "' derived from file name."))
            else
                stop("You must specify qualification type!")
        } 
                
        latestQualificationYear <- year(Sys.Date())
        if (is_null(qualificationYear)) {
            
            qualificationYear <- suppressWarnings(parse_number(basename(pathTofile)))
            if (is.na(qualificationYear) || 
                !(as.integer(qualificationYear) == qualificationYear) || !between(qualificationYear, base_year, latestQualificationYear))
                stop(paste("You must specify qualification year as 'yyyy', between", base_year, "and", latestQualificationYear))
            else
                message(paste0("Qualification year '", qualificationYear, "' derived from file name."))
            
        } else if (!(as.integer(qualification_year) == qualification_year)|| !between(qualificationYear, base_year, latestQualificationYear))
            stop(paste("You must specify qualification year as 'yyyy', between", base_year, "and", latestQualificationYear))
        
        
        # passed tests - load data and return
        award_data <- read_csv(pathTofile, trim_ws = T) %>%
                            filter(rowSums(is.na(.)) != ncol(.)) %>%
                            distinct(across(everything()), .keep_all = TRUE) %>%
                            suppressMessages
        
        award_data <- award_data %>%
            mutate(year = factor(qualificationYear),
                   qualification = factor(qualificationType)) %>%
            select(qualification, year, everything())
        
        # for consistency - some files are extracted with "-Pass" - need all to be "-Passes"
        if (sum(str_detect(names(award_data), "-Pass\\b")) > 0)
            names(award_data) <- gsub("-Pass", "-Passes", names(award_data), fixed = TRUE)

        
        if (glimpseContent)
            glimpse(award_data)
    
        invisible(award_data)
    }


wrangleData <-
    function(awardData) {
      
        if (!exists("redundant_column_flags") || is.null(redundant_column_flags))
            redundant_column_flags <- c("-percentage*", "-COMP", "-PassesUngradedCourses")
        
        if((awardData %>%
                select(-matches(redundant_column_flags)) %>% # "-percentage")) %>%
                select(matches(gender_options)) %>%

                summarise(data_as_counts = (ncol(.) == 0))
        ) == TRUE)
            awardData <- preProcessDataAsPercentages(awardData)

        
        if (sum(str_detect(names(awardData), "NumberOfCentres")) == 0)
            awardData <- awardData %>%
                mutate(NumberOfCentres = NA_integer_)
        
        
        awardData <- awardData %>%

            mutate(across(Subject, as.factor),
                   across(NumberOfCentres, as.integer)) %>%
            rename_with(~ gsub("NA-", "NotApplicable-", .x, fixed = TRUE)) %>%

            mutate(across(starts_with(c("male-", "female-", "NA-", "NotKnown-", "all-")), as.character),
                   across(starts_with(c("male-", "female-", "NA-", "NotKnown-", "all-")), parse_number),
                   across(starts_with(c("male-", "female-", "NA-", "NotKnown-", "all-")), as.integer)
                  ) %>%

            suppressWarnings

        
        if ((sum(str_detect(names(award_data), "[A-Z]-[A-Z]")) == 0) && # no practical way to tell whether the lowest grade summarised
            (sum(str_detect(names(award_data), "-NoAward")) == 0)) {
            #((awardData %>%
            #
            #    select(matches(c("-NoAward"))) %>%
            #    summarise(no_award_recorded = (ncol(.) == 0))
            #) == TRUE)) {

            tmp_df <- awardData %>%
                select(-matches(redundant_column_flags))
            tmp_df[is.na(tmp_df)] <- 0

            awardData <- bind_cols(awardData,

                tmp_df %>%
                    mutate("male-NoAward" = (`male-Entries` - reduce(select(., (starts_with("male") &  !ends_with(c("-Entries", "-Passes")))), `+`)), # rowSums(select(., starts_with("male-")))
                           "female-NoAward" = (`female-Entries` - reduce(select(., (starts_with("female") &  !ends_with(c("-Entries", "-Passes")))), `+`)),
                           "all-NoAward" = (`all-Entries` - reduce(select(., (starts_with("all") &  !ends_with(c("-Entries", "-Passes")))), `+`))) %>%
                    select(c("male-NoAward", "female-NoAward", "all-NoAward"))
                ) %>%

                relocate(`male-NoAward`, .after = `male-Entries`) %>%
                relocate(`female-NoAward`, .after = `female-Entries`) %>%
                relocate(`all-NoAward`, .after = `all-Entries`)
        }
        

        if ("NA-Entries" %in% names(awardData)) {
            awardData <- awardData %>%
                mutate("NA-NoAward" = (`NA-Entries` - reduce(select(., (starts_with("NA") &  !ends_with(c("-Entries", "-Passes")))), `+`))) %>%
                relocate(`NA-NoAward`, .after = `NA-Entries`)
        }
        if ("NotKnown-Entries" %in% names(awardData)) {
            awardData <- awardData %>%
                mutate("NotKnown-NoAward" = (`NotKnown-Entries` - reduce(select(., (starts_with("NotKnown") &  !ends_with(c("-Entries", "-Passes")))), `+`))) %>%
                relocate(`NotKnown-NoAward`, .after = `NotKnown-Entries`)
        }
        
        awardData <- awardData %>%

            pivot_longer(!c(qualification, year, Subject, NumberOfCentres), names_to = "grade", values_to = "NoOfStudents") %>%
            separate("grade", c("gender", "grade"), extra = "merge") %>%
            mutate(across(c("gender", "grade", "year"), as.factor))

        invisible(awardData)
    }


preProcessDataAsPercentages <-
    function(awardData) {
      
        awardData <- awardData %>%

            mutate(across((starts_with("all-") | contains("-percentage")), as.character),
                   across((starts_with("all-") | contains("-percentage")), parse_number),
                   across((starts_with("all-") | contains("-percentage")), as.integer)) %>%
            suppressWarnings
         
        includeOpenGender <- ("NA-percentage" %in% names(awardData))
        includeGenderNotKnown <- ("NotKnown-percentage" %in% names(awardData))

        
        awardData <- awardData %>%

            select(Subject, `all-Entries`, ends_with("-percentage")) %>%
            mutate_at(vars(ends_with("-percentage")), ~round(. /100 * `all-Entries`)) %>%

            select_all(~str_replace(., "-percentage", "-Entries")) %>%

            left_join(awardData %>%
                      select(!ends_with("-percentage"))
                     ) %>%

            mutate(across((starts_with("male-") & contains("-percentage")), ~round(. /100 * `male-Entries`)),
                   across((starts_with("female-") & contains("-percentage")), ~round(. /100 * `female-Entries`)),
                   across((starts_with("all-") & contains("-percentage")), ~round(. /100 * `all-Entries`))
                   )
        
            if (includeOpenGender)
                awardData <- awardData %>%
                    mutate(across((starts_with("NA-") & contains("-percentage")), ~round(. /100 * `NA-Entries`)))
            if (includeGenderNotKnown)
                awardData <- awardData %>%
                    mutate(across((starts_with("NotKnown-") & contains("-percentage")), ~round(. /100 * `NotKnown-Entries`)))

        
        invisible(awardData %>%
                    select_all(~str_replace(., "-percentage", "-"))
                 )
    }


filterInput <-
    function(pathTofile, startYear = NULL, endYear = NULL, qualificationTypes = NULL) {
                
        latestQualificationYear <- year(Sys.Date())
        yearErrorMessage <- paste("Start and end years if set must be between 1986 and", latestQualificationYear,
                                  "(inclusive), and specified as 'yyyy'")
       
        if (is_null(startYear)) 
            startYear <- 1986

        else {             
            startYear <- suppressWarnings(parse_integer(as.character(startYear)))
            if (is.na(startYear) | (startYear > latestQualificationYear))
                stop(yearErrorMessage)            
        } 
        
        if (is_null(endYear)) 
            endYear <- latestQualificationYear

        else {             
            endYear <- suppressWarnings(parse_integer(as.character(endYear)))
            if (is.na(endYear) | !between(endYear, 1986, latestQualificationYear))
                stop(yearErrorMessage)            
        } 


        
        qualificationType <- basename(pathTofile)
        if (is_null(qualificationTypes))
            qualificationTypes <- sqa_qualification_list$QualificationId
        else {
            qualificationTypes <- as.vector(qualificationTypes) # allow single value as input
        
            if (!sum(tolower(qualificationTypes) %in% tolower(sqa_qualification_list$QualificationId)) == length(qualificationTypes)) {
                warning(paste("Non-valid (SQA) qualification types excluded:",
                             qualificationTypes[!tolower(qualificationTypes) %in% tolower(sqa_qualification_list$QualificationId)]))

                qualificationTypes <- qualificationTypes[tolower(qualificationTypes) %in% tolower(sqa_qualification_list$QualificationId)]
            }
        }


        if (!str_ends(qualificationType, ".csv") & !str_detect(qualificationType, regex("(?>.*_)(?>.*\\.)"))) # order matters
            stop("File name must be in format 'yyyy_QualificationType.csv' (e.g., '1999_AdvancedHigher.csv') to apply auto-filter!")
        
        qualificationType <- unlist(str_split(qualificationType, "_"))[2]
        qualificationType <- unlist(str_split(qualificationType, "\\."))[1]

        if (!(tolower(qualificationType) %in% tolower(qualificationTypes)))
            return(FALSE)
                                    
        qualificationYear <- suppressWarnings(parse_number(basename(pathTofile)))
        if (is.na(qualificationYear) | !between(qualificationYear, startYear, endYear))        
            return(FALSE)

        
        # if get this far
        return(TRUE)
    }



runPipeline <-
    function(dataFile, glimpseContent = FALSE, dbConnection = NULL, dbTable = NULL, overwriteDataStore = FALSE) {
        
        #print(dataFile)
        
        award_data <- loadAndParseFile(dataFile, glimpseContent = glimpseContent)
        #print(head(award_data))      

        award_data <- wrangleData(award_data) 
        #print(dim(award_data))
        #print(head(award_data))

        if (is_null(dbTable))
            dbTable = "sqa_data"
        if (!is_null(dbConnection))
            writeToDataStore(award_data, dbConnection, dbTable, overwriteDataStore = overwriteDataStore)
        
        invisible(award_data)
    }



# preferable to open connection in each file reusing code?

#dbConn <- dbConnect(RSQLite::SQLite(), paste0(path_to_file_store, "/sqa-data-db.sqlite"))
#dbConn


# sample - edit and run as needed

## test run
#dbRemoveTable(dbConn, "sqa_data")
#filterInput("sta_it_402/data/grades/1999_newhigher.csv", endYear = 1999, qualificationTypes = c("OrdinaryGrade", "NewHigher"))


#Map(filterInput, files_to_verify, startYear = 1989, endYear = 1999, qualificationTypes = "Higher")
#files_to_verify[sapply(files_to_verify, filterInput, startYear = 1989, endYear = 1999, qualificationTypes = "Higher")]


## (runPipeline("sta_it_402/data/grades/1989_Higher.csv", dbConn, TRUE))
## lapply(files_to_verify[sapply(files_to_verify, filterInput, startYear = 1989, qualificationTypes = "Higher")], 
##        runPipeline, dbConn) #, overwriteDataStore = TRUE)
## (removeDuplicatesFromDataStore(dbConn, "sqa_data"))




# note lag will take into account "duplicate" years such as 2015 with two strands of Higher as New Higher introduced
# to double-check this print out timeine and lag statement in loop

getSubjectChanges <-
    function(awardData, lag = 1, convertNullsToNa = FALSE) {
    
        subject_changes <- data.frame(qualificationPreviousYear = factor(), previousYear = factor(), 
                                      qualificationCurrentYear = factor(), currentYear = factor(), 
                                      countSubjectsPreviousYear = integer(), countSubjectsCurrentYear = integer(), 
                                      subjectsInCommon = character(), countSubjectsInCommon = integer(), 
                                      subjectsDropped = character(), subjectsAdded = character(), 
                                      countSubjectsDropped = integer(), countSubjectsAdded = integer(),
                                      stringsAsFactors = FALSE)
        subject_changes$subjectsInCommon <- list() # only way this works...
        subject_changes$subjectsDropped <-  list()
        subject_changes$subjectsAdded <- list()
        
        
        timeline <- awardData %>%
            arrange(year, qualification) %>%
            distinct(year, qualification)
         # print(str(timeline))
         # print(timeline)

        for (currentRow in 1:(nrow(timeline))) { 
            
            ## comment out the next three lines to check timeline being used to generate dataframe
            # previousYear <- as.factor(timeline$year[currentRow - lag])
            # currentYear <- as.factor(timeline$year[currentRow])
            # print(paste(lag, currentRow, previousYear, currentYear))        

            previousYear <- timeline$year[currentRow - lag]
            if ((currentRow - lag) > 0) {
                qualificationPreviousYear <- timeline$qualification[currentRow - lag]
                subjectsPreviousYear <- awardData %>%
                                          filter(year == previousYear) %>%
                                          select(Subject) %>%
                                          mutate_at(c("Subject"), as.character)
                countSubjectsPreviousYear <- nrow(subjectsPreviousYear)               
            } 
            
            currentYear <- timeline$year[currentRow]
            qualificationCurrentYear <- timeline$qualification[currentRow]
            subjectsCurrentYear <- awardData %>%
                                    filter(year == currentYear) %>%
                                    select(Subject) %>%
                                    mutate_at(c("Subject"), as.character)
            countSubjectsCurrentYear <- nrow(subjectsCurrentYear)


            if ((currentRow - lag) > 0) {
                subjectsInCommon <- intersect(subjectsPreviousYear, subjectsCurrentYear)
                countSubjectsInCommon <- lengths(subjectsInCommon) # sees length one if calculated when creating new row

                subjectsDropped <- setdiff(subjectsPreviousYear, subjectsCurrentYear)
                countSubjectsDropped <- lengths(subjectsDropped)

                subjectsAdded <- setdiff(subjectsCurrentYear, subjectsPreviousYear)
                countSubjectsAdded <- lengths(subjectsAdded)              
            } 


            
            if ((currentRow - lag) > 0) 
                subject_changes <- subject_changes %>%
                                    add_row(qualificationPreviousYear = qualificationPreviousYear, previousYear = previousYear, qualificationCurrentYear = qualificationCurrentYear, currentYear = currentYear,
                                            countSubjectsPreviousYear = countSubjectsPreviousYear, countSubjectsCurrentYear = countSubjectsCurrentYear, 
                                            subjectsInCommon = list(subjectsInCommon), countSubjectsInCommon = countSubjectsInCommon, 
                                            subjectsDropped =  list(subjectsDropped), subjectsAdded = list(subjectsAdded), 
                                            countSubjectsDropped = countSubjectsDropped, countSubjectsAdded = countSubjectsAdded)
            else
                subject_changes <- subject_changes %>%
                                    add_row(qualificationCurrentYear = qualificationCurrentYear, currentYear = currentYear,
                                            countSubjectsCurrentYear = countSubjectsCurrentYear)

        } # end for
        
        if (convertNullsToNa) 
            subject_changes <- subject_changes %>%
                                mutate(subjectsInCommon = na_if(subjectsInCommon, "NULL"), # does not recognise NULL without the quotes
                                       subjectsDropped = na_if(subjectsDropped, "NULL"),
                                       subjectsAdded = na_if(subjectsAdded, "NULL")
                                      )

        
        return(subject_changes)
    }


createSubjectGroups <-

    function(award_data, focus_subject, subject_filter = "", generate_common_subject_label = FALSE, overwrite_subject_group = NULL) {

        if (is.null(subject_filter))
            subject_filter <- ""

        award_data <- award_data %>%

            distinct(Subject) %>%
            filter(str_detect(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)) &
                   ((str_trim(subject_filter) == "") | !str_detect(Subject, regex(paste0(subject_filter, collapse = "|"), ignore_case = TRUE)))
                  ) %>%


            mutate(SubjectGroup = str_match(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)))

        if (!is.null(overwrite_subject_group)) {
          
            if (length(overwrite_subject_group == 1) & !is.na(overwrite_subject_group["all"])) {
                award_data <- award_data %>%
                    mutate(SubjectGroup = snakecase::to_upper_camel_case(overwrite_subject_group[enframe(overwrite_subject_group)$name]))
               
            } else {
                search_options <- paste0("^(", paste0(enframe(overwrite_subject_group)$name, collapse = "|"), ")$")
                
                # test first that there ARE matches ...
                if (award_data %>%
                        filter(str_detect(SubjectGroup, regex(search_options, ignore_case = TRUE))) %>%
                        nrow() > 0) {
                    
                    award_data <- award_data %>%
                        filter(str_detect(SubjectGroup, regex(search_options, ignore_case = TRUE), negate = TRUE)) %>%

                    bind_rows(award_data %>%
                                  filter(str_detect(SubjectGroup, regex(search_options, ignore_case = TRUE))) %>%

                                  left_join(enframe(overwrite_subject_group) %>%
                                                mutate(across(everything(), ~ str_to_title(.))) %>%
                                                rename_with(~ c("SubjectGroup", "SubjectGroupOverride"))
                                           ) %>%
                                  select(-SubjectGroup) %>%
                                  rename(SubjectGroup = SubjectGroupOverride)

                    ) # end bind_rows back to subject list
                }
            } # end if-else - checkinng for subject group overwrites
        }
        
        if (generate_common_subject_label)
            award_data <- award_data %>%
                mutate(CommonSubjectLabel = str_match(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)))
        else
            award_data <- award_data %>%
                mutate(CommonSubjectLabel = NA)

        
        award_data <- award_data  %>%
          arrange(SubjectGroup, Subject)


        invisible(award_data%>%
                    mutate_at(vars(everything()), as.character))
    }


getEducationAuthority <-
    function(local_authority, authorities) {
      
        if (is.na(local_authority) | !(local_authority %in% authorities$LocalAuthority))
            return(NA)
        
        row_number <- which(local_authority == deframe(authorities["LocalAuthority"]))
        return(deframe(authorities["EducationAuthority"])[[row_number]])
    }


# currently leaving DB connection detail in here but actually running where used

# dbDisconnect(dbConn)
## unlink(paste0(path_to_file_store, "/sqa-data-db.sqlite"))



