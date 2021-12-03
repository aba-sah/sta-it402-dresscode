
##

school_rolls_from_1966 <- school_rolls_from_1966 %>%
    mutate_at(vars(year), as.ordered) %>%
    mutate_at(vars(secondary_schools, secondary_pupils, base_year_census), as.integer) 

base_year <- 1996


##
# full dataset
##
sqa_qualifications_data <- bind_rows(dbGetQuery(dbConn, paste("SELECT A.*, B.NoOfStudents AS AllEntries",
                                                       "FROM sqa_data A, sqa_data B",
                                                       "WHERE (A.gender = 'female') AND (A.grade = 'Entries') AND",
                                                                 "(B.gender = 'all') AND (B.grade = 'Entries') AND",
                                                                 "(A.gender <> B.gender) AND (A.qualification = B.qualification) AND",
                                                                 "(A.year = B.year) AND (A.Subject = B.Subject)"
                                                      )),
                              dbGetQuery(dbConn, paste("SELECT A.*, B.NoOfStudents AS AllEntries",
                                                         "FROM sqa_data A, sqa_data B",
                                                         "WHERE (A.gender = 'male') AND (A.grade = 'Entries') AND",
                                                                 "(B.gender = 'all') AND (B.grade = 'Entries') AND",
                                                                 "(A.gender <> B.gender) AND (A.qualification = B.qualification) AND",
                                                                 "(A.year = B.year) AND (A.Subject = B.Subject)"
                                                      ))
          ) %>%

    arrange(qualification, year) %>%
    mutate_at(vars(Subject, gender, grade), as.factor) %>%
    mutate_at(vars(qualification), as.factor) %>%
    mutate_at(vars(year), as.ordered) %>%

    mutate(PercentageOfStudents = (NoOfStudents / AllEntries))

#head(sqa_qualifications_data, 15)

### ###


##
# order qualifications by SCQFLevel and start year
##
sqa_qualifications_ordering <- sqa_qualifications_data %>%
    distinct(qualification) %>%
    mutate_at(vars(qualification), as.character) %>%

    mutate(qualification_label = if_else(qualification == toupper(qualification),
                                         qualification,
                                         snakecase::to_title_case(qualification))) %>%

    left_join(sqa_qualification_list %>%

                select(QualificationId, SCQFLevel, DataStartYear) %>%
                mutate_at(vars(SCQFLevel, DataStartYear), as.integer),

              by = c("qualification" = "QualificationId")) %>%

    arrange(SCQFLevel, DataStartYear, qualification)

sqa_qualifications_ordering < sqa_qualifications_ordering %>%
    mutate(qualification_label = if_else(qualification == toupper(qualification),
                                         qualification,
                                         snakecase::to_title_case(qualification))) %>%
    relocate(qualification_label, .after = qualification)

#sqa_qualifications_ordering

sqa_qualifications_data <- sqa_qualifications_data %>%
    mutate_at(vars(qualification), ~ fct_relevel(., levels = sqa_qualifications_ordering$qualification))
#levels(sqa_qualifications_data$qualification)

### ###



##
# gender distribution
##
gender_distribution <- bind_rows(dbGetQuery(dbConn, paste("SELECT A.year, A.qualification, A.gender, SUM(A.NoOfStudents) AS NoOfStudents, SUM(B.NoOfStudents) AS AllEntries",
                                                                                       "FROM sqa_data A, sqa_data B",
                                                             "WHERE ", 
                                                                 "(A.gender = 'female') AND (A.grade = 'Entries') AND",
                                                                 "(B.gender = 'all') AND (B.grade = 'Entries') AND", 
                                                                 "(A.gender <> B.gender) AND (A.qualification = B.qualification) AND",
                                                                 "(A.year = B.year) AND (A.Subject = B.Subject)",
                                                             "GROUP BY A.year, A.qualification",
                                                             "ORDER BY A.qualification, A.year"
                                                      )),

                                    dbGetQuery(dbConn, paste("SELECT A.year, A.qualification, A.gender, SUM(A.NoOfStudents) AS NoOfStudents, SUM(B.NoOfStudents) AS AllEntries",
                                                                                           "FROM sqa_data A, sqa_data B",
                                                             "WHERE", 
                                                                 "(A.gender = 'male') AND (A.grade = 'Entries') AND",
                                                                 "(B.gender = 'all') AND (B.grade = 'Entries') AND", 
                                                                 "(A.gender <> B.gender) AND (A.qualification = B.qualification) AND",
                                                                 "(A.year = B.year) AND (A.Subject = B.Subject)",
                                                             "GROUP BY A.year, A.qualification",
                                                             "ORDER BY A.qualification, A.year"
                                                                                          ))
                                 ) %>%

    arrange(qualification, year) %>%
    mutate_at(vars(qualification), ~ factor(., levels = sqa_qualifications_ordering$qualification)) %>%
    mutate_at(vars(gender), as.factor) %>%
    mutate_at(vars(year), as.ordered) %>%

    mutate(PercentageOfStudents = (NoOfStudents / AllEntries)) 

### ###


##
# and update gender_distribution ... and normalise using school rolls
##
gender_distribution <- gender_distribution %>%
    #mutate_at(vars(qualification), ~ fct_relevel(., sqa_qualifications_ordering$qualification)) %>%

    mutate_at(vars(year), as.character) %>%
    
    left_join(school_rolls_from_1966 %>%              
                  select(year, secondary_pupils, index_factor)
              ) %>%
    relocate(index_factor, .after = qualification) %>%
    relocate(secondary_pupils, .after = qualification) %>% 
    mutate_at(vars(year), as.ordered) %>%
    
    mutate(NoOfStudents_orig = NoOfStudents, 
           AllEntries_orig = AllEntries) %>%
    mutate_at(vars(NoOfStudents, AllEntries), ~ as.integer(round(. / index_factor))) %>%
    
    relocate(NoOfStudents_orig, .before = NoOfStudents) %>% 
    relocate(AllEntries_orig, .before = AllEntries)


#levels(gender_distribution$qualification)
#dim(gender_distribution)
#tail(gender_distribution, 15)


gender_column_labels <- levels(gender_distribution$gender)

### ###



##
# computing uptake across all qualifications
##
focus_subject <- "computing" 

computing_uptake <- bind_rows(dbGetQuery(dbConn, paste("SELECT A.*, B.NoOfStudents AS AllEntries",
                                                       "FROM sqa_data A, sqa_data B",
                                                       "WHERE (A.Subject LIKE ", paste0("'%", focus_subject, "%'"), ") AND",
                                                                 "(A.gender = 'female') AND (A.grade = 'Entries') AND",
                                                                 "(B.gender = 'all') AND (B.grade = 'Entries') AND", 
                                                                 "(A.gender <> B.gender) AND (A.qualification = B.qualification) AND",
                                                                 "(A.year = B.year) AND (A.Subject = B.Subject)"
                                                      )),
                              dbGetQuery(dbConn, paste("SELECT A.*, B.NoOfStudents AS AllEntries",
                                                         "FROM sqa_data A, sqa_data B",
                                                         "WHERE (A.Subject LIKE ", paste0("'%", focus_subject, "%'"), ") AND",
                                                                 "(A.gender = 'male') AND (A.grade = 'Entries') AND",
                                                                 "(B.gender = 'all') AND (B.grade = 'Entries') AND", 
                                                                 "(A.gender <> B.gender) AND (A.qualification = B.qualification) AND",
                                                                 "(A.year = B.year) AND (A.Subject = B.Subject)"
                                                      ))
          ) %>%

    arrange(qualification, year) %>%
    mutate_at(vars(Subject, gender, grade), as.factor) %>%
    mutate_at(vars(qualification), ~ factor(., levels = sqa_qualifications_ordering$qualification))  %>%
    mutate_at(vars(year), as.ordered) %>%

    mutate(PercentageOfStudents = (NoOfStudents / AllEntries)) %>%
    mutate(Subject.label = eval(formatted_subject_labels_as_expr)) 


### ###


##
# computing across qualifications
##

computing_offering_all_qualifications <- dbGetQuery(dbConn, paste("SELECT year, qualification FROM sqa_data",
                         "GROUP BY year, qualification",
                         "ORDER BY qualification, year"
                        )
                   ) %>%

    left_join(dbGetQuery(dbConn, paste("SELECT year, qualification, Subject AS computing_offered FROM sqa_data ",
                                       "WHERE (Subject LIKE ", paste0("'%", focus_subject, "%'"), ")",
                                       "GROUP BY year, qualification ",
                                       "ORDER BY qualification, year"
                                      )
                   )
            ) %>%

    mutate_at(vars(computing_offered), ~ !is.na(.)) %>%

    mutate_at(vars(year), as.ordered) %>%
    mutate_at(vars(qualification), ~ factor(., levels = sqa_qualifications_ordering$qualification))



time_period_axis_breaks <-
    data.frame(year = levels(computing_offering_all_qualifications$year),
               tick_label = as.logical(seq_len(length(levels(computing_offering_all_qualifications$year))) %% 2)) %>%

        mutate_at(vars(year), as.character) %>%
        mutate_at(vars(tick_label), ~ if_else(., year, ""))


### ###



##
#
##

