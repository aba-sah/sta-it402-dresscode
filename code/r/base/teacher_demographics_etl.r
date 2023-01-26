
#dbConn <- dbConnect(RSQLite::SQLite(), paste0(path_to_file_store, "/sqa-data-db.sqlite"))
#dbConn


teacher_census <- dbGetQuery(dbConn, paste("SELECT DISTINCT Year, LocalAuthority, Subject,",
                                             "SubjectGroup, CommonSubjectLabel, TeacherFTE FROM teacher_census",
                                           "LEFT JOIN subject_groups",
                                               "USING(Subject, Year)"
                                         )) %>%
    mutate(across(LocalAuthority, ~ fct_relevel(., "Grant Aided", "All local authorities", after = Inf)),
           across(c(Subject, SubjectGroup), ~ fct_inorder(.)),
           across(c(Subject, SubjectGroup), ~ fct_relevel(., sort(levels(.)[str_detect(levels(.),
                                                                                       regex("computing", ignore_case = TRUE))]))),
          
           across(CommonSubjectLabel, as.factor), 
           across(Year, as.ordered),
          ) 



teacher_fte_local_authority_by_age <- dbGetQuery(dbConn, "SELECT * FROM teacher_fte_local_authority_by_age") %>%
    mutate(across(Year, as.ordered),
           across(Age, ~ fct_relevel(., ">= 55", after = Inf)),
           across(LocalAuthority, ~ fct_relevel(., "Grant Aided", "All local authorities", after = Inf))
          )

teacher_fte_main_subject_by_age <- dbGetQuery(dbConn, paste("SELECT DISTINCT Year, Subject, SubjectGroup, CommonSubjectLabel, Age, TeacherFTE",
                                    "FROM teacher_fte_main_subject_by_age",
                                        "LEFT JOIN subject_groups",
                                        "USING(Subject, Year)"
                               )) %>%
    mutate(across(Year, as.ordered),
           across(Age, ~ fct_relevel(., ">= 60", after = Inf)),
           across(CommonSubjectLabel, as.factor),
           across(c(Subject, SubjectGroup), ~ fct_inorder(.)),
           across(c(Subject, SubjectGroup), ~ fct_relevel(., sort(levels(.)[str_detect(levels(.),
                                                                                       regex("computing", ignore_case = TRUE))]))),          
          )


teacher_fte_local_authority_by_gender <- dbGetQuery(dbConn, "SELECT * FROM teacher_fte_local_authority_by_gender") %>%
    mutate(across(LocalAuthority, ~ fct_relevel(., "Grant Aided", "All local authorities", after = Inf)),
           across(Year, as.ordered),
          ) 


teacher_fte_main_subject_by_gender <- dbGetQuery(dbConn, "SELECT * FROM teacher_fte_main_subject_by_gender")







#dbDisconnect(dbConn)