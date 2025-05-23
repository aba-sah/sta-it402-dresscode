
#dbConn <- dbConnect(RSQLite::SQLite(), paste0(path_to_file_store, "/sqa-data-db.sqlite"))
#dbConn

teacher_census <- dbGetQuery(dbConn, paste("SELECT DISTINCT teacher_census.Year, LocalAuthority, teacher_census.Subject,",
                             "SubjectGroup, CommonSubjectLabel, TeacherFTE FROM teacher_census",
                          "LEFT JOIN subject_groups",
                          "ON (teacher_census.Subject = subject_groups.Subject) AND",
                                  "(teacher_census.Year = subject_groups.year)"
                         )) %>%
    mutate(across(LocalAuthority, ~ fct_relevel(., "Grant Aided", "All local authorities", "Scotland", after = Inf)),
           across(c(Subject, SubjectGroup), ~ fct_inorder(.)),
           across(c(Subject, SubjectGroup), ~ fct_relevel(., sort(levels(.)[str_detect(levels(.),
                                                                                       regex("computing", ignore_case = TRUE))]))),

           across(CommonSubjectLabel, as.factor),
           across(Year, ~ as.ordered(as.integer(.))),
          )


teacher_fte_local_authority_by_age <-  dbGetQuery(dbConn, "SELECT * FROM teacher_fte_local_authority_by_age") %>%
    mutate(across(Year, as.ordered),
           across(AgeRange, ~ fct_relevel(., ">= 55", after = Inf)),
           across(LocalAuthority, ~ fct_relevel(., "Grant Aided", "All local authorities", "Scotland", after = Inf))
          )


teacher_fte_main_subject_by_age <- dbGetQuery(dbConn, paste("SELECT DISTINCT Year, Subject, SubjectGroup, CommonSubjectLabel, AgeRange, TeacherFTE",
                                    "FROM teacher_fte_main_subject_by_age",
                                        "LEFT JOIN subject_groups",
                                        "USING(Subject, Year)"
                               )) %>%
    mutate(across(Year, as.ordered),
           across(AgeRange, ~ fct_relevel(., ">= 60", after = Inf)),
           across(CommonSubjectLabel, as.factor),
           across(c(Subject, SubjectGroup), ~ fct_inorder(.)),
           across(c(Subject, SubjectGroup), ~ fct_relevel(., sort(levels(.)[str_detect(levels(.),
                                                                                       regex("computing", ignore_case = TRUE))]))),
          ) 

teacher_fte_local_authority_by_gender <- dbGetQuery(dbConn, "SELECT * FROM teacher_fte_local_authority_by_gender ") %>%
    mutate(across(LocalAuthority, ~ fct_relevel(., "Grant Aided", "All local authorities", "Scotland", after = Inf)),
           across(Gender, ~ fct_relevel(., "all", after = Inf)),
           across(Year, ~ as.ordered(as.integer(.))),
          )


teacher_fte_main_subject_by_gender <- dbGetQuery(dbConn, paste("SELECT DISTINCT Year, Subject, SubjectGroup, CommonSubjectLabel, Gender,",
                                "TeacherFTE_Main, TeacherFTE_Other",
                                    "FROM teacher_fte_main_subject_by_gender",
                                "LEFT JOIN subject_groups",
                                "USING(Subject, Year)")) %>%

    mutate(across(Year, as.ordered),
           across(Gender, ~ fct_relevel(., "all", after = Inf)),

           across(c(Subject, SubjectGroup), ~ fct_inorder(.)),
           across(c(Subject, SubjectGroup), ~ fct_relevel(., sort(levels(.)[str_detect(levels(.),
                                                                                       regex("computing", ignore_case = TRUE))]))),
          )

teacher_fte_main_subject_by_gender <- teacher_fte_main_subject_by_gender %>%
    filter(Gender != "all") %>%

    rowwise() %>%
    mutate(TeacherFTE = sum(across(matches("TeacherFTE_\\w+")), na.rm = TRUE)) %>%

    group_by(Year, Gender) %>%
    mutate(across(TeacherFTE, list(median = median, mean = mean, total = sum), .names = "{.fn}_all_subjects")) %>%


    bind_rows(teacher_fte_main_subject_by_gender %>%
                filter(Gender == "all") %>%

                rowwise() %>%
                mutate(TeacherFTE = sum(across(matches("TeacherFTE_\\w+")), na.rm = TRUE)) %>%

                group_by(Year) %>%
                mutate(across(TeacherFTE, list(median = median, mean = mean, total = sum), .names = "{.fn}_all_subjects"))
    ) %>%
    ungroup() %>%
    arrange(Year)


distribution_teacher_age <- dbGetQuery(dbConn, "SELECT * FROM distribution_teacher_age") %>%

    mutate(across(c(Age, Year), as.ordered)
          )

teacher_fte_main_subject_by_age_start_year <- min(levels(teacher_fte_main_subject_by_age$Year), na.rm = TRUE)
teacher_fte_main_subject_by_age_end_year <- max(levels(teacher_fte_main_subject_by_age$Year), na.rm = TRUE)

dbDisconnect(dbConn)
