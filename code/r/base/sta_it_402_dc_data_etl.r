
##

dbConn <- dbConnect(RSQLite::SQLite(), paste0(path_to_file_store, "/sqa-data-db.sqlite"))
dbConn


sqa_qualification_list <- dbGetQuery(dbConn, "SELECT * FROM sqa_qualification_list")

# default for normalising pupil counts across years
school_rolls_from_1966 <- dbGetQuery(dbConn, "SELECT * FROM school_rolls") %>%
    mutate(across(year, as.ordered),
           across(c(secondary_schools, secondary_pupils, base_year_census), as.integer),
           across(predicted, as.logical)
          )

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
    mutate(across(where(is.character), ~ str_squish(.))) %>%

    arrange(qualification, year) %>%
    mutate(across(where(is.character), ~ str_squish(.)),
           across(c(NoOfStudents, AllEntries), as.integer),
           across(c(Subject, gender, grade, qualification), as.factor),
           across(year, as.ordered)) %>%

    mutate(PercentageOfStudents = (NoOfStudents / AllEntries))

#head(sqa_qualifications_data, 15)

### ###



##
# order qualifications by SCQFLevel and start year
##
sqa_qualifications_ordering <- sqa_qualifications_data %>%
    distinct(qualification) %>%
    mutate(across(qualification, as.character)) %>%

    mutate(qualification_label = if_else(qualification == toupper(qualification),
                                         qualification,
                                         snakecase::to_title_case(qualification))) %>%

    left_join(sqa_qualification_list %>%

                select(QualificationId, SCQFLevel, DataStartYear) %>%
                mutate(across(c(SCQFLevel, DataStartYear), as.integer)),

              by = c("qualification" = "QualificationId")) %>%

    arrange(SCQFLevel, DataStartYear, qualification)

sqa_qualifications_ordering <- sqa_qualifications_ordering %>%
    mutate(qualification_label = if_else(qualification == toupper(qualification),
                                         qualification,
                                         snakecase::to_title_case(qualification))) %>%
    relocate(qualification_label, .after = qualification)

#sqa_qualifications_ordering

sqa_qualifications_data <- sqa_qualifications_data %>%
    mutate(across(qualification, ~ fct_relevel(., sqa_qualifications_ordering$qualification)))

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
    mutate(across(where(is.character), ~ str_squish(.))) %>%

    arrange(qualification, year) %>%
    mutate(across(qualification, ~ factor(., levels = sqa_qualifications_ordering$qualification)),
           across(gender, as.factor),
           across(year, as.ordered),
           across(c(NoOfStudents, AllEntries), as.integer),
           PercentageOfStudents = (NoOfStudents / AllEntries)
          )

### ###



##
# and update gender_distribution ... and normalise using school rolls
##
gender_distribution <- gender_distribution %>%
    #mutate(across(qualification, ~ fct_relevel(., sqa_qualifications_ordering$qualification))) %>%

    mutate(across(year, as.character)) %>%

    left_join(school_rolls_from_1966 %>%
                  select(year, secondary_pupils, index_factor)
              ) %>%
    relocate(c(secondary_pupils, index_factor), .after = qualification) %>%
    mutate(across(year, as.ordered)) %>%

    mutate(NoOfStudents_orig = NoOfStudents,
           AllEntries_orig = AllEntries,
           across(c(NoOfStudents, AllEntries), ~ as.integer(round(. / index_factor)))
           ) %>%

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
focus_subject <- default_focus_subject

#focus_subject_filter <- ""
#
#for (i in seq_along(focus_subject)) {
#
#    focus_subject_filter <- paste0(focus_subject_filter, "(A.Subject LIKE '%", focus_subject[i], "%')")
#    if (i < length(focus_subject))
#        focus_subject_filter <- paste(focus_subject_filter, "OR ")
#}
# works but expensive, need to test using joins
focus_subject_filter <- paste(paste("(A.Subject LIKE ", paste0("'%", focus_subject, "%')")), collapse = " OR ")

computing_uptake <- bind_rows(dbGetQuery(dbConn, paste("SELECT A.*, B.NoOfStudents AS AllEntries",
                                                       "FROM sqa_data A, sqa_data B",
                                                       "WHERE (", focus_subject_filter, ") AND",
                                                                 "(A.gender = 'female') AND (A.grade = 'Entries') AND",
                                                                 "(B.gender = 'all') AND (B.grade = 'Entries') AND",
                                                                 "(A.gender <> B.gender) AND (A.qualification = B.qualification) AND",
                                                                 "(A.year = B.year) AND (A.Subject = B.Subject)"
                                                      )),
                              dbGetQuery(dbConn, paste("SELECT A.*, B.NoOfStudents AS AllEntries",
                                                         "FROM sqa_data A, sqa_data B",
                                                         "WHERE (", focus_subject_filter, ") AND",
                                                                 "(A.gender = 'male') AND (A.grade = 'Entries') AND",
                                                                 "(B.gender = 'all') AND (B.grade = 'Entries') AND",
                                                                 "(A.gender <> B.gender) AND (A.qualification = B.qualification) AND",
                                                                 "(A.year = B.year) AND (A.Subject = B.Subject)"
                                                      ))
          ) %>%
    mutate(across(where(is.character), ~ str_squish(.)))%>%

    arrange(qualification, year) %>%
    mutate(across(c(Subject, gender, grade), as.factor),
           across(qualification, ~ factor(., levels = sqa_qualifications_ordering$qualification)),
           across(year, as.ordered),

           across(c(NoOfStudents, AllEntries), as.integer),
           PercentageOfStudents = (NoOfStudents / AllEntries),
           Subject.label = eval(formatted_subject_labels_as_expr)
          )


##
# and update computing_uptake ... normalise using school rolls
##

computing_uptake <- computing_uptake %>%

    mutate(across(year, as.character)) %>%

    left_join(school_rolls_from_1966 %>%
                  select(year, secondary_pupils, index_factor)
              ) %>%
    relocate(c(qualification, secondary_pupils, index_factor), .after = year) %>%
    mutate(across(year, as.ordered)) %>%

    mutate(NoOfStudents_orig = NoOfStudents,
           AllEntries_orig = AllEntries,
           across(c(NoOfStudents, AllEntries), ~ as.integer(round(. / index_factor)))
          ) %>%

    relocate(NoOfStudents_orig, .before = NoOfStudents) %>%
    relocate(AllEntries_orig, .before = AllEntries)

#head(computing_uptake, 15)

### ###



##
# computing across qualifications
##

focus_subject <- default_focus_subject
#focus_subject_filter <- ""

#for (i in seq_along(focus_subject)) {
#
#    focus_subject_filter <- paste0(focus_subject_filter, "(Subject LIKE '%", focus_subject[i], "%')")
#    if (i < length(focus_subject))
#        focus_subject_filter <- paste(focus_subject_filter, "OR ")
#}
# works but expensive, need to test using joins
focus_subject_filter <- paste(paste("(Subject LIKE ", paste0("'%", focus_subject, "%')")), collapse = " OR ")

computing_offering_all_qualifications <- dbGetQuery(dbConn, paste("SELECT year, qualification FROM sqa_data",
                                                                     "GROUP BY year, qualification",
                                                                     "ORDER BY qualification, year"
                                                                    )
                                                               ) %>%

    left_join(dbGetQuery(dbConn, paste("SELECT year, qualification, Subject AS computing_offered FROM sqa_data ",
                                       "WHERE (", focus_subject_filter, ")",
                                       "GROUP BY year, qualification, computing_offered",
                                       "ORDER BY qualification, year, computing_offered"
                                      )
                   )
            ) %>%

    mutate(across(computing_offered, ~ !is.na(.))) %>%
    distinct %>%

    mutate(across(year, as.ordered),
           across(qualification, ~ factor(., levels = sqa_qualifications_ordering$qualification))
           )


rm(focus_subject_filter)



time_period_axis_breaks <-
    data.frame(year = levels(computing_offering_all_qualifications$year),
               tick_label = as.logical(seq_len(length(levels(computing_offering_all_qualifications$year))) %% 2)) %>%

        mutate(across(year, as.character),
               across(tick_label, ~ if_else(., year, ""))
               )

### ###
##
# selected subject groups
##

### ###
select_focus_subjects <- dbGetQuery(dbConn, "SELECT DISTINCT Subject, SubjectGroup, CommonSubjectLabel FROM subject_groups")

select_focus_subjects <- select_focus_subjects %>%
    mutate(across(everything(), as.factor),
           across(SubjectGroup, ~ fct_relevel(., sort(unique(select_focus_subjects$SubjectGroup)[str_detect(unique(select_focus_subjects$SubjectGroup),
                                                      regex("computing", ignore_case = TRUE))])))
           )


### ###
##
# gender distribution teacher FTE
##

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

##
#
##


### ###
##
# initial teacher intake - PGDE et al.
##

teachers_initial_intake_pgde_secondary_and_alt_routes <- dbGetQuery(dbConn, "SELECT * FROM teachers_initial_intake_pgde_secondary_and_alt_routes")


##
#
##


### ###
##
# initial teacher intake - university
##

teachers_initial_intake_university <- dbGetQuery(dbConn, "SELECT * FROM teachers_initial_intake_university") 

##
#
##


