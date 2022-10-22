##
# selected subject groups
##

### ###
focus_subject <- c("english", "math", "arithmetic")
subject_filter <- "English for Speakers of Other Languages"
overwrite_subject_group <- c("math" = "maths",
                             "arithmetic" = "maths")

select_focus_subjects <- createSubjectGroups(sqa_qualifications_data,
                                             focus_subject,
                                             subject_filter,
                                             TRUE,
                                             overwrite_subject_group = overwrite_subject_group)


focus_subject <- c("physics", "chemistry", "biology")
subject_filter <- "technology"
overwrite_subject_group <- c("all" = "sciences")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          subject_filter,
                                                          TRUE,
                                                          overwrite_subject_group = overwrite_subject_group))


focus_subject <- c("engineering", "biotechnology", "technological studies")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject)) %>%
                            suppressWarnings() # already catering for empty subject_filter search


focus_subject <- c("art and design", "visual arts", "drama", "dance", "music" #,
                   #"creative digital media" - all digital media now in its subject group
                  )

subject_filter <- "digital media"
overwrite_subject_group <- c("all" = "expressive art")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          subject_filter,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()


focus_subject <- c("health and food technology", "physical education")
overwrite_subject_group <- c("all" = "health and well-being")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()


focus_subject <- c("french", "spanish", "french", "spanish", "italian", "german", "swedish", "portuguese", "greek",
                   "russian", "urdu", "mandarin", "chinese", "gaelic", "gaidhlig")
subject_filter <- "classical"
overwrite_subject_group <- c("all" = "modern languages")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          subject_filter,
                                                          TRUE,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()


focus_subject <- c("religious", "philosoph")
overwrite_subject_group <- c("all" = "religious and moral education")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()


focus_subject <- c("history", "geography", "modern studies", "business")
overwrite_subject_group <- c("all" = "modern studies")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()


focus_subject <- c("craft design technology", "wood", "metal", "home economics", "textile")
subject_filter <- "health"
overwrite_subject_group <- c("all" = "technology")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          subject_filter,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()


focus_subject <- c("digital media")
subject_filter <- "computing" # any with "computing" in subject name will be picked up with computing specific filter
overwrite_subject_group <- c("all" = other_computing_subject_flag)

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          subject_filter,
                                                          overwrite_subject_group = overwrite_subject_group) %>%
                                      mutate(across(CommonSubjectLabel, ~ coalesce(., snakecase::to_upper_camel_case("digital media"))))
                                     ) %>%
                            suppressWarnings()



focus_subject <- c("data science", "cyber", "digital", "mobile", "PC Passport", "software", "web")
subject_filter <- "digital media|comput" # any with "computing" in subject name will be picked up with computing specific filter
overwrite_subject_group <- c("all" = other_computing_subject_flag)

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          subject_filter,
                                                          overwrite_subject_group = overwrite_subject_group) #%>%
                                      #mutate(across(CommonSubjectLabel, ~ coalesce(., other_computing_subject_flag)))
                                     ) %>%
                            suppressWarnings()


focus_subject <- default_focus_subject
# computing - filter actually root - "comput"
# Information Systems
# Information and Communications Technology
overwrite_subject_group <- c("all" = "computing")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()


select_focus_subjects <- select_focus_subjects %>%
    mutate(across(SubjectGroup, ~ if_else(is.na(.), Subject, .)),
           across(everything(), as.factor),
           across(SubjectGroup, ~ fct_relevel(., levels(select_focus_subjects$SubjectGroup)[str_detect(levels(select_focus_subjects$SubjectGroup),
                                                      regex("computing", ignore_case = TRUE))]))
           )



#levels(select_focus_subjects$Subject)
#levels(select_focus_subjects$SubjectGroup)
#select_focus_subjects

rm(subject_filter, overwrite_subject_group)


##
# uptake over time - selected subjects
##


select_focus_subjects_normalised <- as.data.frame(levels(sqa_qualifications_data$year)) %>%

    full_join(as.data.frame(unique(sqa_qualifications_data$qualification)),
              by = character()) %>%

    full_join(as.data.frame(select_focus_subjects$Subject),
              by = character()) %>%

    full_join(as.data.frame(unique(sqa_qualifications_data$gender)),
              by = character()) %>%

    rename_with(~ c("year", "qualification", "Subject", "gender")) %>%
    mutate(Subject.label = factor(eval(formatted_subject_labels_as_expr)))


select_focus_subjects_normalised <- select_focus_subjects_normalised %>%
    mutate(across(year, as.character)) %>%

    full_join(sqa_qualifications_data %>%
                   filter(Subject %in% select_focus_subjects$Subject)
              ) %>%

    left_join(select_focus_subjects) %>%
    relocate(SubjectGroup, .after = Subject) %>%

    select(-c(NumberOfCentres, grade)) %>%
    mutate(across(year, as.ordered)) %>%

    mutate(pseudo_point = is.na(NoOfStudents),
           tooltip = if_else(pseudo_point, "",
                             paste0(gender, " at ", qualification, " - ", round(PercentageOfStudents, 2) * 100, "% of entries in ", year)),
           across(c(NoOfStudents, AllEntries, PercentageOfStudents), ~ replace_na(., -Inf))
           )


select_focus_subjects_normalised <- select_focus_subjects_normalised %>%

    mutate(across(year, as.character)) %>%

    left_join(school_rolls_from_1966 %>%
                  select(year, secondary_pupils, index_factor) %>%
                  mutate(across(year, as.character))
              ) %>%
    relocate(index_factor, .after = year) %>%
    relocate(secondary_pupils, .after = year) %>%
    mutate(across(year, as.ordered)) %>%


    mutate(NoOfStudents_orig = NoOfStudents,
           AllEntries_orig = AllEntries,
           across(c(NoOfStudents, AllEntries), ~ as.integer(round(. / index_factor)))
           ) %>%

    relocate(NoOfStudents_orig, .before = NoOfStudents) %>%
    relocate(AllEntries_orig, .before = AllEntries) %>%
    mutate(across(c(NoOfStudents, AllEntries), ~ replace_na(., -Inf)))




# detail - by qualification

filter_focus_subject_groups <- select_focus_subjects_normalised %>%

    filter(!pseudo_point) %>%

    filter(SubjectGroup %in% levels(select_focus_subjects$SubjectGroup)) %>%
    group_by(year, qualification, SubjectGroup, gender, pseudo_point) %>%

    summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE)) %>%

    pivot_wider(names_from = gender, values_from = NoOfStudents) %>%


    right_join(select_focus_subjects_normalised %>% # need to account for empty entries (by gender and subject)

                #filter(!pseudo_point) %>%
                mutate(across(AllEntries, ~ if_else(NoOfStudents == -Inf, NA_real_, .)),
                       across(c(NoOfStudents, AllEntries), ~ na_if(., -Inf))
                      ) %>%

                filter(SubjectGroup %in% levels(select_focus_subjects$SubjectGroup)) %>%
                group_by(year, qualification, SubjectGroup, Subject, gender, pseudo_point) %>%

                summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
                          AllEntries = sum(AllEntries, na.rm = TRUE)) %>%

                pivot_wider(names_from = gender, values_from = NoOfStudents) %>%

                group_by(year, qualification, SubjectGroup) %>%
                summarise(AllEntries = sum(AllEntries, na.rm = TRUE))
    ) %>% # end join
    ungroup() %>%

    mutate(uptake_difference = abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries,
           rounded_uptake_difference = round(abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries * 100, 1),

           across(c(all_of(gender_column_labels), AllEntries), ~ if_else(is.na(.) | (. == 0), -Inf, .)),
           across(pseudo_point, ~ is.na(pseudo_point))
          ) %>%

    mutate(tooltip = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                                 is.na(female) | (female == -Inf) ~ paste(SubjectGroup, "at", qualification, "- No female uptake; uptake by males", paste0(rounded_uptake_difference, "% in"), year),
                                 is.na(male) | (male == -Inf) ~ paste(SubjectGroup, "at", qualification, "- No male uptake; uptake by females", paste0(rounded_uptake_difference, "% in"), year),
                                 female > male ~ paste(SubjectGroup, "at", qualification, "- Uptake by females exceeds males by", paste0(rounded_uptake_difference, "% in"), year),
                                 female < male ~ paste(SubjectGroup, "at", qualification, "- Uptake by males exceeds females by", paste0(rounded_uptake_difference, "% in"), year),
                                 female == male ~ paste(SubjectGroup, "at", qualification, "- Gender uptake equal in ", year),
                                 TRUE ~ NA_character_),
           tooltip2 = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                                   is.na(female) | (female == -Inf) ~ paste(SubjectGroup, "at", qualification, "- No female uptake; male only class in", year),
                                   is.na(male) | (male == -Inf) ~ paste(SubjectGroup, "at", qualification, "- No male uptake; female only class in", year),
                                   female > male ~ paste(SubjectGroup, "at", qualification, "- Uptake by females exceeds males by", round(female / male, 1), "times", paste0("(", rounded_uptake_difference, "%)"), "in", year),
                                   female < male ~ paste(SubjectGroup, "at", qualification, "- Uptake by males exceeds females by", round(male / female, 1), "times", paste0("(", rounded_uptake_difference, "%)"), "in", year),
                                   female == male ~ paste(SubjectGroup, "at", qualification, "- Gender uptake equal in ", year),
                                   TRUE ~ NA_character_),
           segment_encoding = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                                        female > male ~ "female",
                                        female < male ~ "male",
                                        female == male ~ "gray75",
                                        TRUE ~ "")
          ) %>%
    #mutate_at(vars(tooltip), ~ if_else(is.na(.), ., paste(SubjectGroup, "at", qualification, "-", .))) %>% fails :@ - returns NA_character_ not NA
    select(-rounded_uptake_difference) %>%
    relocate(AllEntries, .before = uptake_difference)


# summary - across all qualifications

filter_focus_subject_group_summaries <- filter_focus_subject_groups %>%

    #filter(!pseudo_point) %>%
    mutate(across(c(all_of(gender_column_labels), AllEntries), ~ na_if(., -Inf))) %>%

    group_by(year, SubjectGroup) %>%
    summarise_at(vars(all_of(gender_column_labels), AllEntries), ~ sum(., na.rm = TRUE)) %>%
    mutate(uptake_difference = abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries,
           rounded_uptake_difference = round(abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries * 100, 1),
           across(c(all_of(gender_column_labels), AllEntries), ~ if_else(is.na(.) | (. == 0), -Inf, .))) %>%
    ungroup() %>%

    mutate(tooltip = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                           is.na(female) | (female == -Inf) ~ paste(SubjectGroup, "- No female uptake; uptake by males", paste0(rounded_uptake_difference, "% in"), year),
                           is.na(male) | (male == -Inf) ~ paste(SubjectGroup, "- No male uptake; uptake by females", paste0(rounded_uptake_difference, "% in"), year),
                           female > male ~ paste(SubjectGroup, "- Uptake by females exceeds males by", paste0(rounded_uptake_difference, "% in"), year),
                           female < male ~ paste(SubjectGroup, "- Uptake by males exceeds females by", paste0(rounded_uptake_difference, "% in"), year),
                           female == male ~ paste(SubjectGroup, "- Gender uptake equal in ", year),
                           TRUE ~ NA_character_),
           tooltip2 = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                           is.na(female) | (female == -Inf) ~ paste(SubjectGroup, "- No female uptake; male only class in", year),
                           is.na(male) | (male == -Inf) ~ paste(SubjectGroup, "- No male uptake; female only class in", year),
                           female > male ~ paste(SubjectGroup, "- Uptake by females exceeds males by", round(female / male, 1), "times", paste0("(", rounded_uptake_difference, "%)"), "in", year),
                           female < male ~ paste(SubjectGroup, "- Uptake by males exceeds females by", round(male / female, 1), "times", paste0("(", rounded_uptake_difference, "%)"), "in", year),
                           female == male ~ paste(SubjectGroup, "- Gender uptake equal in ", year),
                           TRUE ~ NA_character_),
          segment_encoding = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                                        female > male ~ "female",
                                        female < male ~ "male",
                                        female == male ~ "gray75",
                                        TRUE ~ "")
          )


### ###

# detail - by qualification and subect detail

filter_focus_subjects <- select_focus_subjects_normalised %>%

    filter(!pseudo_point) %>%

    filter(SubjectGroup %in% levels(select_focus_subjects$SubjectGroup)) %>%
    mutate(across(SubjectGroup, ~ coalesce(CommonSubjectLabel, .))) %>%
    select(- CommonSubjectLabel) %>%

    group_by(year, qualification, SubjectGroup, gender, pseudo_point) %>%
    summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE)) %>%

    pivot_wider(names_from = gender, values_from = NoOfStudents) %>%


    right_join(select_focus_subjects_normalised %>% # need to account for empty entries (by gender and subject)

                #filter(!pseudo_point) %>%
                mutate(across(AllEntries, ~ if_else(NoOfStudents == -Inf, NA_real_, .)),
                       across(c(NoOfStudents, AllEntries), ~ na_if(., -Inf))
                      ) %>%

                filter(SubjectGroup %in% levels(select_focus_subjects$SubjectGroup)) %>%
                mutate(across(SubjectGroup, ~ coalesce(CommonSubjectLabel, .))) %>%
                select(- CommonSubjectLabel) %>%

                group_by(year, qualification, SubjectGroup, Subject, gender, pseudo_point) %>%
                summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
                          AllEntries = sum(AllEntries, na.rm = TRUE)) %>%

                pivot_wider(names_from = gender, values_from = NoOfStudents) %>%

                group_by(year, qualification, SubjectGroup) %>%
                summarise(AllEntries = sum(AllEntries, na.rm = TRUE))
    ) %>% # end join
    ungroup() %>%


    mutate(uptake_difference = abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries,
           rounded_uptake_difference = round(abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries * 100, 1),

           across(c(all_of(gender_column_labels), AllEntries), ~ if_else(is.na(.) | (. == 0), -Inf, .))
          ) %>%
    mutate(across(pseudo_point, ~ is.na(pseudo_point)),

           #uptake_difference = abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries,
           #rounded_uptake_difference = round(abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries * 100, 1),
           tooltip = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                                 is.na(female) | (female == -Inf) ~ paste(SubjectGroup, "at", qualification, "- No female uptake; uptake by males", paste0(rounded_uptake_difference, "% in"), year),
                                 is.na(male) | (male == -Inf) ~ paste(SubjectGroup, "at", qualification, "- No male uptake; uptake by females", paste0(rounded_uptake_difference, "% in"), year),
                                 female > male ~ paste(SubjectGroup, "at", qualification, "- Uptake by females exceeds males by", paste0(rounded_uptake_difference, "% in"), year),
                                 female < male ~ paste(SubjectGroup, "at", qualification, "- Uptake by males exceeds females by", paste0(rounded_uptake_difference, "% in"), year),
                                 female == male ~ paste(SubjectGroup, "at", qualification, "- Gender uptake equal in ", year),
                                 TRUE ~ NA_character_),
           tooltip2 = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                                   is.na(female) | (female == -Inf) ~ paste(SubjectGroup, "at", qualification, "- No female uptake; male only class in", year),
                                   is.na(male) | (male == -Inf) ~ paste(SubjectGroup, "at", qualification, "- No male uptake; female only class in", year),
                                   female > male ~ paste(SubjectGroup, "at", qualification, "- Uptake by females exceeds males by", round(female / male, 1), "times", paste0("(", rounded_uptake_difference, "%)"), "in", year),
                                   female < male ~ paste(SubjectGroup, "at", qualification, "- Uptake by males exceeds females by", round(male / female, 1), "times", paste0("(", rounded_uptake_difference, "%)"), "in", year),
                                   female == male ~ paste(SubjectGroup, "at", qualification, "- Gender uptake equal in ", year),
                                   TRUE ~ NA_character_),
           segment_encoding = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                                        female > male ~ "female",
                                        female < male ~ "male",
                                        female == male ~ "gray75",
                                        TRUE ~ "")
          ) %>%
    select(-rounded_uptake_difference) %>%
    relocate(AllEntries, .before = uptake_difference)


# summary - by qualification and subect detail

filter_focus_subject_summaries <- filter_focus_subjects %>%

    #filter(!pseudo_point) %>%
    mutate(across(c(all_of(gender_column_labels), AllEntries), ~ na_if(., -Inf))) %>%

    group_by(year, SubjectGroup) %>%
    summarise_at(vars(all_of(gender_column_labels), AllEntries), ~ sum(., na.rm = TRUE)) %>%
    mutate(uptake_difference = abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries,
           rounded_uptake_difference = round(abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries * 100, 1),
           across(c(all_of(gender_column_labels), AllEntries), ~ if_else(is.na(.) | (. == 0), -Inf, .))) %>%
    ungroup() %>%

    mutate(tooltip = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                           is.na(female) | (female == -Inf) ~ paste(SubjectGroup, "- No female uptake; uptake by males", paste0(rounded_uptake_difference, "% in"), year),
                           is.na(male) | (male == -Inf) ~ paste(SubjectGroup, "- No male uptake; uptake by females", paste0(rounded_uptake_difference, "% in"), year),
                           female > male ~ paste(SubjectGroup, "- Uptake by females exceeds males by", paste0(rounded_uptake_difference, "% in"), year),
                           female < male ~ paste(SubjectGroup, "- Uptake by males exceeds females by", paste0(rounded_uptake_difference, "% in"), year),
                           female == male ~ paste(SubjectGroup, "- Gender uptake equal in ", year),
                           TRUE ~ NA_character_),
           tooltip2 = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                           is.na(female) | (female == -Inf) ~ paste(SubjectGroup, "- No female uptake; male only class in", year),
                           is.na(male) | (male == -Inf) ~ paste(SubjectGroup, "- No male uptake; female only class in", year),
                           female > male ~ paste(SubjectGroup, "- Uptake by females exceeds males by", round(female / male, 1), "times", paste0("(", rounded_uptake_difference, "%)"), "in", year),
                           female < male ~ paste(SubjectGroup, "- Uptake by males exceeds females by", round(male / female, 1), "times", paste0("(", rounded_uptake_difference, "%)"), "in", year),
                           female == male ~ paste(SubjectGroup, "- Gender uptake equal in ", year),
                           TRUE ~ NA_character_),
          segment_encoding = case_when((is.na(female) & is.na(male)) | ((female == -Inf) & (male == -Inf)) ~ NA_character_,
                                        female > male ~ "female",
                                        female < male ~ "male",
                                        female == male ~ "gray75",
                                        TRUE ~ "")
          )
          
### ###


##
# and update computing_uptake - all variants by gender
##
focus_subject_filter <- paste0("A.Subject IN (", paste(paste0("'", focus_subject, "'"), collapse = ", "), ")")

computing_uptake_all_variants <- bind_rows(dbGetQuery(dbConn, paste("SELECT A.*, B.NoOfStudents AS AllEntries",
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

           PercentageOfStudents = (NoOfStudents / AllEntries),
           Subject.label = eval(formatted_subject_labels_as_expr)
          )

##
# and update computing_uptake_all_variants ... normalise using school rolls
##

computing_uptake_all_variants <- computing_uptake_all_variants %>%

    mutate(across(year, as.character)) %>%

    left_join(school_rolls_from_1966 %>%
                  select(year, secondary_pupils, index_factor)
              ) %>%
    relocate(c(qualification, secondary_pupils, index_factor), .after = year) %>%
    mutate(across(year, as.ordered)) %>%

    mutate(NoOfStudents_orig = NoOfStudents,
           AllEntries_orig = AllEntries,
           across(c(NoOfStudents, AllEntries), ~ as.integer(round(. / index_factor)))
          )%>%

    relocate(NoOfStudents_orig, .before = NoOfStudents) %>%
    relocate(AllEntries_orig, .before = AllEntries)

focus_subject <- default_focus_subject # reset

## end - computing - all variants by gender

##
# top 5 subjects across qualifications
##

focus_subject <- c("english", "math", "arithmetic")
subject_filter <- "English for Speakers of Other Languages"

top_5_subjects <- sqa_qualifications_data %>%

    filter(!str_detect(Subject, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE)) &
           !str_detect(Subject, regex(subject_filter, ignore_case = TRUE))) %>%

    group_by(qualification, year, Subject, gender) %>%
    summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE)) %>%
    group_by(qualification, year, gender)%>%
    slice_max(NoOfStudents, n = 5) %>%
    ungroup()

focus_subject <- default_focus_subject


time_range <- top_5_subjects %>%
    select(year) %>%
    n_distinct()


top_5_subjects <- top_5_subjects %>%

    left_join(top_5_subjects %>%
                group_by(qualification) %>%
                summarise(subject_time_range = n_distinct(year))
             ) %>%

    left_join(top_5_subjects %>%
                group_by(Subject) %>%
                summarise(popularity_overall = n())
             )

top_5_subjects <- top_5_subjects %>%

    left_join(top_5_subjects %>%

                group_by(qualification, Subject, gender) %>%
                summarise(popularity = n(),
                          popularityOverTimeAndQualification = popularity / subject_time_range,
                          popularityOverTime = popularity /time_range) %>%
                distinct()
             ) %>%
    arrange(desc(popularity)) %>%

    mutate(Subject.label = eval(formatted_subject_labels_as_expr))

rm(subject_filter, time_range)


# normalisation

top_5_subjects <- top_5_subjects %>%
    mutate(across(year, as.character)) %>%

    left_join(school_rolls_from_1966 %>%
                  select(year, secondary_pupils, index_factor)
              ) %>%
    relocate(index_factor, .after = year) %>%
    relocate(secondary_pupils, .after = year) %>%
    mutate(across(year, as.ordered)) %>%

    mutate(NoOfStudents_orig = NoOfStudents,
           across(c(NoOfStudents), ~ as.integer(round(. / index_factor)))
          ) %>%

    relocate(NoOfStudents_orig, .before = NoOfStudents)


# deselect and padding
top_5_subjects <- top_5_subjects %>%

    select(-c(secondary_pupils, index_factor, NoOfStudents_orig)) %>%
    mutate(across(c(Subject, Subject.label), ~ fct_drop(.)))


top_5_subjects <- data.frame(year = levels(top_5_subjects$year)) %>%

    full_join(top_5_subjects %>%
                distinct(qualification, subject_time_range), by = character()) %>%

    full_join(top_5_subjects %>%
                distinct(Subject, Subject.label, popularity_overall), by = character()) %>%

    full_join(top_5_subjects %>%
                distinct(gender), by = character()) %>%
    mutate(across(c(year, qualification, Subject, Subject.label, gender), as.character)) %>%

    full_join(top_5_subjects) %>%

    mutate(across(year, as.ordered),
           across(c(qualification, Subject, Subject.label, gender), as.factor),

           across(c(Subject, Subject.label), ~ reorder(., popularityOverTime, na.rm = TRUE)),
           across(qualification, ~ fct_relevel(., sqa_qualifications_ordering$qualification))
          ) %>%
    arrange(qualification) %>%

    mutate(across(c(NoOfStudents, (starts_with("popularity") & !popularityOverTime)), ~ replace_na(., -Inf)))


top_5_subjects <- top_5_subjects %>%

    mutate(tooltip = case_when((NoOfStudents > 0) ~ paste(Subject.label, "at", qualification, "- with", NoOfStudents, gender, "entries in", year,
                                   "\npopularity over time at", qualification, " -", paste0(100 * round(popularityOverTimeAndQualification, 2), "%"),
                                   "\npopularity overall -", paste0(100 * round(popularityOverTime, 2), "%")),
                               (year == "Overall") ~ paste(Subject.label, "popularity over time for", paste0(gender, "s at"), qualification,
                                   "-", paste0(100 * round(popularityOverTimeAndQualification, 2), "%"),
                                   "\n", Subject.label, "popularity overall -", paste0(100 * round(popularityOverTime, 2), "%")),
                               TRUE ~ ""
                              ))
### ###




##
#
##



### ###
