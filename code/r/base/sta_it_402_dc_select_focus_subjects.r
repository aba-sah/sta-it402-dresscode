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


focus_subject <- c("art and design", "drama", "dance", "music" #,
                   #"creative digital media" - all digital media now in its subject group
                  )

subject_filter <- "digital media"
overwrite_subject_group <- c("all" = "expressive art")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          subject_filter,
                                                          TRUE,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()

focus_subject <- c("visual arts")

subject_filter <- "digital media"
overwrite_subject_group <- c("all" = "expressive art")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          generate_common_subject_label = TRUE,
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
                   "russian", "urdu", "mandarin", "chinese", "gaelic", "gaidhlig", "gàidhlig")
subject_filter <- "classical"
overwrite_subject_group <- c("all" = "modern languages")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          subject_filter,
                                                          TRUE,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            mutate(across(CommonSubjectLabel, ~ case_when(str_detect(Subject, regex("gaidhlig", ignore_case = TRUE)) ~ "Gaelic",
                                                                          TRUE ~ .
                                                                         ))) %>%
                            suppressWarnings()


focus_subject <- c("religious", "philosoph")
overwrite_subject_group <- c("all" = "religious and moral education")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            mutate(across(CommonSubjectLabel, ~ gsub("\\s\\(\\w+\\)", "", Subject))) %>%

                            suppressWarnings()


focus_subject <- c("history", "geography", "modern studies", "business")
overwrite_subject_group <- c("all" = "modern studies")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          generate_common_subject_label = TRUE,
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
           across(SubjectGroup, ~ fct_relevel(., sort(unique(select_focus_subjects$SubjectGroup)[str_detect(unique(select_focus_subjects$SubjectGroup),
                                                      regex("computing", ignore_case = TRUE))])))
           )



#levels(select_focus_subjects$Subject)
#levels(select_focus_subjects$SubjectGroup)
#select_focus_subjects

rm(subject_filter, overwrite_subject_group)

