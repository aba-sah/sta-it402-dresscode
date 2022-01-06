

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


focus_subject <- c("art and design", "visual arts", "drama", "dance", "music",
                   "creative digital media")

overwrite_subject_group <- c("all" = "expressive art")
select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
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



focus_subject <- c(default_focus_subject)
# "computing"
# Information Systems
overwrite_subject_group <- c("all" = "computing")

select_focus_subjects <- select_focus_subjects %>%
                            bind_rows(createSubjectGroups(sqa_qualifications_data,
                                                          focus_subject,
                                                          overwrite_subject_group = overwrite_subject_group)) %>%
                            suppressWarnings()


select_focus_subjects <- select_focus_subjects %>%
    mutate_at(vars(SubjectGroup), ~ if_else(is.na(.), Subject, .)) %>%
    mutate_at(vars(everything()), as.factor) %>%
    mutate_at(vars(SubjectGroup), ~ fct_relevel(., str_to_title("computing")))


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
    mutate_at(vars(year), as.character) %>%

    full_join(sqa_qualifications_data %>%
                   filter(Subject %in% select_focus_subjects$Subject)
              ) %>%

    left_join(select_focus_subjects) %>%
    relocate(SubjectGroup, .after = Subject) %>%

    select(-c(NumberOfCentres, grade)) %>%
    mutate_at(vars(year), as.ordered) %>%

    mutate(pseudo_point = is.na(NoOfStudents),
           tooltip = if_else(pseudo_point, "",
                             paste0(gender, " at ", qualification, " - ", round(PercentageOfStudents, 2) * 100, "% of entries in ", year))
           ) %>%
    mutate_at(vars(NoOfStudents, AllEntries, PercentageOfStudents), ~ replace_na(., -Inf))


select_focus_subjects_normalised <- select_focus_subjects_normalised %>%

    mutate_at(vars(year), as.character) %>%

    left_join(school_rolls_from_1966 %>%
                  select(year, secondary_pupils, index_factor) %>%
                  mutate_at(vars(year), as.character)
              ) %>%
    relocate(index_factor, .after = year) %>%
    relocate(secondary_pupils, .after = year) %>%
    mutate_at(vars(year), as.ordered) %>%


    mutate(NoOfStudents_orig = NoOfStudents,
           AllEntries_orig = AllEntries) %>%
    mutate_at(vars(NoOfStudents, AllEntries), ~ as.integer(round(. / index_factor))) %>%

    relocate(NoOfStudents_orig, .before = NoOfStudents) %>%
    relocate(AllEntries_orig, .before = AllEntries) %>%
    mutate_at(vars(NoOfStudents, AllEntries), ~ replace_na(., -Inf))


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


    mutate(uptake_difference = abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries,
           rounded_uptake_difference = round(abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries * 100, 1),

           #across(all_of(gender_column_labels), ~ if_else(is.na(.) | (. == 0), -Inf, .)),
           across(c(all_of(gender_column_labels), AllEntries), ~ if_else(is.na(.) | (. == 0), -Inf, .))
          ) %>%
    #mutate_at(vars(all_of(gender_column_labels), AllEntries), as.integer) %>%
    mutate_at(vars(pseudo_point), ~ is.na(pseudo_point)) %>%

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
    mutate_at(vars(SubjectGroup), ~ coalesce(CommonSubjectLabel, .)) %>%
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
                mutate_at(vars(SubjectGroup), ~ coalesce(CommonSubjectLabel, .)) %>%
                select(- CommonSubjectLabel) %>%

                group_by(year, qualification, SubjectGroup, Subject, gender, pseudo_point) %>%
                summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
                          AllEntries = sum(AllEntries, na.rm = TRUE)) %>%

                pivot_wider(names_from = gender, values_from = NoOfStudents) %>%

                group_by(year, qualification, SubjectGroup) %>%
                summarise(AllEntries = sum(AllEntries, na.rm = TRUE))
    ) %>% # end join


    mutate(uptake_difference = abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries,
           rounded_uptake_difference = round(abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries * 100, 1),

           #across(all_of(gender_column_labels), ~ if_else(is.na(.) | (. == 0), -Inf, .)),
           across(c(all_of(gender_column_labels), AllEntries), ~ if_else(is.na(.) | (. == 0), -Inf, .))
          ) %>%
    #mutate_at(vars(all_of(gender_column_labels), AllEntries), as.integer) %>%
    mutate_at(vars(pseudo_point), ~ is.na(pseudo_point)) %>%

    mutate(#uptake_difference = abs(coalesce(female, 0) - coalesce(male, 0)) / AllEntries,
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
    #mutate_at(vars(tooltip), ~ if_else(is.na(.), ., paste(SubjectGroup, "at", qualification, "-", .))) %>% fails :@ - returns NA_character_ not NA
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
# computing_uptake
##

computing_uptake <- computing_uptake %>%

    mutate_at(vars(year), as.character) %>%

    left_join(school_rolls_from_1966 %>%
                  select(year, secondary_pupils, index_factor)
              ) %>%
    relocate(index_factor, .after = year) %>%
    relocate(secondary_pupils, .after = year) %>%
    relocate(qualification, .after = year) %>%
    mutate_at(vars(year), as.ordered) %>%

    mutate(NoOfStudents_orig = NoOfStudents,
           AllEntries_orig = AllEntries) %>%
    mutate_at(vars(NoOfStudents, AllEntries), ~ as.integer(round(. / index_factor))) %>%

    relocate(NoOfStudents_orig, .before = NoOfStudents) %>%
    relocate(AllEntries_orig, .before = AllEntries)

#head(computing_uptake, 15)


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
    mutate_at(vars(year), as.character) %>%

    left_join(school_rolls_from_1966 %>%
                  select(year, secondary_pupils, index_factor)
              ) %>%
    relocate(index_factor, .after = year) %>%
    relocate(secondary_pupils, .after = year) %>%
    mutate_at(vars(year), as.ordered) %>%

    mutate(NoOfStudents_orig = NoOfStudents) %>%
    mutate_at(vars(NoOfStudents), ~ as.integer(round(. / index_factor))) %>%

    relocate(NoOfStudents_orig, .before = NoOfStudents)


# deselect and padding
top_5_subjects <- top_5_subjects %>%

    select(-c(secondary_pupils, index_factor, NoOfStudents_orig)) %>%
    mutate_at(vars(Subject, Subject.label), ~ fct_drop(.))


top_5_subjects <- data.frame(year = levels(top_5_subjects$year)) %>%

    full_join(top_5_subjects %>%
                distinct(qualification, subject_time_range), by = character()) %>%

    full_join(top_5_subjects %>%
                distinct(Subject, Subject.label, popularity_overall), by = character()) %>%

    full_join(top_5_subjects %>%
                distinct(gender), by = character()) %>%
    mutate_at(vars(year, qualification, Subject, Subject.label, gender), as.character) %>%

    full_join(top_5_subjects) %>%

    mutate_at(vars(year), as.ordered)  %>%
    mutate_at(vars(qualification, Subject, Subject.label, gender), as.factor) %>%

    mutate_at(vars(Subject, Subject.label), ~ reorder(., popularityOverTime, na.rm = TRUE)) %>%
    mutate_at(vars(qualification), ~ fct_relevel(., sqa_qualifications_ordering$qualification)) %>%
    arrange(qualification) %>%

    mutate_at(vars(NoOfStudents, (starts_with("popularity") & !popularityOverTime)), ~ replace_na(., -Inf))



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


