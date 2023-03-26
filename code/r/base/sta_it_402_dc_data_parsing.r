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
