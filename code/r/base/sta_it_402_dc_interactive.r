## ---- computing_offering_across_qualifications --------

options(repr.plot.width = 16, repr.plot.height = 4)


time_period_axis_breaks <- 
    data.frame(year = levels(computing_offering_all_qualifications$year), 
               tick_label = as.logical(seq_len(length(levels(computing_offering_all_qualifications$year))) %% 2)) %>%

        mutate_at(vars(year), as.character) %>%
        mutate_at(vars(tick_label), ~ if_else(., year, ""))


computing_offering_all_qualifications %>%
    mutate(fill_or_not = factor(if_else(computing_offered, as.character(qualification), NA_character_), 
                                levels = levels(computing_offering_all_qualifications$qualification))) %>%

    ggplot(aes(y = fct_rev(qualification), x = year)) +
        geom_point(aes(colour = qualification, 
                       fill = fill_or_not, 
                      ), 
                       size = 1, shape = 21) +
        scale_fill_discrete(na.value = "transparent") +
        scale_x_discrete(breaks = time_period_axis_breaks$year,
                         labels = time_period_axis_breaks$tick_label) +
        ylab(NULL) + xlab(NULL) + 
        ggtitle("Years Computing Subject Offered per Qualification") +
        dressCodeTheme +
        theme(legend.position = "none")    


## ---- gender_distribution_overall --------

plot1 <- gender_distribution %>%

    group_by(year, gender) %>%
    summarise(median = median(NoOfStudents, na.rm = TRUE), 
              mean = mean(NoOfStudents, na.rm = TRUE),
              NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate(Subject = "all") %>% 

        ggplot(aes(y = year, x = if_else(gender == "male", PercentageOfStudents, -PercentageOfStudents), fill = gender, 
                   text = paste0(gender, " - ", round(PercentageOfStudents, 2) * 100, "% in ", year))) +
            geom_segment(aes(xend = 0, yend = year, colour = gender), size = 1) +
            geom_point(aes(colour = gender), size = 0.75) +
            xlab("Gender Distribution Overall") + 
                 #paste0("\nGender Distribution Overall,\n", 
                 #       str_wrap(paste0(sqa_qualifications_ordering$qualification, collapse = ", "), width = 60, exdent = 4))) +
 
            ylab(NULL) +
            scale_y_discrete(breaks = time_period_axis_breaks$year, labels = time_period_axis_breaks$tick_label) +
            scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) + 
            #scale_shape_manual(values = c(6, 2)) + #-0x2640L, -0x2642L)) + # \u2640 and \u2642 
            scale_colour_manual(values = gender_colour_scheme) + 
            scale_fill_manual(values = gender_colour_scheme) + 
            guides(fill = guide_legend(reverse = TRUE), colour = FALSE) + 
            dressCodeTheme +
            theme(panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_line(), 
                  axis.text.x = element_text(size = 8, angle = 0, vjust = 0),
                  axis.text.y = element_markdown(size = 9), 
                  axis.title = element_text(size = 9),
                  legend.title = element_blank(), legend.position = "bottom")                                               

                               
convertToPlotly(plot1, renameTraces = TRUE, regexpr = genderRegex, height = 450, width = 440, maxMargin = 15) %>% 
    layout(legend = list(orientation = 'h', yanchor = "bottom")) %>%
    config(displayModeBar = FALSE)



## ---- summary_tables_gender_distribution_overall --------
       
gender_distribution %>%

    group_by(gender) %>%
    summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate_at(vars(PercentageOfStudents), ~ (round(., 3) * 100)) %>%
    select(c(gender, PercentageOfStudents)) %>%
    rename_with(~ c(" ", "% of all Students")) %>%

    kable() %>% #caption = "\nGender distribution overall") %>%
    kable_paper("striped", full_width = FALSE, position = "left")    
                               
                                   
## ---- gender_distribution_computing --------

plot1 <- computing_uptake %>%

    group_by(year, gender) %>%
    summarise(median = median(NoOfStudents, na.rm = TRUE), 
              mean = mean(NoOfStudents, na.rm = TRUE),
              NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate(Subject = "all") %>% 
                                   

            ggplot(aes(y = year, x = if_else(gender == "male", PercentageOfStudents, -PercentageOfStudents), fill = gender,
                       text = paste0(gender, " - ", round(PercentageOfStudents, 2) * 100, "% in ", year))) +
                geom_segment(aes(xend = 0, yend = year, colour = gender), size = 1) +
                geom_point(aes(colour = gender), size = 0.75) +
                xlab(paste0("Gender Distribution - ", str_to_title(focus_subject))) + 
                     #paste0("\nGender Distribution - ", str_to_title(focus_subject), "\n",
                     #       str_wrap(paste0(sqa_qualifications_ordering$qualification, collapse = ", "), width = 60, exdent = 4))) + 
                ylab(NULL) +
                scale_y_discrete(breaks = time_period_axis_breaks$year, labels = time_period_axis_breaks$tick_label) +
                scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) + 
                scale_colour_manual(values = gender_colour_scheme) + 
                scale_fill_manual(values = gender_colour_scheme) + 
                guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = TRUE)) + 
                dressCodeTheme +
                theme(panel.grid.major.y = element_blank(),
                      panel.grid.major.x = element_line(), 
                      axis.text.x = element_text(size = 8, angle = 0, vjust = 0),
                      axis.text.y = element_markdown(size = 9), 
                      axis.title = element_text(size = 9),
                      legend.title = element_blank(), 
                      legend.position = "bottom")             

convertToPlotly(plot1, renameTraces = TRUE, regexpr = genderRegex, height = 450, width = 440, maxMargin = 15) %>% 
    layout(legend = list(orientation = 'h', yanchor = "bottom")) %>%
    config(displayModeBar = FALSE)
                                   

## ---- summary_tables_gender_distribution_computing --------
       
computing_uptake %>%

    group_by(gender) %>%
    summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate_at(vars(PercentageOfStudents), ~ (round(., 3) * 100)) %>%
    select(c(gender, PercentageOfStudents)) %>%
    rename_with(~ c(" ", "% of all Students")) %>%

    kable() %>% #caption = "\nGender distribution, computing only") %>%
    kable_paper("striped", full_width = FALSE, position = "left")    


                                   
## ---- top_5_subjects_overall --------

options(repr.plot.width = 16, repr.plot.height = 10)

top_5_subjects  %>%
    filter(NoOfStudents > 0) %>%

    ggplot(aes(popularityOverTime, reorder(Subject.label, popularityOverTime, na.rm = TRUE), 
               shape = gender, colour = qualification, size = popularityOverTimeAndQualification, 
               #frame = year, ids = Subject, 
               #text = tooltip
              )) +
        geom_point(alpha = 0.65) +
        #scale_colour_manual(values = c("purple", "darkgreen")) + 
        #scale_shape_manual(values = c(-0x2640L, -0x2642L)) + # c("\u2640", "\u2642")
        scale_x_continuous(labels = function(x) scales::percent(abs(x))) + 
        ylab(NULL) + xlab("Popularity Over Time") +
        guides(size = FALSE) +        
        dressCodeTheme +
        theme(legend.title = element_blank(), legend.text = element_text(size = 9),
              axis.text.x = element_text(size = 11, angle = 0, vjust = 0),
              axis.text.y = element_markdown(size = 7), 
              axis.title = element_text(size = 14)
             ) +
        scale_colour_hue(c = 45, l = 45) %>%
    
        suppressMessages()
                          

## ---- top_5_subjects_ --------

qualifications <- levels(top_5_subjects$qualification)
years <- levels(top_5_subjects$year)

steps <- list()

plot_tmp <- plot_ly(height = 900, width = 1000)

for (i in seq_along(years)) {

    for (j in seq_along(qualifications)) {
     
        data_trace <- top_5_subjects %>%
                        filter((qualification == qualifications[j]) & (year == years[i])) 


        plot_tmp <- add_trace(plot_tmp, data = data_trace,

                                x = ~ popularityOverTimeAndQualification,
                                y = ~ Subject.label,
                                text = ~ tooltip,
                                visible = (i == 1),
                                name = ~qualification, # paste(qualification, "-", gender), 

                                type = "scatter", 
                                mode = "markers", 
                                marker = list(opacity = 0.65),
                                size = ~ popularityOverTime,
                                #symbol = ~ gender,
                                #symbols = c("-0x2640L", "-0x2642L"), # \u2640 and \u2642 
                                hoverinfo = "text", 
                                
                                color = ~ qualification,
                                showlegend = TRUE
                               )
     } # end iteration over qualification

    
    step <- list(args = list("visible", rep(FALSE, length(qualifications) * length(years))),
                 label = years[i], 
                 method = "restyle")
    for (j in seq_along(qualifications))    
        step$args[[2]][((i - 1) * length(qualifications)) + j] <- TRUE
    steps[[i]] <- step

} # end iteration over years


plot_tmp %>%
    layout(title = "Top 5 Subjects across Qualifications",
           xaxis = list(range = c(0, 1.05), tickformat = "%", tickfont = tickFont, title = list(text = "Popularity over Time")),
           yaxis = list(tickfont = list(size = 11), title = list(text = "", font = list(size = 20)), 
                        autotick = FALSE, type = "category", categoryarray = levels(top_5_subjects$Subject.label), categoryorder = "array"), 
           legend = list(itemsizing = "constant"), # being ignored ...
           margin = list(l = 5),
           sliders = list(list(active = 0,
                               currentvalue = list(prefix = "Year: "), 
                               font = list(color = "grey"),
                               steps = steps))
    )

                                   
                                   
## ---- computing_uptake_over_time --------
                           
focus_subject <- "computing"

plot_tmp <- plot_ly(height = 320, width = 1000, 
                    data = filter_focus_subject_summaries %>%
                                filter(str_detect(SubjectGroup, regex(focus_subject, ignore_case = TRUE))))


plot_tmp <- add_trace(plot_tmp,

                        x = ~ year,
                        y = ~ get(gender_column_labels[1]),
                        text = ~ paste0(gender_column_labels[1], " - ", 
                                        round(get(gender_column_labels[1]) / AllEntries, 3) * 100, "% (", 
                                            formatNumber(get(gender_column_labels[1])), ") in ", year),
                        name = gender_column_labels[1], 

                        type = "scatter", 
                        mode = "markers", 
                        hoverinfo = "text", 

                        color = gender_column_labels[1], 
                        colors = gender_colour_scheme,
                        showlegend = TRUE
                     )

plot_tmp <- add_trace(plot_tmp,

                        x = ~ year,
                        y = ~ get(gender_column_labels[2]),
                        text = ~ paste0(gender_column_labels[2], " - ", 
                                        round(get(gender_column_labels[2]) / AllEntries, 3) * 100, "% (", 
                                            formatNumber(get(gender_column_labels[2])), ") in ", year),
                        name = gender_column_labels[2], 

                        type = "scatter", 
                        mode = "markers", 
                        hoverinfo = "text", 

                        color = gender_column_labels[2], 
                        colors = gender_colour_scheme,
                        showlegend = TRUE
                     )
   
plot_tmp <- add_segments(plot_tmp, 
                         
                         x = ~ year, xend = ~ year, 
                         y = ~ get(gender_column_labels[1]), 
                         yend = ~ get(gender_column_labels[2]), 
                         name = ~ "gender_difference",  
                         line = list(width = 1),
                         opacity = 0.65,
                         color = ~ I(gender_colour_scheme[segment_encoding]), 
                         name = "gender_difference",
                         showlegend = FALSE
                     )
   
plot_tmp <- add_text(plot_tmp, 
                     
                     x = ~ year, 
                     y = ~ get(gender_column_labels[1]),
                     texttemplate = "%{text:%}",
                     
                     type = "scatter",
                     mode = "text", 
                     text = ~ round(get(gender_column_labels[1]) / AllEntries, 2),
                     textposition = "top right",
                     textfont = list(color = "gray75", size = 8),

                     name = paste0("gender_uptake_", gender_column_labels[1]),
                     showlegend = FALSE
                    )
   
plot_tmp <- add_text(plot_tmp, 
                     
                     x = ~ year, 
                     y = ~ get(gender_column_labels[2]),
                     texttemplate = "%{text:%}",
                     
                     type = "scatter",
                     mode = "text", 
                     text = ~ round(get(gender_column_labels[2]) / AllEntries, 2),
                     textposition = "top right",
                     textfont = list(color = "gray75", size = 8),

                     name = paste0("gender_uptake_", gender_column_labels[2]),
                     showlegend = FALSE
                    )

plot_tmp <- add_text(plot_tmp, 
                     
                     x = ~ year, 
                     y = ~ (get(gender_column_labels[1]) + get(gender_column_labels[2])) / 2, 
                     hovertemplate = ~ paste(tooltip, "<extra></extra>"), #hoverinfo doesn't work in text mode ...
                     hoverlabel = list(bgcolor = "white"),
                     texttemplate = "", #"%{text}%",
                     
                     type = "scatter",
                     mode = "text", 
                     text = ~ uptake_difference, 
                     #textposition = "middle right", # "outside",
                     textfont = list(color = "gray65", size = 8),

                     name = "gender_difference_detail",
                     showlegend = FALSE
                    )


plot_tmp %>%
    layout(legend = list(itemclick = FALSE, itemdoubleclick = FALSE), #orientation = 'h', yanchor = "bottom", y = -2.5), 
           xaxis = list(tickfont = list(size = 10), tickangle = -45, title = list(text = "")), #showgrid = FALSE),
           yaxis = list(tickfont = tickFont, showgrid = TRUE, showline = TRUE, 
                        title = list(text = paste("No. of Students -", str_to_title(focus_subject)), font = list(size = 16))), 
           margin = list(b = 5, t = -25)
          ) # end layout
                           
                                  
                                   
## ---- computing_uptake_over_time_by_qualification --------

focus_subject <- "computing"

qualifications <- levels(filter_focus_subjects$qualification)
genders <- gender_column_labels #levels(filter_focus_subjects$gender)
additional_traces <- 4

steps <- list()
plot_tmp <- plot_ly(height = 380, width = 1000)

for (i in seq_along(qualifications)) {
    
    plot_tmp <- add_trace(plot_tmp,
                          data = filter_focus_subjects %>%
                                  filter(str_detect(SubjectGroup, regex(focus_subject, ignore_case = TRUE))) %>%
                                  filter(qualification == qualifications[i]),

                            x = ~ year,
                            y = ~ get(gender_column_labels[1]),
                            text = ~ paste0(qualification, " - ", gender_column_labels[1], ": ", 
                                            round(get(gender_column_labels[1]) / AllEntries, 3) * 100, "% (", 
                                            formatNumber(get(gender_column_labels[1])), ") in ", year),
                            visible = (i == 1),
                            #name = ~ qualification, 

                            name = gender_column_labels[1], 

                            type = "scatter", 
                            mode = "markers", 
                            hoverinfo = "text", 

                            color = gender_column_labels[1], 
                            colors = gender_colour_scheme,
                            showlegend = TRUE#, #(i == 1),
                            #legendgroup = ~ get(gender_column_labels[1])
                          
                         )
    
    plot_tmp <- add_trace(plot_tmp,

                            x = ~ year,
                            y = ~ get(gender_column_labels[2]),
                            text = ~ paste0(qualification, " - ", gender_column_labels[2], ": ", 
                                            round(get(gender_column_labels[2]) / AllEntries, 3) * 100, "% (", 
                                            formatNumber(get(gender_column_labels[2])), ") in ", year),
                            visible = (i == 1),
                            #name = ~ qualification, 

                            name = gender_column_labels[2], 

                            type = "scatter", 
                            mode = "markers", 
                            hoverinfo = "text", 

                            color = gender_column_labels[2], 
                            colors = gender_colour_scheme,
                            showlegend = TRUE#,
                            #legendgroup = ~ get(gender_column_labels[2])
                          
                         )

   
    plot_tmp <- add_segments(plot_tmp, 

                             x = ~ year, xend = ~ year, 
                             y = ~ get(gender_column_labels[1]), 
                             yend = ~ get(gender_column_labels[2]), 
                             visible = (i == 1),
                             
                             name = ~ "gender_difference",  
                             line = list(width = 1),
                             opacity = 0.65,
                             color = ~ I(gender_colour_scheme[segment_encoding]), 
                             name = "gender_difference",
                             showlegend = FALSE
                         )

    plot_tmp <- add_text(plot_tmp, 

                         x = ~ year, 
                         y = ~ get(gender_column_labels[1]),
                         texttemplate = "%{text:%}",
                         visible = (i == 1),                            

                         type = "scatter",
                         mode = "text", 
                         text = ~ round(get(gender_column_labels[1]) / AllEntries, 2),
                         textposition = "top right",
                         textfont = list(color = "gray75", size = 8),

                         name = paste0("gender_uptake_", gender_column_labels[1]),
                         showlegend = FALSE
                        )

    plot_tmp <- add_text(plot_tmp, 

                         x = ~ year, 
                         y = ~ get(gender_column_labels[2]),
                         texttemplate = "%{text:%}",
                         visible = (i == 1),                            

                         type = "scatter",
                         mode = "text", 
                         text = ~ round(get(gender_column_labels[2]) / AllEntries, 2),
                         textposition = "top right",
                         textfont = list(color = "gray75", size = 8),

                         name = paste0("gender_uptake_", gender_column_labels[2]),
                         showlegend = FALSE
                        )

    plot_tmp <- add_text(plot_tmp, 

                         x = ~ year, 
                         y = ~ (get(gender_column_labels[1]) + get(gender_column_labels[2])) / 2, 
                         hovertemplate = ~ paste(tooltip, "<extra></extra>"), #hoverinfo doesn't work in text mode ...
                         hoverlabel = list(bgcolor = "white"),
                         texttemplate = "", #"%{text}%",
                         visible = (i == 1),                            

                         type = "scatter",
                         mode = "text", 
                         text = ~ uptake_difference, 
                         #textposition = "middle right", # "outside",
                         textfont = list(color = "gray65", size = 8),

                         name = "gender_difference_detail",
                         showlegend = FALSE
                        )


    step <- list(args = list("visible", rep(FALSE, (length(genders) + additional_traces) * length(qualifications))),
               label = qualifications[i], 
               method = "restyle")
    for (j in seq(1, length(genders) + additional_traces, 1)) #seq_along(genders))    
        step$args[[2]][((i - 1) * (length(genders) + additional_traces) ) + j] <- TRUE

    steps[[i]] = step  

} # end iteration over qualification


plot_tmp %>%
    layout(#title = paste("Gender Distribution -", str_to_title(focus_subject)),
           xaxis = list(title = list(text = ""), tickfont = list(size = 10), tickangle = -45, showgrid = FALSE, showline = FALSE),
           yaxis = list(tickfont = tickFont, showgrid = TRUE, showline = TRUE,
                        title = list(text = paste("No. of Students -", str_to_title(focus_subject)), font = list(size = 16)), rangemode = "tozero"), 
           legend = list(itemclick = FALSE, itemdoubleclick = FALSE),
           margin = list(l = 5),
           sliders = list(list(active = 0, pad = list(t = 35), 
                               currentvalue = list(prefix = "Qualification: "), 
                               font = list(color = "grey"),
                               steps = steps)) 
          ) # end layout
         
                           
                           
## ---- computing_uptake_over_time_cf_english_maths --------

focus_subject <- c("computing", "english", "math")

plot1 <- filter_focus_subject_summaries %>%
            filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%

                           
    ggplot(aes(x = fct_reorder(SubjectGroup, AllEntries))) +
        geom_segment(aes(y = get(gender_column_labels[1]), yend = get(gender_column_labels[2]), xend = SubjectGroup, 
                         colour = segment_encoding), alpha = 0.35, show.legend = FALSE) +

        # segment won't display tooltip - using invisible point to hold uptake difference
        geom_point(aes(y = (get(gender_column_labels[1]) + get(gender_column_labels[2])) / 2, size = 0, 
                       text = tooltip), alpha = 0) +

        geom_point(aes(y = get(gender_column_labels[1]), colour = gender_column_labels[1], 
                       text = paste0(SubjectGroup, " - ", gender_column_labels[1], ": ", 
                                     round(get(gender_column_labels[1]) / AllEntries, 2) * 100, "% (", 
                                            formatNumber(get(gender_column_labels[1])), ") in ", year)),
                   size = 1, alpha = 0.65, na.rm = TRUE) +
        geom_point(aes(y = get(gender_column_labels[2]), colour = gender_column_labels[2], 
                       text = paste0(SubjectGroup, " - ", gender_column_labels[2], ": ", 
                                     round(get(gender_column_labels[2]) / AllEntries, 2) * 100, "% (", 
                                     formatNumber(get(gender_column_labels[2])), ") in ", year)),
                   size = 1, alpha = 0.65, na.rm = TRUE) +
        scale_y_log10() + 
        xlab("") +
        ylab("") + #paste("No. of Students")) + 
        scale_colour_manual(values = gender_colour_scheme) + 
        dressCodeTheme + 
        theme(legend.position = "bottom", legend.title = element_blank(), 
              legend.text = element_text(size = 12),
              strip.text.x = element_text(size = 11),
              axis.title = element_text(size = 18, face = "plain"),
              axis.text.x = element_text(size = 8, angle = 90),
              axis.text.y = element_markdown(size = 12) 
              ) +
        facet_grid(. ~ year)
    
                           
convertToPlotly(plot1, height = 450, width = 2000) %>% 
    layout(legend = list(title = "", itemclick = FALSE, itemdoubleclick = FALSE), 
           #xaxis = list(tickfont = list(size = 8), title = list(text = "")),
           yaxis = list(tickfont = tickFont, title = list(text = "No. of Students", font = list(size = 16))), 
           margin = list(l = 5, t = -1))                       
                           
                               
## ---- computing_uptake_over_time_cf_english_maths_summary --------

focus_subject <- c("computing", "english", "math")

plot1 <- filter_focus_subject_summaries %>%
            filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
            mutate(tooltip = paste(SubjectGroup, "-", formatNumber(AllEntries), "entries in", year)) %>%

                           
    ggplot(aes(x = fct_reorder(SubjectGroup, AllEntries))) +
        geom_point(aes(y = AllEntries, shape = SubjectGroup, 
                       text = tooltip), size = 2, alpha = 0.65, na.rm = TRUE) +
        xlab("") + ylab("") + 
        dressCodeTheme + 
        theme(legend.position = "bottom", legend.title = element_blank(), 
              legend.text = element_text(size = 12),
              strip.text.x = element_text(size = 11),
              axis.title = element_text(size = 18, face = "plain"),
              axis.text.x = element_text(size = 8, angle = 90),
              axis.text.y = element_markdown(size = 12) 
              ) +
        facet_grid(. ~ year)
    

convertToPlotly(plot1, height = 350, width = 2000) %>% 
    layout(legend = list(itemclick = FALSE, itemdoubleclick = FALSE), 
           #xaxis = list(tickfont = list(size = 8), title = list(text = "")),
           yaxis = list(tickfont = tickFont, title = list(text = "No. of Students", font = list(size = 16))), 
           margin = list(l = 5, t = -1))
         
                           
                           
## ---- computing_uptake_over_time_cf_english_maths_by_qualification --------

plots_cf_english_maths <- list()
qualifications <- sqa_qualifications_ordering$qualification


for (i in seq_along(qualifications)) {


    plots_cf_english_maths[[i]] <- filter_focus_subjects %>%
        filter(str_detect(SubjectGroup, regex(paste(focus_subjects[[1]], collapse = "|"), ignore_case = TRUE))) %>%
        filter(qualification == qualifications[i]) %>% 
        mutate(SubjectGroup = droplevels(SubjectGroup),
               SubjectGroup = fct_reorder(SubjectGroup, AllEntries)) %>%


        group_by(year) %>%
        group_map(~ plot_ly(., width = 2000, height = 360) %>%

                    add_trace(
                            x = ~ SubjectGroup, 
                            y = ~ get(gender_column_labels[2]), 
                            color = gender_column_labels[2], 
                            colors = gender_colour_scheme,
                            type = "scatter", 
                            mode = "markers",
                            text = ~ paste0(SubjectGroup, " at ", qualification, " - ", gender_column_labels[2], ": ", 
                                          round(get(gender_column_labels[2]) / AllEntries, 3) * 100, "% of entries in ", year),
                            hoverinfo = "text",

                            showlegend =  FALSE, 
                            legendgroup = ~ gender_column_labels[2] 
                            ) %>%

                    add_trace(
                          x = ~ SubjectGroup, 
                          y = ~ get(gender_column_labels[1]), 
                          color = gender_column_labels[1], 
                          colors = gender_colour_scheme,
                          type = "scatter", 
                          mode = "markers",
                          text = ~ paste0(SubjectGroup, " at ", qualification, " - ", gender_column_labels[1], ": ", 
                                          round(get(gender_column_labels[1]) / AllEntries, 3) * 100, "% of entries in ", year),
                          hoverinfo = "text",

                          showlegend = FALSE,
                          legendgroup = ~ gender_column_labels[1]                   
                    ) %>%


                    add_trace(
                            x = ~ SubjectGroup, 
                            y = ~ (get(gender_column_labels[1]) + get(gender_column_labels[2])) / 2, 
                            text = ~ tooltip, 
                            color = I("transparent"),
                            type = "scatter", 
                            mode = "markers",
                            #marker = list(opacity = 0, size = 0), # ignores size - need opacity or colour to prevent visibility
                            text = ~ tooltip,
                            hoverinfo = "text",
                            hoverlabel = list(bgcolor="white"),

                            showlegend = FALSE                   
                    ) %>%

                    add_segments(
                             x = ~ SubjectGroup, xend = ~ SubjectGroup, 
                             y = ~ get(gender_column_labels[1]), 
                             yend = ~ get(gender_column_labels[2]), 
                             line = list(width = 1),
                             opacity = 0.65,
                             color = ~ segment_encoding, 
                             colors = gender_colour_scheme,
                             text = ~ tooltip, # shows in json but not actually on screen :S
                             hoverinfo = "text",
                        
                             name = "gender_difference",
                             showlegend = FALSE
                         ) %>%


                    add_annotations(
                        text = ~ year,
                        x = 0.5,
                        y = 1,

                        yref = "paper",
                        xref = "paper",
                        xanchor = "center",
                        yanchor = "bottom",
                        showarrow = FALSE,
                        font = list(size = 12)
                    ) %>%

                    layout(
                        yaxis = list(title = list(text = "No. of Students"), type = "log"),
                        xaxis = list(title = list(text = ""), tickfont = list(size = 10), tickangle = -90,
                                     autotick = FALSE, type = "category", categoryarray = ~ levels(SubjectGroup), categoryorder = "array"),
                        legend = list(itemclick = FALSE, itemdoubleclick = FALSE, tracegroupgap = 1, traceorder = "reversed"), 
                        #margin = list(l = 125, r = 15),
                        #hovermode = "x unified",
                        showlegend = TRUE,
                        
                        shapes = list(
                                    type = "rect",
                                    x0 = 0,
                                    x1 = 1,
                                    xref = "paper",

                                    y0 = 0, 
                                    y1 = 16,
                                    xanchor = 1,
                                    yanchor = 1,
                                    yref = "paper",

                                    ysizemode = "pixel",
                                    fillcolor = toRGB("gray80"),
                                    line = list(color = "transparent")
                        )
                    ), 
                  .keep = TRUE) %>%
            subplot(nrows = 1, margin = 0.0035, shareX = TRUE, shareY = TRUE) %>%

            plotly_build()


    female_legend <- FALSE
    male_legend <- FALSE

    for (j in seq_along(plots_cf_english_maths[[i]]$x$data)) {

        if (plots_cf_english_maths[[i]]$x$data[[j]]$name == gender_column_labels[1]) {

            plots_cf_english_maths[[i]]$x$data[[j]]$showlegend <- TRUE
            female_legend <- !female_legend

        } else if (plots_cf_english_maths[[i]]$x$data[[j]]$name == gender_column_labels[2]) {

            plots_cf_english_maths[[i]]$x$data[[j]]$showlegend <- TRUE
            male_legend <- !male_legend
        }

        if (female_legend & male_legend)
            break;
    }
}

rm(female_legend, male_legend)

                           
                           
## ---- computing_uptake_over_time_cf_sciences --------

focus_subject <- c("computing", "biology", "physics", "chemistry") 

plot1 <- filter_focus_subject_summaries %>%
            filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%

                           
    ggplot(aes(x = fct_reorder(SubjectGroup, AllEntries))) +
        geom_segment(aes(y = get(gender_column_labels[1]), yend = get(gender_column_labels[2]), xend = SubjectGroup, 
                         colour = segment_encoding), alpha = 0.35, show.legend = FALSE) +

        # segment won't display tooltip - using invisible point to hold uptake difference
        geom_point(aes(y = (get(gender_column_labels[1]) + get(gender_column_labels[2])) / 2, size = 0, 
                       text = tooltip), alpha = 0) +

        geom_point(aes(y = get(gender_column_labels[1]), colour = gender_column_labels[1], 
                       text = paste0(SubjectGroup, " - ", gender_column_labels[1], ": ", 
                                     round(get(gender_column_labels[1]) / AllEntries, 2) * 100, "% (", 
                                            formatNumber(get(gender_column_labels[1])), ") in ", year)),
                   size = 1, alpha = 0.65, na.rm = TRUE) +
        geom_point(aes(y = get(gender_column_labels[2]), colour = gender_column_labels[2], 
                       text = paste0(SubjectGroup, " - ", gender_column_labels[2], ": ", 
                                     round(get(gender_column_labels[2]) / AllEntries, 2) * 100, "% (", 
                                     formatNumber(get(gender_column_labels[2])), ") in ", year)),
                   size = 1, alpha = 0.65, na.rm = TRUE) +
        scale_y_log10() + 
        xlab("") +
        ylab("") + #paste("No. of Students")) + 
        scale_colour_manual(values = gender_colour_scheme) + 
        dressCodeTheme + 
        theme(legend.position = "bottom", legend.title = element_blank(), 
              legend.text = element_text(size = 12),
              strip.text.x = element_text(size = 11),
              axis.title = element_text(size = 18, face = "plain"),
              axis.text.x = element_text(size = 8, angle = 90),
              axis.text.y = element_markdown(size = 12) 
              ) +
        facet_grid(. ~ year)
    
                           
convertToPlotly(plot1, height = 450, width = 2500) %>% 
    layout(legend = list(title = "", itemclick = FALSE, itemdoubleclick = FALSE), 
           #xaxis = list(tickfont = list(size = 8), title = list(text = "")),
           yaxis = list(tickfont = tickFont, title = list(text = "No. of Students", font = list(size = 16))), 
           margin = list(l = 5, t = -1))             
                           
                               
## ---- computing_uptake_over_time_cf_sciences_summary --------

focus_subject <- c("computing", "biology", "physics", "chemistry") 

plot1 <- filter_focus_subject_summaries %>%
            filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
            mutate(tooltip = paste(SubjectGroup, "-", formatNumber(AllEntries), "entries in", year)) %>%

                           
    ggplot(aes(x = fct_reorder(SubjectGroup, AllEntries))) +
        geom_point(aes(y = AllEntries, shape = SubjectGroup, 
                       text = tooltip), size = 2, alpha = 0.65, na.rm = TRUE) +
        xlab("") + ylab("") + 
        dressCodeTheme + 
        theme(legend.position = "bottom", legend.title = element_blank(), 
              legend.text = element_text(size = 12),
              strip.text.x = element_text(size = 11),
              axis.title = element_text(size = 18, face = "plain"),
              axis.text.x = element_text(size = 8, angle = 90),
              axis.text.y = element_markdown(size = 12) 
              ) +
        facet_grid(. ~ year)
    

convertToPlotly(plot1, height = 350, width = 2500) %>% 
    layout(legend = list(itemclick = FALSE, itemdoubleclick = FALSE), 
           #xaxis = list(tickfont = list(size = 8), title = list(text = "")),
           yaxis = list(tickfont = tickFont, title = list(text = "No. of Students", font = list(size = 16))), 
           margin = list(l = 5, t = -1))

                           
                           
## ---- computing_uptake_over_time_cf_sciences_by_qualification --------



plots_cf_sciences <- list()
qualifications <- sqa_qualifications_ordering$qualification


for (i in seq_along(qualifications)) {


    plots_cf_sciences[[i]] <- filter_focus_subjects %>%
        filter(str_detect(SubjectGroup, regex(paste(focus_subjects[[2]], collapse = "|"), ignore_case = TRUE))) %>%
        filter(qualification == qualifications[i]) %>%
        mutate(SubjectGroup = droplevels(SubjectGroup),
               SubjectGroup = fct_reorder(SubjectGroup, AllEntries)) %>%


        group_by(year) %>%
        group_map(~ plot_ly(., width = 2500, height = 350) %>%

                    add_trace(
                            x = ~ SubjectGroup,
                            y = ~ get(gender_column_labels[2]),
                            color = gender_column_labels[2],
                            colors = gender_colour_scheme,
                            type = "scatter",
                            mode = "markers",
                            text = ~ paste0(SubjectGroup, " at ", qualification, " - ", gender_column_labels[2], ": ",
                                          round(get(gender_column_labels[2]) / AllEntries, 3) * 100, "% of entries in ", year),
                            hoverinfo = "text",

                            showlegend =  FALSE,
                            legendgroup = ~ gender_column_labels[2]
                            ) %>%

                    add_trace(
                          x = ~ SubjectGroup,
                          y = ~ get(gender_column_labels[1]),
                          color = gender_column_labels[1],
                          colors = gender_colour_scheme,
                          type = "scatter",
                          mode = "markers",
                          text = ~ paste0(SubjectGroup, " at ", qualification, " - ", gender_column_labels[1], ": ",
                                          round(get(gender_column_labels[1]) / AllEntries, 3) * 100, "% of entries in ", year),
                          hoverinfo = "text",

                          showlegend = FALSE,
                          legendgroup = ~ gender_column_labels[1]
                    ) %>%


                    add_trace(
                            x = ~ SubjectGroup,
                            y = ~ (get(gender_column_labels[1]) + get(gender_column_labels[2])) / 2,
                            text = ~ tooltip,
                            color = I("transparent"),
                            type = "scatter",
                            mode = "markers",
                            #marker = list(opacity = 0, size = 0), # ignores size - need opacity or colour to prevent visibility
                            text = ~ tooltip,
                            hoverinfo = "text",
                            hoverlabel = list(bgcolor="white"),

                            showlegend = FALSE
                    ) %>%

                    add_segments(
                             x = ~ SubjectGroup, xend = ~ SubjectGroup,
                             y = ~ get(gender_column_labels[1]),
                             yend = ~ get(gender_column_labels[2]),
                             line = list(width = 1),
                             opacity = 0.65,
                             color = ~ segment_encoding,
                             colors = gender_colour_scheme,
                             text = ~ tooltip, # shows in json but not actually on screen :S
                             hoverinfo = "text",

                             name = "gender_difference",
                             showlegend = FALSE
                         ) %>%


                    add_annotations(
                        text = ~ year,
                        x = 0.5,
                        y = 1,

                        yref = "paper",
                        xref = "paper",
                        xanchor = "center",
                        yanchor = "bottom",
                        showarrow = FALSE,
                        font = list(size = 12)
                    ) %>%

                    layout(
                        yaxis = list(title = list(text = "No. of Students"), type = "log"),
                        xaxis = list(title = list(text = ""), tickfont = list(size = 10), tickangle = -90,
                                     autotick = FALSE, type = "category", categoryarray = ~ levels(SubjectGroup), categoryorder = "array"),
                        legend = list(itemclick = FALSE, itemdoubleclick = FALSE, tracegroupgap = 1, traceorder = "reversed"),
                        #margin = list(l = 125, r = 15),
                        #hovermode = "x unified",
                        showlegend = TRUE,

                        shapes = list(
                                    type = "rect",
                                    x0 = 0,
                                    x1 = 1,
                                    xref = "paper",

                                    y0 = 0,
                                    y1 = 16,
                                    xanchor = 1,
                                    yanchor = 1,
                                    yref = "paper",

                                    ysizemode = "pixel",
                                    fillcolor = toRGB("gray80"),
                                    line = list(color = "transparent")
                        )
                    ),
                  .keep = TRUE) %>%
            subplot(nrows = 1, margin = 0.0035, shareX = TRUE, shareY = TRUE) %>%

            plotly_build()


    female_legend <- FALSE
    male_legend <- FALSE

    for (j in seq_along(plots_cf_sciences[[i]]$x$data)) {

        if (plots_cf_sciences[[i]]$x$data[[j]]$name == gender_column_labels[1]) {

            plots_cf_sciences[[i]]$x$data[[j]]$showlegend <- TRUE
            female_legend <- !female_legend

        } else if (plots_cf_sciences[[i]]$x$data[[j]]$name == gender_column_labels[2]) {

            plots_cf_sciences[[i]]$x$data[[j]]$showlegend <- TRUE
            male_legend <- !male_legend
        }

        if (female_legend & male_legend)
            break;
    }
}

rm(female_legend, male_legend)

                           
                           
## ----  --------


                           
                           
## ----  --------




