## ---- computing_offering_across_qualifications --------

options(repr.plot.width = 16, repr.plot.height = 6)

computing_offering_all_qualifications %>%
    mutate(fill_or_not = factor(if_else(computing_offered, as.character(qualification), NA_character_), 
                                levels = levels(computing_offering_all_qualifications$qualification))) %>%

    ggplot(aes(y = fct_rev(qualification), x = year)) +
        geom_point(aes(colour = qualification, 
                       fill = fill_or_not, 
                      ), 
                       size = 1, shape = 21) +
        #scale_fill_discrete(na.value = "transparent") +
        khroma::scale_colour_romaO(discrete = TRUE, 
                                 breaks = computing_offering_all_qualifications$qualification[1], 
                                 labels = "Qualification Offered") + 
        khroma::scale_fill_romaO(discrete = TRUE, 
                                 breaks = computing_offering_all_qualifications$qualification[1], 
                                 labels = "Computing Offered") + 
        scale_x_discrete(breaks = time_period_axis_breaks$year,
                         labels = time_period_axis_breaks$tick_label) +
        ylab(NULL) + xlab(NULL) + 
        ggtitle("Computing Subject Timelines") +
        dressCodeTheme +
        theme(axis.text.y = element_text(size = 8		),
              legend.position = "bottom",
              legend.title = element_blank(), 
              legend.margin = margin(0))    



## ---- gender_distribution_static_charts_overall --------

plot1 <- gender_distribution %>%

    group_by(year, gender) %>% *** update sum/mean ... etc.
    summarise(across(NoOfStudents, list(median = median, mean = mean, NoOfStudents = sum), na.rm = TRUE, .names = "{.fn}"),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    ungroup() %>%
    mutate(Subject = "All",
           tooltip = paste0(gender, " - ", round(PercentageOfStudents, 2) * 100, "% in ", year)) %>%

    ggplot(aes(y = year, x = if_else(gender == "male", PercentageOfStudents, -PercentageOfStudents),
               fill = gender, colour = gender, text = tooltip)) +
        geom_segment(aes(xend = 0, yend = year), size = 1.5) +
        geom_point(aes(colour = gender), size = 0.75) +
        #ggtitle(paste0("Gender Distribution - All Subjects")) +
        ylab("") +  xlab(paste0("\nGender Distribution - All Subjects")) + 
        # scale_y_discrete(breaks = time_period_axis_breaks$year, labels = time_period_axis_breaks$tick_label) +
        scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) +
        #scale_shape_manual(values = c(6, 2)) + #-0x2640L, -0x2642L)) + # \u2640 and \u2642 
        scale_colour_manual(values = gender_colour_scheme) + 
        scale_fill_manual(values = gender_colour_scheme) + 
        #guides(fill = guide_legend(reverse = TRUE), colour = "none") +
        dressCodeTheme +
        theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
              axis.text.y = element_text(size = 8), 
              axis.title = element_text(size = 11),
              panel.grid.major.y = element_blank(),
              #axis.text.y = element_text(size = 9, margin = margin(0, 0, 0, 0, unit = "cm")),
              panel.grid.major.x = element_line(),
              legend.title = element_blank(), 
              legend.position = "bottom")

#plot1
                                   
## ---- gender_distribution_overall --------

convertToPlotly(plot1, renameTraces = TRUE, regexpr = genderRegex, height = 580, width = 500, maxMargin = 1,
                yaxis = list(tickfont = list(size = 9))) %>%
    layout(xaxis = list(title = list(font = list(size = 14)), showline = TRUE, linecolor = "rgb(175, 175, 175)",
                        zeroline = TRUE),
          legend = list(y = -0.12, orientation = 'h', yanchor = "bottom", title = list(text = "")),
           margin = list(l = 5)) %>%
    config(displayModeBar = FALSE)
                                                  


## ---- summary_tables_gender_distribution_overall --------
       
gender_distribution %>%

    group_by(gender) %>%
    summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate(across(PercentageOfStudents, ~ (round(., 3) * 100))) %>%
    select(gender, PercentageOfStudents) %>%
    rename_with(~ c(" ", "% of all Students")) %>%

    kable() %>% #caption = "\nGender distribution overall") %>%
    kable_paper("striped", full_width = FALSE, position = "left")    


## ---- gender_distribution_static_charts_computing --------

focus_subject_label <- "computing / information systems / data science"

plot1 <- computing_uptake %>%

    group_by(year, gender) %>%
    summarise(across(NoOfStudents, list(median = median, mean = mean, NoOfStudents = sum), na.rm = TRUE, .names = "{.fn}"),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    ungroup() %>%
    mutate(Subject = "All",
           tooltip = paste0(gender, " - ", round(PercentageOfStudents, 2) * 100, "% in ", year)
          ) %>%

    ggplot(aes(y = year, x = if_else(gender == "male", PercentageOfStudents, -PercentageOfStudents), fill = gender,
               text = tooltip)) +
        geom_segment(aes(xend = 0, yend = year, colour = gender), size = 1.5) +
        geom_point(aes(colour = gender), size = 0.75) +
        #ggtitle(paste0("Gender Distribution - Computing")) +
        xlab("\nGender Distribution - All Computing") +
            #paste0("\nGender Distribution - ", str_to_title(focus_subject_label))) +
             #paste0("\nGender Distribution - ", str_to_title(focus_subject_label), "\n",
             #       str_wrap(paste0(sqa_qualifications_ordering$qualification, collapse = ", "), width = 60, exdent = 4))) +
        ylab(NULL) +
        #scale_y_discrete(breaks = time_period_axis_breaks$year, labels = time_period_axis_breaks$tick_label) +
        scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) +
        scale_colour_manual(values = gender_colour_scheme) +
        scale_fill_manual(values = gender_colour_scheme) +
        #guides(fill = guide_legend(reverse = TRUE), colour = guide_legend(reverse = TRUE)) +
        dressCodeTheme +
        theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
              axis.text.y = element_text(size = 8),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(),
              legend.title = element_blank(),
              legend.position = "bottom")

#plot1

rm(focus_subject_label)

                                   
## ---- gender_distribution_computing --------

convertToPlotly(plot1, renameTraces = TRUE, regexpr = genderRegex, height = 580, width = 500, maxMargin = 1,
                yaxis = list(tickfont = list(size = 9))) %>%
    layout(xaxis = list(title = list(font = list(size = 14)), showline = TRUE, linecolor = "rgb(175, 175, 175)",
                        zeroline = TRUE),
          legend = list(y = -0.12, orientation = 'h', yanchor = "bottom", title = list(text = "")),
          margin = list(l = 5)) %>%
    config(displayModeBar = FALSE)


## ---- summary_tables_gender_distribution_computing --------
       
computing_uptake %>%

    group_by(gender) %>%
    summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate(across(PercentageOfStudents, ~ (round(., 3) * 100))) %>%
    select(gender, PercentageOfStudents) %>%
    rename_with(~ c(" ", "% of Computing Entries")) %>%

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
        guides(size = "none") +
        dressCodeTheme +
        theme(legend.title = element_blank(), legend.text = element_text(size = 9),
              axis.text.x = element_text(size = 11, angle = 0, vjust = 0),
              axis.text.y = element_markdown(size = 7), 
              axis.title = element_text(size = 14)
             ) +
        scale_colour_hue(c = 45, l = 45) %>%
    
        suppressMessages()
                          

## ---- top_5_subjects_over_time --------

qualifications <- levels(top_5_subjects$qualification)
years <- levels(top_5_subjects$year)

steps <- list()

plot_tmp <- plot_ly(height = 980, width = 1200)

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
                                marker = ~ list(#symbol = "circle", 
                                                sizemode = "diameter", opacity = 0.65, 
                                                line = list(width = 2, color = gender_colour_scheme[gender]),
                                                size = popularityOverTime * 35), 
                                sizes = c(10, 15),
                                #symbols = c("circle", "x"), #c("-0x2640L", "-0x2642L"), # \u2640 and \u2642 
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
           xaxis = list(range = c(0, 1.05), tickformat = ".0%", tickfont = tickFont, title = list(text = "Popularity over Time")),
           yaxis = list(tickfont = list(size = 8), title = list(text = ""), #font = list(size = 20)),
                        autotick = FALSE, type = "category", categoryarray = levels(top_5_subjects$Subject.label), categoryorder = "array"),
           list(itemsizing = "constant", itemwidth = 30), # being ignored ...
           margin = list(l = 5, b = 2, pad = 5),
           sliders = list(list(active = 0,
                               currentvalue = list(prefix = "Year: "),
                               font = list(color = "grey"),
                               steps = steps,
                               pad = list(t = 35)))
    ) #%>%

#    partial_bundle(local = FALSE) %>%
#    #mapToPlotlyBasicCDN() %>%
#    toWebGL()

rm(data_trace)
                                  
                                   
## ---- computing_uptake_over_time --------
                           
plot_tmp <- plot_ly(height = 330, width = 1200,
                    data = filter_focus_subject_group_summaries %>%
                                #filter(str_detect(SubjectGroup, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE))))
                                filter(SubjectGroup == "Computing"))


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
                     texttemplate = "%{text:.0%}",
                     
                     type = "scatter",
                     mode = "text", 
                     text = ~ round(get(gender_column_labels[1]) / AllEntries, 2),
                     textposition = "top right",
                     textfont = list(color = "gray75", size = 8),

                     name = "gender uptake", #paste0("gender_uptake_", gender_column_labels[1]),
                     showlegend = TRUE,
                     legendgroup = gender_column_labels[1]
                    )
   
plot_tmp <- add_text(plot_tmp, 
                     
                     x = ~ year, 
                     y = ~ get(gender_column_labels[2]),
                     texttemplate = "%{text:.0%}",
                     
                     type = "scatter",
                     mode = "text", 
                     text = ~ round(get(gender_column_labels[2]) / AllEntries, 2),
                     textposition = "top right",
                     textfont = list(color = "gray75", size = 8),

                     name = "gender uptake", #paste0("gender_uptake_", gender_column_labels[2]),
                     showlegend = FALSE,
                     legendgroup = gender_column_labels[1]
                    )

plot_tmp <- add_text(plot_tmp, 
                     
                     x = ~ year, 
                     y = ~ (get(gender_column_labels[1]) + get(gender_column_labels[2])) / 2, 
                     hovertemplate = ~ paste(tooltip2, "<extra></extra>"), #hoverinfo doesn't work in text mode ...
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
    layout(#legend = list(itemclick = FALSE, itemdoubleclick = FALSE), #orientation = 'h', yanchor = "bottom", y = -2.5), 
           xaxis = list(tickfont = list(size = 10), tickangle = -45, title = list(text = ""), standoff = -5), #showgrid = FALSE),
           yaxis = list(tickfont = tickFont, showgrid = TRUE, showline = TRUE, 
                        title = list(text = paste("No. of Students - ", str_to_title("Computing Subjects")), font = list(size = 16))),
           margin = list(b = 5) #, t = -25)
          ) # end layout
                           
                                  
                                   
## ---- computing_uptake_over_time_by_qualification --------

focus_subject <- default_focus_subject

qualifications <- levels(filter_focus_subject_groups$qualification)
genders <- gender_column_labels #levels(filter_focus_subject_groups$gender)
additional_traces <- 4

steps <- list()
plot_tmp <- plot_ly(height = 380, width = 1200)

for (i in seq_along(qualifications)) {
    
    plot_tmp <- add_trace(plot_tmp,
                          data = filter_focus_subject_groups %>%
                                  #filter(str_detect(SubjectGroup, regex(paste0(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
                                  filter(SubjectGroup == "Computing") %>%
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
                         texttemplate = "%{text:.0%}",
                         visible = (i == 1),                            

                         type = "scatter",
                         mode = "text", 
                         text = ~ round(get(gender_column_labels[1]) / AllEntries, 2),
                         textposition = "top right",
                         textfont = list(color = "gray75", size = 8),

                         name = "gender uptake", #paste0("gender_uptake_", gender_column_labels[1]),
                         showlegend = TRUE,
                         legendgroup = gender_column_labels[1]
                        )

    plot_tmp <- add_text(plot_tmp, 

                         x = ~ year, 
                         y = ~ get(gender_column_labels[2]),
                         texttemplate = "%{text:.0%}",
                         visible = (i == 1),                            

                         type = "scatter",
                         mode = "text", 
                         text = ~ round(get(gender_column_labels[2]) / AllEntries, 2),
                         textposition = "top right",
                         textfont = list(color = "gray75", size = 8),

                         name = "gender uptake", #paste0("gender_uptake_", gender_column_labels[2]),
                         showlegend = FALSE,
                         legendgroup = gender_column_labels[1]
                        )

    plot_tmp <- add_text(plot_tmp, 

                         x = ~ year, 
                         y = ~ (get(gender_column_labels[1]) + get(gender_column_labels[2])) / 2, 
                         hovertemplate = ~ paste(tooltip2, "<extra></extra>"), #hoverinfo doesn't work in text mode ...
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
                        title = list(text = "No. of Students - Computing", font = list(size = 16)), rangemode = "tozero"),
           #legend = list(itemclick = FALSE, itemdoubleclick = FALSE),
           margin = list(l = 5),
#           sliders = list(list(active = 0, pad = list(t = 35),
#                               currentvalue = list(prefix = "Qualification: "),
#                               font = list(color = "grey"),
#                               steps = steps)),
           updatemenus = list(list(#active = (which(str_detect(providers, fixed(label_all_bicycle_providers, TRUE))) - 1),
                                   x = 0.2, y = 1.15, #direction = "up",
                                   buttons = steps
                                )) # end dropdown
          ) # end layout
         
                           
                           
## ---- computing_uptake_over_time_cf_english_maths --------

focus_subject <- c("computing", "english", "maths")

plot1 <- filter_focus_subject_group_summaries %>%
            #filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
            filter(SubjectGroup %in% snakecase::to_upper_camel_case(focus_subject)) %>%
                           
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

focus_subject <- c("computing", "english", "maths")

plot1 <- filter_focus_subject_group_summaries %>%
            #filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
            filter(SubjectGroup %in% snakecase::to_upper_camel_case(focus_subject)) %>%
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
focus_subject <- c("computing", "english", "maths")

for (i in seq_along(qualifications)) {


    plots_cf_english_maths[[i]] <- filter_focus_subject_groups %>%
        #filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
        filter(SubjectGroup %in% snakecase::to_upper_camel_case(focus_subject)) %>%
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

            plotly_build() #%>%
#            partial_bundle(local = FALSE) %>%
#            #mapToPlotlyBasicCDN() %>%
#            toWebGL()


    female_legend <- FALSE
    male_legend <- FALSE

    for (j in seq_along(plots_cf_english_maths[[i]]$x$data)) {

        if (!female_legend & (plots_cf_english_maths[[i]]$x$data[[j]]$name == gender_column_labels[1])) {

            plots_cf_english_maths[[i]]$x$data[[j]]$showlegend <- TRUE
            female_legend <- !female_legend

        } else if (!male_legend & (plots_cf_english_maths[[i]]$x$data[[j]]$name == gender_column_labels[2])) {

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
            #filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
            filter(SubjectGroup %in% snakecase::to_upper_camel_case(focus_subject)) %>%
                           
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
            #filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
            filter(SubjectGroup %in% snakecase::to_upper_camel_case(focus_subject)) %>%
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

focus_subject <- select_focus_subjects %>%

                        filter(SubjectGroup == "Sciences") %>%
                        distinct(CommonSubjectLabel) %>%
                        deframe() %>%
                        as.character()
focus_subject <- c("computing", focus_subject)


plots_cf_sciences <- list()
qualifications <- sqa_qualifications_ordering$qualification


for (i in seq_along(qualifications)) {


    plots_cf_sciences[[i]] <- filter_focus_subjects %>%
        #filter(str_detect(SubjectGroup, regex(paste(focus_subject, collapse = "|"), ignore_case = TRUE))) %>%
        filter(SubjectGroup %in% snakecase::to_upper_camel_case(focus_subject)) %>%
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

            plotly_build()  #%>%
#            partial_bundle(local = FALSE) %>%
#            #mapToPlotlyBasicCDN() %>%
#            toWebGL()


    female_legend <- FALSE
    male_legend <- FALSE

    for (j in seq_along(plots_cf_sciences[[i]]$x$data)) {

        if (!female_legend & (plots_cf_sciences[[i]]$x$data[[j]]$name == gender_column_labels[1])) {

            plots_cf_sciences[[i]]$x$data[[j]]$showlegend <- TRUE
            female_legend <- !female_legend

        } else if (!male_legend & (plots_cf_sciences[[i]]$x$data[[j]]$name == gender_column_labels[2])) {

            plots_cf_sciences[[i]]$x$data[[j]]$showlegend <- TRUE
            male_legend <- !male_legend
        }

        if (female_legend & male_legend)
            break;
    }
}

rm(female_legend, male_legend)

                           
        
## ---- teacher_demographics_head_count_teacher_age --------                                           
                           
distribution_teacher_age %>%

    ggplot(aes(Age, HeadCount, colour = fct_rev(Year), group = Year)) + #, shape = Year)) +
        #geom_point(size = 1.15) +
        geom_line() + 
        labs(y = "Teacher HeadCount",
             caption = "Note: Data on teacher age over 70 is only provided from 2018.",
             alt = "Change in Teacher Age over Time, Publicly Funded Schools in Scotland"
            ) +
        dressCodeTheme +
        #scale_colour_manual(values = colorRampPalette(brewer.pal(7, "Dark2"))(length(levels(distribution_teacher_age$Year)))) +        
        scale_shape_manual(values = seq(6, 14)) +


        theme(legend.title = element_blank(), 
              legend.margin = margin(0),
              axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
              plot.caption = element_text(size = 12),              
              ) 
                           
        
                           
## ---- teacher_demographics_head_count_teacher_age_statistics --------                                           

distribution_teacher_age  %>%

    mutate(across(Age, ~ as.integer(as.character(.))),
           MinimumAge = min(Age, na.rm = TRUE),
           MaximumAge = max(Age, na.rm = TRUE),
          ) %>%
    distinct(Year, across(matches("\\w+Age$")), PercentOver55) %>%
    pivot_longer(matches("Age$"), names_to = "Metric", values_to = "Age") %>%
    mutate(across(Metric, ~ gsub("Age", "", .)),
           across(Metric, ~ fct_inorder(.)) # or updated ggplot messes up legend
           ) %>%

    ggplot(aes(Year, Age, colour = Metric, group = Metric)) +
        geom_line(aes(lty = Metric), size = 1.05) +
        ylab("Age") +
        xlab("") +
        dressCodeTheme +
        scale_linetype_manual("Teacher Age",
                                values = c("Average" = 4, "Median" = 2, "Minimum" = 3, "Maximum" = 3),
                                labels = c("Average", "Median", "Minimum", "Maximum")) +
        scale_colour_manual("Teacher Age",
                                values = c("Average" = as.character(color("high contrast")(3)[3]), #["red"]),
                                           "Median" = as.character(color("high contrast")(3)[1]), #["blue"]),
                                           "Minimum" = as.character(color("vibrant")(7)[3]), #["cyan"]),
                                           "Maximum" = as.character(color("vibrant")(7)[3])), #["cyan"])),
                                labels = c("Average", "Median", "Minimum", "Maximum")) +


        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              legend.text = element_text(size = 18),
              axis.text.y = element_text(size = 16),
              )
                          
                           
## ---- teacher_demographics_head_count_teacher_age_central_values --------                                           

distribution_teacher_age  %>%

    distinct(Year, across(matches("\\w+Age$")), PercentOver55) %>%
    pivot_longer(matches("Age$"), names_to = "Metric", values_to = "Age") %>%
    mutate(across(Metric, ~ gsub("Age", "", .)),
           across(Metric, ~ fct_inorder(.)) # or updated ggplot messes up legend
           ) %>%

    ggplot(aes(Year, Age, colour = Metric, group = Metric)) +
        geom_line(aes(lty = Metric), size = 1.05) +
        ylab("Age") +
        xlab("") +
        dressCodeTheme +
        scale_shape_manual(values = seq(6, 14)) +
        scale_linetype_manual("Teacher Age",
                                values = c("Average" = 4, "Median" = 2),
                                labels = c("Average", "Median")) +
        scale_colour_manual("Teacher Age",
                                values = c("Average" = as.character(color("high contrast")(3)[3]), #["red"]),
                                           "Median" = as.character(color("high contrast")(3)[1]) #["blue"])
                                           ),
                                labels = c("Average", "Median")) +


        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              legend.text = element_text(size = 18),
              axis.text.y = element_text(size = 16),
              )


## ---- teacher_demographics_age_over_time_median_fte_all_local_authorities --------

teacher_fte_local_authority_by_age %>%

    filter((AgeRange != "Average") & !(LocalAuthority %in% c("All local authorities", "Scotland"))) %>%
    group_by(AgeRange, Year) %>%
    summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                    )) %>%
    ungroup() %>%

    ggplot(aes(Year, TeacherFTE_median, colour = AgeRange, group = AgeRange, shape = AgeRange)) +
        geom_point(size = 0.95) +
        geom_line() +
        ylab("Median FTE - All Local Authorities") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("Age Range", palette = "Dark2", direction =  -1) +
        scale_shape_manual("Age Range", values = seq(6, 14)) +


        theme(#legend.position = "bottom",
              #legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              )


## ---- teacher_demographics_age_over_time_total_fte_all_local_authorities--------

teacher_fte_local_authority_by_age %>%

    filter((AgeRange != "Average") & (LocalAuthority == "All local authorities")) %>%

    ggplot(aes(Year, TeacherFTE, colour = AgeRange, group = AgeRange, shape = AgeRange)) +
        geom_point(size = 1.15) +
        geom_line() +
        ylab("Total FTE - All Local Authorities") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("Age Range", palette = "Dark2", direction =  -1) +
        scale_shape_manual("Age Range", values = seq(6, 14)) +


        theme(#legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              )
                           

## ---- teacher_demographics_age_median_fte_all_local_authorities --------

teacher_fte_local_authority_by_age %>%
    filter((AgeRange != "Average") & !(LocalAuthority %in% c("All local authorities", "Scotland"))) %>%
    group_by(Year, AgeRange) %>%
    summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                    )) %>%
    ungroup() %>%

    ggplot(aes(AgeRange, TeacherFTE_median, colour = Year, group = Year)) +
        #geom_point(size = 0.5, show.legend = FALSE) +
        geom_line() +
        ylab("Median FTE - All Local Authorities") + xlab("Age Range") +
        dressCodeTheme +
        #scale_colour_romaO(discrete = TRUE) +

        theme(axis.text.x = element_text(angle = 0, vjust = 0),
              legend.title = element_blank(),
              legend.margin = margin(0)
              )



## ---- teacher_demographics_age_fte_all_local_authorities --------

teacher_fte_local_authority_by_age %>%
    filter((AgeRange != "Average") & (LocalAuthority == "All local authorities")) %>%

    ggplot(aes(AgeRange, TeacherFTE, colour = Year, group = Year)) +
        #geom_point(size = 0.5, show.legend = FALSE) +
        geom_line() +
        ylab("Total FTE - All Local Authorities") + xlab("Age Range") +
        dressCodeTheme +
        #scale_colour_romaO(discrete = TRUE) +

        theme(axis.text.x = element_text(angle = 0, vjust = 0),
              legend.title = element_blank(),
              legend.margin = margin(0)
              )


                           
## ---- teacher_demographics_age_fte_computing_cf_english_maths_sciences_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(AgeRange) %>%
    summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                    )) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(AgeRange != "Average") %>%
                filter((SubjectGroup %in% c("Computing", "Sciences", "ModernLanguages", "English", "Maths")) &
                       !(Subject %in% c("General Science", "Science (General)", "Community Languages", "Other Modern Languages"))) %>%

                group_by(AgeRange, SubjectGroup) %>%
                summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                                ))
             ) %>%
    mutate(across(SubjectGroup, ~ fct_recode(., "Sciences (excl. General)" = "Sciences")),
           across(SubjectGroup, ~ fct_na_value_to_level(., level = "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "ModernLanguages", "English", "Maths", after = Inf)),
          ) %>%

    ggplot(aes(AgeRange, TeacherFTE_median, colour = SubjectGroup, group = SubjectGroup, shape = SubjectGroup)) +
        geom_point(size = 0.95) +
        geom_line() +
        ggtitle("Computing cf. English, Maths, Sciences & Modern Languages") +
        ylab("Median FTE") + xlab("") +
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0)
              )



## ---- teacher_demographics_age_fte_computing_cf_sciences_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(AgeRange) %>%
    summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                    )) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(AgeRange != "Average") %>%
                filter((SubjectGroup %in% c("Computing", "Sciences", "ModernLanguages")) &
                       !(Subject %in% c("General Science", "Science (General)", "Community Languages", "Other Modern Languages"))) %>%
                group_by(AgeRange, SubjectGroup) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(SubjectGroup, ~ fct_recode(., "Sciences (excl. General)" = "Sciences")),
           across(SubjectGroup, ~ fct_na_value_to_level(., level = "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "ModernLanguages", after = Inf)),
          ) %>%

    ggplot(aes(AgeRange, TeacherFTE_median, colour = SubjectGroup, group = SubjectGroup, shape = SubjectGroup)) +
        geom_point(size = 0.95) +
        geom_line() +
        ggtitle("Computing cf. Sciences & Modern Languages") +
        ylab("Median FTE") + xlab("") +
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0)
              )



## ---- teacher_demographics_age_fte_computing_cf_all_sciences_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(AgeRange) %>%
    summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                    )) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(AgeRange != "Average") %>%
                filter(SubjectGroup %in% c("Computing", "Sciences", "ModernLanguages")) %>%
                group_by(AgeRange, SubjectGroup) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(SubjectGroup, ~ fct_na_value_to_level(., level = "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "ModernLanguages", after = Inf)),
           across(SubjectGroup, ~ fct_recode(., "All Sciences" = "Sciences")),
           across(SubjectGroup, ~ fct_recode(., "Modern & Community Languages" = "ModernLanguages")),
          ) %>%

    ggplot(aes(AgeRange, TeacherFTE_median, colour = SubjectGroup, group = SubjectGroup, shape = SubjectGroup)) +
        geom_point(size = 0.95) +
        geom_line() +
        ggtitle("Computing cf. Sciences, Modern & Community Languages") +
        ylab("Median FTE") + xlab("") +
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0)
              )



## ---- teacher_demographics_age_fte_computing_cf_english_maths_all_sciences_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(AgeRange) %>%
    summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                    )) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(AgeRange != "Average") %>%
                filter(SubjectGroup %in% c("Computing", "Sciences", "ModernLanguages", "English", "Maths")) %>%

                group_by(AgeRange, SubjectGroup) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(SubjectGroup, ~ fct_na_value_to_level(., level = "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "ModernLanguages", "English", "Maths", after = Inf)),
           across(SubjectGroup, ~ fct_recode(., "All Sciences" = "Sciences")),
           across(SubjectGroup, ~ fct_recode(., "Modern & Community Languages" = "ModernLanguages")),
          ) %>%

    ggplot(aes(AgeRange, TeacherFTE_median, colour = SubjectGroup, group = SubjectGroup, shape = SubjectGroup)) +
        geom_point(size = 0.95) +
        geom_line() +
        ggtitle("Computing cf. English, Maths, Sciences, Modern & Community Languages") +
        ylab("Median FTE") + xlab("") +
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0)
              )



## ---- teacher_demographics_age_fte_computing_cf_sciences --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(AgeRange) %>%
    summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                    )) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(AgeRange != "Average") %>%
                filter((SubjectGroup  == "Computing") |
                       (Subject %in% c("Biology", "Chemistry", "Physics"))) %>%
                group_by(AgeRange, Subject) %>%
                summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                                  average = ~ mean(., na.rm = TRUE))
                                ))
    ) %>%
    mutate(across(Subject, ~ fct_na_value_to_level(., level = "All Subjects")),
           across(Subject, ~ fct_relevel(., "All Subjects")),
          ) %>%
    arrange(AgeRange, Subject) %>%

    ggplot(aes(AgeRange, TeacherFTE_median, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 0.95) +
        geom_line() +
        ggtitle("Median Teacher FTE - Computing cf. Sciences") +
        ylab("Median FTE") + xlab("") +
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0)
              )


## ---- teacher_demographics_age_fte_computing_cf_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(AgeRange) %>%
    summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                      average = ~ mean(., na.rm = TRUE))
                    )) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                mutate(across(CommonSubjectLabel, as.character),
                       across(CommonSubjectLabel, ~ case_when(str_detect(Subject, regex("gaidhlig", ignore_case = TRUE)) ~ "Gaelic",
                                                          TRUE ~ .
                                                         )),
                       across(CommonSubjectLabel, as.factor)
                      ) %>%

                filter(AgeRange != "Average") %>%
                filter((SubjectGroup  == "Computing") |
                       (CommonSubjectLabel %in% c("Gaelic", "French", "German", "Spanish", "Italian"))) %>%
                group_by(AgeRange, Subject) %>%
                summarise(across(TeacherFTE, list(median = ~ median(., na.rm = TRUE),
                                                  average = ~ mean(., na.rm = TRUE))
                                ))
    ) %>%
    mutate(across(Subject, ~ fct_na_value_to_level(., level = "All Subjects")),
           across(Subject, ~ fct_relevel(., "All Subjects")),
          ) %>%
    arrange(AgeRange, Subject) %>%

    ggplot(aes(AgeRange, TeacherFTE_median, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 0.95) +
        geom_line() +
        ggtitle("Median Teacher FTE - Computing cf. Modern Languages") +
        ylab("Median FTE") + xlab("") +
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        #scale_colour_manual(values = colorRampPalette(brewer.pal(7, "Dark2"))(7)) +
        scale_shape_manual(values = 0:6) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0)
              )

              
## ---- teacher_demographics_age_over_time_computing_vs_sciences --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(Year, AgeRange) %>%
    summarise(across(TeacherFTE, median, na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(AgeRange != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Biology", "Chemistry", "Physics"))) %>%
                select(Year, AgeRange, Subject, TeacherFTE)
    ) %>%

    mutate(across(Subject, ~ fct_na_value_to_level(., level = "Median - All Subjects")),
           across(Subject, ~ fct_relevel(., "Median - All Subjects")),
          ) %>%
     
    ggplot(aes(AgeRange, TeacherFTE, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 0.95) +
        geom_line() + 
        ggtitle(paste("Teacher FTE Change",
                      paste0(teacher_fte_main_subject_by_age_start_date, "-",
                             teacher_fte_main_subject_by_age_end_date
                            ),
                      "- Computing cf. Sciences")) +
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5),
              ) +
        facet_wrap(~ Year, nrow = 2)


                           
## ---- teacher_demographics_age_over_time_computing_vs_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(Year, AgeRange) %>%
    summarise(across(TeacherFTE, median, na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
               mutate(across(CommonSubjectLabel, as.character),
                       across(CommonSubjectLabel, ~ case_when(str_detect(Subject, regex("gaidhlig", ignore_case = TRUE)) ~ "Gaelic",
                                                          TRUE ~ .
                                                         )),
                       across(CommonSubjectLabel, as.factor)
                      ) %>%
                
                filter(AgeRange != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Gaelic", "French", "German", "Spanish", "Italian"))) %>%
                select(Year, AgeRange, Subject, TeacherFTE)
    ) %>%

    mutate(across(Subject, ~ fct_na_value_to_level(., level = "Median - All Subjects")),
           across(Subject, ~ fct_relevel(., "Median - All Subjects")),
          ) %>%
    
    ggplot(aes(AgeRange, TeacherFTE, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 0.95) +
        geom_line() + 
        ggtitle(paste("Teacher FTE Change",
                      paste0(teacher_fte_main_subject_by_age_start_date, "-",
                             teacher_fte_main_subject_by_age_end_date
                            ),
                      "- Computing cf. Modern Languages")) +
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = 0:6) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5),
              ) +
        facet_wrap(~ Year, nrow = 2)


                           
## ---- teacher_demographics_age_change_over_time_computing_vs_sciences --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(AgeRange, Year) %>%
    summarise(across(TeacherFTE, median, na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(AgeRange != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Biology", "Chemistry", "Physics"))) %>%
                select(Year, AgeRange, Subject, TeacherFTE)
    ) %>%

    mutate(across(Subject, ~ fct_na_value_to_level(., level = "Median - All Subjects")),
           across(Subject, ~ fct_relevel(., "Median - All Subjects")),
          ) %>%
    #arrange(Year, AgeRange, Subject) %>%
    
    ggplot(aes(Year, TeacherFTE, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 0.95) +
        geom_line() + 
        ggtitle(paste("Teacher FTE Change",
                      paste0(teacher_fte_main_subject_by_age_start_date, "-",
                             teacher_fte_main_subject_by_age_end_date
                            ),
                      "- Computing cf. Sciences")) +
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0),
              #plot.title = element_text(size = 16),
              axis.text.y = element_text(size = 14), # element_markdown
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5),
              ) +
        facet_wrap(~ AgeRange, nrow = 2)


                          
## ---- teacher_demographics_age_change_over_time_computing_vs_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(AgeRange != "Average") %>%
    group_by(AgeRange, Year) %>%
    summarise(across(TeacherFTE, median, na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
               mutate(across(CommonSubjectLabel, as.character),
                       across(CommonSubjectLabel, ~ case_when(str_detect(Subject, regex("gaidhlig", ignore_case = TRUE)) ~ "Gaelic",
                                                          TRUE ~ .
                                                         )),
                       across(CommonSubjectLabel, as.factor)
                      ) %>%
                
                filter(AgeRange != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Gaelic", "French", "German", "Spanish", "Italian"))) %>%
                select(Year, AgeRange, Subject, TeacherFTE)
    ) %>%

    mutate(across(Subject, ~ fct_na_value_to_level(., level = "Median - All Subjects")),
           across(Subject, ~ fct_relevel(., "Median - All Subjects")),
          ) %>%
    #arrange(Year, AgeRange, Subject) %>%
    
    ggplot(aes(Year, TeacherFTE, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 0.95) +
        geom_line() + 
        ggtitle(paste("Teacher FTE Change",
                      paste0(teacher_fte_main_subject_by_age_start_date, "-",
                             teacher_fte_main_subject_by_age_end_date
                            ),
                      "- Computing cf. Modern Languages")) +
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer(palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = 0:6) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5),
              ) +
        facet_wrap(~ AgeRange, nrow = 2)



## ---- teacher_demographics_gender_distribution_fte_overall --------

plot1 <- teacher_fte_main_subject_by_gender %>%
    filter(Gender != "all") %>%

    group_by(Year) %>%
    mutate(total_year = sum(TeacherFTE, na.rm = TRUE)) %>%

    ungroup() %>%
    mutate(PercentageFTE = (total_all_subjects / total_year),
           tooltip = paste0(Gender, " - ", round(PercentageFTE, 2) * 100, "% in ", Year),
           ) %>%

    ggplot(aes(y = Year, x = if_else(Gender == "male", PercentageFTE, - PercentageFTE),
               fill = Gender, colour = Gender, text = tooltip)) +
        geom_segment(aes(xend = 0, yend = Year), size = 1.5) + #alpha = 0.35) +
        geom_point(aes(colour = Gender), size = 0.75) + #, alpha = 0.65) +
        xlab("\nGender Distribution Overall") +
        ylab(NULL) +
        scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) +
        #scale_shape_manual(values = c(6, 2)) + #-0x2640L, -0x2642L)) + # \u2640 and \u2642
        scale_colour_manual(values = gender_colour_scheme) +
        scale_fill_manual(values = gender_colour_scheme) +
        dressCodeTheme +
        theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
              axis.text.y = element_text(size = 8),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(),
              legend.title = element_blank(),
              legend.position = "bottom")

#plot1

convertToPlotly(plot1, renameTraces = TRUE, regexpr = genderRegex, height = 280, width = 460, maxMargin = 1,
                yaxis = list(tickfont = list(size = 9))
               ) %>%
    layout(xaxis = list(title = list(font = list(size = 14)), showline = TRUE,
                        linecolor = "rgb(175, 175, 175)", zeroline = TRUE),
           legend = list(y = -0.28, orientation = 'h', yanchor = "bottom", title = list(text = "")),
           margin = list(l = 5)) %>%
    config(displayModeBar = FALSE)


## ---- teacher_demographics_gender_distribution_fte_computing --------

plot1 <- teacher_fte_main_subject_by_gender %>%
    filter((Gender != "all") & (SubjectGroup == "Computing")) %>%

    group_by(Year) %>%
    mutate(PercentageFTE = (TeacherFTE_Main / sum(TeacherFTE_Main, na.rm = TRUE)),
           tooltip = paste0(Gender, " - ", round(PercentageFTE, 2) * 100, "% in ", Year),
          ) %>%
    ungroup() %>%

    ggplot(aes(y = Year, x = if_else(Gender == "male", PercentageFTE, - PercentageFTE),
               fill = Gender, colour = Gender, text = tooltip)) +
        geom_segment(aes(xend = 0, yend = Year), size = 1.5) + #alpha = 0.35) +
        geom_point(aes(colour = Gender), size = 0.75) + #, alpha = 0.65) +
        xlab("\nGender Distribution - Computing as Main Subject") +
        ylab(NULL) +
        scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) +
        #scale_shape_manual(values = c(6, 2)) + #-0x2640L, -0x2642L)) + # \u2640 and \u2642
        scale_colour_manual(values = gender_colour_scheme) +
        scale_fill_manual(values = gender_colour_scheme) +
        dressCodeTheme +
        theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
              axis.text.y = element_text(size = 8),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(),
              legend.title = element_blank(),
              legend.position = "bottom")

convertToPlotly(plot1, renameTraces = TRUE, regexpr = genderRegex, height = 280, width = 460, maxMargin = 1,
                yaxis = list(tickfont = list(size = 9))
               ) %>%
    layout(xaxis = list(title = list(font = list(size = 14)), showline = TRUE,
                        linecolor = "rgb(175, 175, 175)", zeroline = TRUE),
           legend = list(y = -0.28, orientation = 'h', yanchor = "bottom", title = list(text = "")),
           margin = list(l = 5)) %>%
    config(displayModeBar = FALSE)


## ---- summary_cs_teacher_fte_main_subject_by_gender --------

summary_cs_teacher_fte_main_subject_by_gender <- teacher_fte_main_subject_by_gender %>%
    filter((Gender != "all") &
           str_detect(SubjectGroup, regex(paste0(default_focus_subject, collapse = "|"), ignore_case = TRUE))) %>%

    group_by(Gender) %>%
    mutate(across(matches("TeacherFTE"), list(median = ~ (median(., na.rm = TRUE)),
                                              mean = ~ (mean(., na.rm = TRUE))),
                  .names = "{.col}_{.fn}_by_gender")
          ) %>%
    ungroup()


## ---- teacher_fte_main_subject_by_gender_cs_total_zoomed_in --------

summary_cs_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE, colour = Gender, group = Gender, shape = Gender)) +
        geom_point(size = 1.15) +
        geom_line() +
        geom_hline(aes(yintercept = TeacherFTE_median_by_gender, colour = Gender), linetype = "dashed") +
        annotate("label",
                 x = nth(levels(summary_cs_teacher_fte_main_subject_by_gender$Year), 2),
                 y = unique(summary_cs_teacher_fte_main_subject_by_gender$TeacherFTE_median_by_gender),
                 label = "median FTE",
                 colour = gender_colour_scheme,
                 label.size = NA # remove border
                ) +
        #ggtitle("\n\nTotal FTE, Computing, By Gender") +
        ylab("Teacher FTE") +
        xlab("") +
        dressCodeTheme +
        scale_colour_manual(values = gender_colour_scheme) +
        scale_shape_manual(values = seq(6, 14)) +

        theme(legend.position = c(0.95, 0.95), #"bottom",
              legend.justification = c(1, 1),
              legend.title = element_blank(),
              legend.margin = margin(0, 5, 2, 5),
              legend.background = element_rect(linewidth = 0.5, colour = "grey"),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              )


## ---- teacher_fte_main_subject_by_gender_cs_total --------

summary_cs_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE, colour = Gender, group = Gender, shape = Gender)) +
        geom_point(size = 1.15) +
        geom_line() +
        geom_hline(aes(yintercept = TeacherFTE_median_by_gender, colour = Gender), linetype = "dashed") +
        #annotate("text", x = "2010", y = TeacherFTE_median_by_gender, label = "median") +
        #ggtitle("\n\nTotal FTE, Computing, By Gender") +
        ylab("Teacher FTE") +
        xlab("") +
        dressCodeTheme +
        scale_colour_manual(values = gender_colour_scheme) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +

        theme(legend.position = c(0.95, 0.05), #"bottom",
              legend.justification = c(1, 0),
              legend.title = element_blank(),
              legend.margin = margin(0, 5, 2, 5),
              legend.background = element_rect(linewidth = 0.5, colour = "grey"),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              )


## ---- teacher_fte_main_subject_by_gender_cs_as_main_zoomed_in --------

summary_cs_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Main, colour = Gender, group = Gender, shape = Gender)) +
        geom_point(size = 1.15) +
        geom_line() +
        geom_hline(aes(yintercept = TeacherFTE_Main_median_by_gender, colour = Gender), linetype = "dashed") +
        annotate("label",
                 x = nth(levels(summary_cs_teacher_fte_main_subject_by_gender$Year), 2),
                 y = unique(summary_cs_teacher_fte_main_subject_by_gender$TeacherFTE_Main_median_by_gender),
                 label = "median FTE",
                 colour = gender_colour_scheme,
                 label.size = NA # remove border
                ) +
        #ggtitle("\n\nTotal FTE, Computing as Main Subject, By Gender") +
        ylab("Teacher FTE - Main Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_manual(values = gender_colour_scheme) +
        scale_shape_manual(values = seq(6, 14)) +

        theme(legend.position = c(0.95, 0.95), #"bottom",
              legend.justification = c(1, 1),
              legend.title = element_blank(),
              legend.margin = margin(0, 5, 2, 5),
              legend.background = element_rect(linewidth = 0.5, colour = "grey"),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              )


## ---- teacher_fte_main_subject_by_gender_cs_as_main --------

summary_cs_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Main, colour = Gender, group = Gender, shape = Gender)) +
        geom_point(size = 1.15) +
        geom_line() +
        geom_hline(aes(yintercept = TeacherFTE_Main_median_by_gender, colour = Gender), linetype = "dashed") +
        #annotate("text", x = "2010", y = TeacherFTE_Main_median_by_gender, label = "median") +
        #ggtitle("\n\nTotal FTE, Computing as Main Subject, By Gender") +
        ylab("Teacher FTE - Main Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_manual(values = gender_colour_scheme) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +

        theme(legend.position = c(0.95, 0.05), #"bottom",
              legend.justification = c(1, 0),
              legend.title = element_blank(),
              legend.margin = margin(0, 5, 2, 5),
              legend.background = element_rect(linewidth = 0.5, colour = "grey"),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              )


## ---- teacher_fte_main_subject_by_gender_cs_as_other_zoomed_in --------

summary_cs_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Other, colour = Gender, group = Gender, shape = Gender)) +
        geom_point(size = 1.15) +
        geom_line() +
        geom_hline(aes(yintercept = TeacherFTE_Other_median_by_gender, colour = Gender), linetype = "dashed") +
        annotate("label",
                 x = nth(levels(summary_cs_teacher_fte_main_subject_by_gender$Year), 2),
                 y = unique(summary_cs_teacher_fte_main_subject_by_gender$TeacherFTE_Other_median_by_gender),
                 label = "median FTE",
                 colour = gender_colour_scheme,
                 label.size = NA
                ) +
        #ggtitle("\n\nTotal FTE, Computing as Other Subject, By Gender") +
        ylab("Teacher FTE - Other Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_manual(values = gender_colour_scheme) +
        scale_shape_manual(values = seq(6, 14)) +

        theme(legend.position = c(0.95, 0.95), #"bottom",
              legend.justification = c(1, 1),
              legend.title = element_blank(),
              legend.margin = margin(0, 5, 2, 5),
              legend.background = element_rect(linewidth = 0.5, colour = "grey"),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              )


## ---- teacher_fte_main_subject_by_gender_cs_as_other --------

summary_cs_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Other, colour = Gender, group = Gender, shape = Gender)) +
        geom_point(size = 1.15) +
        geom_line() +
        geom_hline(aes(yintercept = TeacherFTE_Other_median_by_gender, colour = Gender), linetype = "dashed") +
        #annotate("text", x = "2010", y = TeacherFTE_Other_median_by_gender, label = "median") +
        #ggtitle("\n\nTotal FTE, Computing as Other Subject, By Gender") +
        ylab("Teacher FTE - Other Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_manual(values = gender_colour_scheme) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +

        theme(legend.position = c(0.95, 0.05), #"bottom",
              legend.justification = c(1, 0),
              legend.title = element_blank(),
              legend.margin = margin(0, 5, 2, 5),
              legend.background = element_rect(linewidth = 0.5, colour = "grey"),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              )


## ---- summary_cs_sciences_teacher_fte_main_subject_by_gender --------

focus_subject_group <- "Sciences"

summary_cs_sciences_teacher_fte_main_subject_by_gender <- teacher_fte_main_subject_by_gender %>%
    filter((Gender != "all") & (SubjectGroup %in% c(focus_subject_group, "Computing"))) %>%
    mutate(across(Subject, ~ fct_relevel(., sort(levels(.)[str_detect(levels(.),
                                                                           regex("general", ignore_case = TRUE))]), after = Inf)),
           across(Subject, ~ fct_drop(.)),
          ) %>%

    group_by(Gender, Subject) %>%
    mutate(across(matches("TeacherFTE"), list(median = ~ (median(., na.rm = TRUE)),
                                              mean = ~ (mean(., na.rm = TRUE))),
                  .names = "{.col}_{.fn}_by_gender")
          ) %>%
    ungroup() %>%
    mutate(across(Subject, ~ fct_drop(.)))


## ---- summary_cs_sciences_teacher_fte_main_subject_by_gender_total --------

summary_cs_sciences_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE, colour = Subject, group = Subject, shape = Subject, linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #geom_hline(aes(yintercept = TeacherFTE_median_by_gender, colour = Subject), linetype = "dashed") +
        #annotate("label",
        #         x = nth(levels(summary_cs_sciences_teacher_fte_main_subject_by_gender$Year), 2),
        #         y = unique(summary_cs_sciences_teacher_fte_main_subject_by_gender$TeacherFTE_median_by_gender),
        #         label = "median FTE",
        #         colour = gender_colour_scheme,
        #         label.size = NA # remove border
        #        ) +
        #ggtitle(paste("\n\nTotal FTE by Gender,", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE") +
        xlab("") +
        dressCodeTheme +
        #scale_colour_manual(values = gender_colour_scheme) +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        #scale_linetype_manual(values = c("twodash",  "dotted")) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_sciences_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              ) +
        facet_grid(cols = vars(Gender))


## ---- summary_cs_sciences_teacher_fte_main_subject_by_gender_as_main --------

summary_cs_sciences_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Main, colour = Subject, group = Subject, shape = Subject,
               linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender, Main Subject - ", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE - Main Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_sciences_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              ) +
        facet_grid(cols = vars(Gender))


## ---- summary_cs_sciences_teacher_fte_main_subject_by_gender_as_other --------

summary_cs_sciences_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Other, colour = Subject, group = Subject, shape = Subject,
               linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender, Other Subject - ", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE - Other Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_sciences_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              ) +
        facet_grid(cols = vars(Gender))


## ---- summary_cs_sciences_teacher_fte_main_subject_by_gender_as_other_excl_generaj_science --------

summary_cs_sciences_teacher_fte_main_subject_by_gender %>%
    filter(!(str_detect(Subject, "General") & str_detect(Subject, "Science"))) %>%
    
    ggplot(aes(Year, TeacherFTE_Other, colour = Subject, group = Subject, shape = Subject,
               linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender, Other Subject - ", focus_subject_group, "(excl. General Science) cf. Computing")) +
        ylab("Teacher FTE - Other Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1
        #scale_colour_manual(values = colorRampPalette(colour("bamako", force = TRUE)(15))(5)) +
        #scale_colour_manual(values = viridis::magma(15)) +
        #scale_colour_manual(values = colour("muted", force = TRUE)(8)) +
        #khroma::scale_colour_romaO(discrete = TRUE) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(subset(summary_cs_sciences_teacher_fte_main_subject_by_gender,
                                                    !(str_detect(Subject, "General") & str_detect(Subject, "Science"))
                                                   )$TeacherFTE_Other, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              #axis.text.x = element_text(size = 14, angle = 0, vjust = 0),
              ) +
        facet_grid(cols = vars(Gender))



## ---- summary_cs_languages_teacher_fte_main_subject_by_gender --------

summary_cs_languages_teacher_fte_main_subject_by_gender <- teacher_fte_main_subject_by_gender %>%
    filter((Gender != "all") &
           ((SubjectGroup == "Computing") |
            ((SubjectGroup == "ModernLanguages") & (CommonSubjectLabel %in% c("French", "German", "Spanish", "Italian")))
            )
          ) %>%

    group_by(Gender, Subject) %>%
    mutate(across(matches("TeacherFTE"), list(median = ~ (median(., na.rm = TRUE)),
                                              mean = ~ (mean(., na.rm = TRUE))),
                  .names = "{.col}_{.fn}_by_gender")
          ) %>%
    ungroup() %>%
    mutate(across(Subject, ~ fct_drop(.)))


## ---- summary_cs_languages_teacher_fte_main_subject_by_gender_total --------

summary_cs_languages_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE, colour = Subject, group = Subject, shape = Subject, linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender,", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        #scale_linetype_manual(values = c("twodash",  "dotted")) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_languages_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14),
              ) +
        facet_grid(cols = vars(Gender))


## ---- summary_cs_languages_teacher_fte_main_subject_by_gender_as_main --------

summary_cs_languages_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Main, colour = Subject, group = Subject, shape = Subject,
               linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender, Main Subject - ", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE- Main Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_languages_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14),
              ) +
        facet_grid(cols = vars(Gender))


## ---- summary_cs_languages_teacher_fte_main_subject_by_gender_as_other --------

summary_cs_languages_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Other, colour = Subject, group = Subject, shape = Subject,
               linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender, Other Subject - ", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE - Other Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_languages_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14),
              ) +
        facet_grid(cols = vars(Gender))



## ---- summary_cs_business_teacher_fte_main_subject_by_gender --------

focus_subject_group <- "Business"

summary_cs_business_teacher_fte_main_subject_by_gender <- teacher_fte_main_subject_by_gender %>%
    filter((Gender != "all") &
           ((SubjectGroup == "Computing") |
            str_detect(Subject, regex(focus_subject_group, ignore_case = TRUE))
           )
          ) %>%

    group_by(Gender, Subject) %>%
    mutate(across(matches("TeacherFTE"), list(median = ~ (median(., na.rm = TRUE)),
                                              mean = ~ (mean(., na.rm = TRUE))),
                  .names = "{.col}_{.fn}_by_gender")
          ) %>%
    ungroup() %>%
    mutate(across(SubjectGroup, ~ fct_recode(., "Business" = "ModernStudies")),
           across(Subject, ~ fct_drop(.)))


## ---- summary_cs_business_teacher_fte_main_subject_by_gender_total --------

summary_cs_business_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE, colour = Subject, group = Subject, shape = Subject, linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender,", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_business_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14),
              ) +
        facet_grid(cols = vars(Gender))


## ---- summary_cs_business_teacher_fte_main_subject_by_gender_as_main --------

summary_cs_business_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Main, colour = Subject, group = Subject, shape = Subject,
               linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender, Main Subject - ", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE- Main Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_business_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14),
              ) +
        facet_grid(cols = vars(Gender))


## ---- summary_cs_business_teacher_fte_main_subject_by_gender_as_other --------

summary_cs_business_teacher_fte_main_subject_by_gender %>%

    ggplot(aes(Year, TeacherFTE_Other, colour = Subject, group = Subject, shape = Subject,
               linetype = Subject)) +
        geom_point(size = 2.15) +
        geom_line() +
        #ggtitle(paste("\n\nTotal FTE by Gender, Other Subject - ", focus_subject_group, "cf. Computing")) +
        ylab("Teacher FTE - Other Subject") +
        xlab("") +
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = seq(6, 14)) +
        scale_y_continuous(limits = c(0, max(summary_cs_business_teacher_fte_main_subject_by_gender$TeacherFTE, na.rm = TRUE))) +
        guides(linetype = "none",
               colour = guide_legend("title"), shape = guide_legend("title") # megre
              ) +

        theme(#legend.position = "bottom",
              legend.title = element_blank(),
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14),
              ) +
        facet_grid(cols = vars(Gender))


## ---- gender_distribution_focus_subjects_teacher_fte --------

facet_labeller <- function(facet_value) {
    paste0(case_when((facet_value == "TeacherFTE") ~ "Total FTE",
                     TRUE ~ str_remove(facet_value, "TeacherFTE") %>%
                                 snakecase::to_title_case()
                    )
           )
}

gender_distribution_focus_subjects_teacher_fte <- teacher_fte_main_subject_by_gender %>%
    select(!matches("Normalised$")) %>%
    filter(((SubjectGroup %in% c("Computing", "Sciences")) &
                !str_detect(Subject, regex("general", ignore_case = TRUE))
            ) |
            if_any(c(Subject, CommonSubjectLabel), ~
                   str_detect(., regex(paste0(c("Business", "French", "German", "Spanish", "Italian"), collapse = "|"),
                                       ignore_case = TRUE)))
          ) %>%
     mutate(across(SubjectGroup, ~ fct_recode(., "Business" = "ModernStudies"))) %>%

    group_by(Year, Gender, CommonSubjectLabel) %>%
    mutate(across(matches("^TeacherFTE"), ~ sum(., na.rm = TRUE), .names = "total_year_by_gender_{.col}")) %>%

    group_by(Year, CommonSubjectLabel) %>%
    mutate(across(matches("^TeacherFTE"), ~ sum(., na.rm = TRUE), .names = "total_year_{.col}")) %>%

    ungroup() %>%
    distinct() %>%
    mutate(across(matches("total_year_by_gender_"), ~ (. / get(str_remove(cur_column(), "by_gender_"))),
                  .names = "PercentageFTE_{.col}"),
           ) %>%
    rename_with(~ str_remove(., "total_year_by_gender_"), matches("PercentageFTE_")) %>%

    pivot_longer(where(is.double), names_to = "Metric", values_to = "TeacherFTE", values_drop_na = TRUE) %>%

    filter(str_detect(Metric, "PercentageFTE")) %>%
    rename(PercentageFTE = TeacherFTE) %>%

    mutate(across(Metric, ~ gsub("PercentageFTE_", "", .)),
           across(Metric, ~ gsub("(Main|Other)", "As\\1Subject", .)),
           across(CommonSubjectLabel, ~ if_else(!is.na(.), ., SubjectGroup)),
           across(CommonSubjectLabel, ~ fct_reorder(., as.integer(SubjectGroup))),
           across(matches("Subject"), ~ fct_drop(.)),
           tooltip = paste0(CommonSubjectLabel, ", ", Gender, " - ", round(PercentageFTE, 2) * 100, "% in ", Year)
          )

## ---- gender_distribution_computing_cf_sciences_teacher_fte --------

plots_cf_sciences <- list()
current_filter <- gender_distribution_focus_subjects_teacher_fte %>%
    filter(SubjectGroup %in% c("Computing", "Sciences")) %>%
    distinct(CommonSubjectLabel) %>%
    deframe() %>%
    as.character()

for (i in seq(current_filter)) {
  
    plots_cf_sciences[[i]] <- gender_distribution_focus_subjects_teacher_fte %>%
        filter(CommonSubjectLabel == current_filter[i]) %>%
        ggplot(aes(y = Year, x = if_else(Gender == "male", PercentageFTE, - PercentageFTE),
                   fill = Gender, colour = Gender, text = tooltip)) +
            geom_segment(aes(xend = 0, yend = Year), linewidth = 1.5) + #alpha = 0.35) +
            geom_point(aes(colour = Gender), size = 0.75) + #, alpha = 0.65) +
            ggtitle(current_filter[i]) + #paste("Gender Distribution -", current_filter[i])) +
            xlab(NULL) + ylab(NULL) +
            scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) +
            #scale_shape_manual(values = c(6, 2)) + #-0x2640L, -0x2642L)) + # \u2640 and \u2642
            scale_colour_manual(values = gender_colour_scheme) +
            scale_fill_manual(values = gender_colour_scheme) +
            dressCodeTheme +
            theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
                  axis.text.y = element_text(size = 7),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_line(),
                  strip.text = element_text(size = 11),
                  legend.title = element_blank(),
                  legend.position = "bottom"
                 ) +
            facet_wrap(~ Metric, ncol = 1, strip.position = "right", labeller = as_labeller(facet_labeller))
            
       
        plots_cf_sciences[[i]] <- convertToPlotly(plots_cf_sciences[[i]],
                    renameTraces = TRUE, regexpr = genderRegex, height = 650, width = 900, maxMargin = 1,
                    yaxis = list(tickfont = list(size = 9))
                   ) %>%
                   layout(xaxis = list(title = list(font = list(size = 14)), showline = TRUE,
                            linecolor = "rgb(175, 175, 175)", zeroline = TRUE),
                       #yaxis = list(dtick = "2", tick0 = min(computing_uptake$year), tickmode = "linear"), #, yaxis.minor.dtick = 1), #tickfont = list(font = list(size = 4))),
                       legend = list(orientation = 'h', yanchor = "bottom", title = list(text = "")), #y = -0.28,
                       margin = list(l = 5)) %>%
                    config(displayModeBar = FALSE)
}


## ----  --------


## ---- summary_tables_gender_distribution_teacher_fte_overall --------

teacher_fte_main_subject_by_gender %>%
    filter(Gender != "all") %>%

    group_by(Gender) %>%
    summarise(total_FTE = sum(TeacherFTE)) %>%

    ungroup() %>%
    summarise(` ` = Gender,
              `Total Teacher FTE` = (total_FTE / sum(total_FTE))
              ) %>%
    mutate(across(where(is.numeric), ~ scales::percent(., accuracy = 0.1, scale = 100))) %>%

    kable(align = "lc") %>% #caption = "\nGender distribution overall") %>%
    kable_paper("striped", full_width = FALSE, position = "left")


## ---- summary_tables_gender_distribution_teacher_fte_computing --------

teacher_fte_main_subject_by_gender %>%
    filter((Gender != "all") & (SubjectGroup == "Computing")) %>%

    group_by(Gender) %>%
        summarise(total_FTE_Main = sum(TeacherFTE_Main),
              total_FTE_Other = sum(TeacherFTE_Other),
              total_FTE = sum(TeacherFTE),
             ) %>%

    ungroup() %>%
    summarise(` ` = Gender,
              `As Main Subject` = (total_FTE_Main / sum(total_FTE_Main)),
              `As Other Subject` = (total_FTE_Other / sum(total_FTE_Other)),
              `Total FTE` = (total_FTE / sum(total_FTE))
              ) %>%
    mutate(across(where(is.numeric), ~ scales::percent(., accuracy = 0.1, scale = 100))) %>%

    kable(align = "lccc") %>% #caption = "\nGender distribution - Computing only") %>%
    kable_paper("striped", full_width = FALSE, position = "left")



## ---- teachers_initial_intake_pgde_secondary_and_alt_routes_overview --------

teachers_initial_intake_pgde_secondary_and_alt_routes %>%
    
    group_by(year) %>%
    summarise(across(c(SFCTarget, Intake), ~ sum(., na.rm = TRUE))) %>%
    pivot_longer(!year, names_to = "category", values_to = "count") %>%
    mutate(across(year, ~ gsub("_", "-", .))) %>%

    ggplot(aes(y = year, x = count)) +
        geom_col(aes(fill = category), width = 0.65, alpha = 0.65, position = "dodge", na.rm = TRUE) +
        scale_fill_brewer("statistics", palette = "Dark2", direction = -1) +
        dressCodeTheme +
        ggtitle("Initial teacher education: PGDE and Other Routes") +
        ylab("") +
        xlab("No. of Student Teachers") +
        scale_x_continuous(labels = scales::comma) +  #label_number_si()) + 
        dressCodeTheme  +
        theme(axis.text.y = element_markdown(),
              axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
			  #axis.text.y = element_text(size = 7),
			  strip.text = element_text(size = 11),
			  legend.title = element_blank()
			  )


## ---- teachers_initial_intake_university_overview --------

teachers_initial_intake_university %>%
    group_by(year) %>%
    rename(SFCTarget = Target) %>%
    summarise(across(c(SFCTarget, Intake), ~ sum(., na.rm = TRUE))) %>%
    pivot_longer(!year, names_to = "category", values_to = "count") %>%
    mutate(across(year, ~ gsub("_", "-", .))) %>%

    ggplot(aes(y = year, x = count)) +
        geom_col(aes(fill = category), width = 0.65, alpha = 0.65, position = "dodge", na.rm = TRUE) +
        scale_fill_brewer("statistics", palette = "Dark2", direction = -1) +
        dressCodeTheme +
        ggtitle("Initial teacher education: University") +
        ylab("") +
        xlab("No. of Student Teachers") +
        scale_x_continuous(labels = scales::comma) +  #label_number_si()) + 
        dressCodeTheme  +
        theme(axis.text.y = element_markdown(),
              axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
			  #axis.text.y = element_text(size = 7),
			  strip.text = element_text(size = 11),
			  legend.title = element_blank()
              )  

## ---- teachers_initial_intake_pgde_secondary_and_alt_routes_by_subject --------

teachers_initial_intake_pgde_secondary_and_alt_routes %>%
    
    pivot_longer(!c(Subject, year), names_to = "category", values_to = "count") %>%
    mutate(across(year, ~ gsub("_", "-", .)),
           across(Subject, ~ fct_reorder(Subject, count, .na_rm = TRUE)),
           across(Subject, ~ fct_relevel(., "Alternative routes")),
          ) %>%

    ggplot(aes(y = Subject, x = count)) +
        geom_segment(aes(xend = 0, yend = Subject)) +
        geom_point(aes(colour = category), size = 3, alpha = 0.65) +
        scale_colour_brewer("statistics", palette = "Dark2", direction = -1) +
        dressCodeTheme +
        ggtitle("Initial teacher education intake by Subject: PGDE and Other Routes") +
        ylab("") +
        xlab("No. of Student Teachers") +
        scale_x_continuous(labels = scales::comma) +  #label_number_si()) + 
        dressCodeTheme  +
        theme(axis.text.y = element_markdown(),
              axis.text.x = element_text(size = 11), 
              legend.title = element_blank(),
              ) +
        facet_grid(cols = vars(year))							   


## ---- teachers_initial_intake_university_by_award --------

teachers_initial_intake_university %>%
    rename(SFCTarget = Target) %>%
    pivot_longer(!c(ITECategory, year, award_type, school_stage), names_to = "category", values_to = "count") %>%

    mutate(across(school_stage, ~ fct_na_level_to_value(.)),
		   across(award_type, ~ fct_relevel(., "New routes", "Alternative routes", after = Inf)),
          ) %>%

    arrange(award_type, school_stage, desc(count)) %>%
    mutate(across(year, ~ gsub("_", "-", .)),
           across(ITECategory, ~ fct_inorder(.))
           ) %>%
    
    ggplot(aes(y = fct_rev(ITECategory), x = count)) +
        geom_segment(aes(xend = 0, yend = ITECategory)) +
        geom_point(aes(colour = category), size = 3, alpha = 0.65) +
        scale_colour_brewer("statistics", palette = "Dark2", direction = -1) +
        dressCodeTheme +
        ggtitle("Initial teacher education intake by award: University") +
        ylab("") +
        xlab("No. of Student Teachers") +
        scale_x_continuous(labels = scales::comma) +  #label_number_si()) + 
        dressCodeTheme  +
        theme(axis.text.y = element_markdown(),
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5), 
              legend.title = element_blank(),
              ) +
        facet_grid(cols = vars(year))
							   
## ----  --------

							   


