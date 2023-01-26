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

tmp_df <- gender_distribution %>%

    group_by(year, gender) %>%
    summarise(median = median(NoOfStudents, na.rm = TRUE), 
              mean = mean(NoOfStudents, na.rm = TRUE),
              NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate(Subject = "all") 


tmp_df %>% 

    ggplot(aes(y = year, x = if_else(gender == "male", PercentageOfStudents, -PercentageOfStudents), 
               fill = gender, colour = gender)) +
        geom_segment(aes(xend = 0, yend = year), size = 1.5) + 
        geom_point(aes(colour = gender), size = 0.75) +
        #ggtitle(paste0("Gender Distribution - All Subjects")) +
        ylab("") +  xlab(paste0("\nGender Distribution - All Subjects")) + 
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

                                   
## ---- gender_distribution_static_charts_computing --------
                                   
tmp_df <- computing_uptake %>%

    group_by(year, gender) %>%
    summarise(median = median(NoOfStudents, na.rm = TRUE), 
              mean = mean(NoOfStudents, na.rm = TRUE),
              NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate(Subject = "all") 


tmp_df %>% 

    ggplot(aes(y = year, x = if_else(gender == "male", PercentageOfStudents, -PercentageOfStudents), fill = gender, 
       text = paste0(gender, " - ", round(PercentageOfStudents, 2) * 100, "% in ", year))) +
        geom_segment(aes(xend = 0, yend = year, colour = gender), size = 1.5) +
        geom_point(aes(colour = gender), size = 0.75) +
        #ggtitle(paste0("Gender Distribution - Computing")) +
        ylab("") + xlab(paste0("\nGender Distribution - Computing")) +
        scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) + 
        scale_colour_manual(values = gender_colour_scheme) + 
        scale_fill_manual(values = gender_colour_scheme) + 
        dressCodeTheme +
        theme(axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
              axis.text.y = element_text(size = 8), 
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_line(), 
              legend.title = element_blank(), 
              legend.position = "bottom")             


                                   
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
            xlab("\nGender Distribution Overall") + 
                 #paste0("\nGender Distribution Overall,\n", 
                 #       str_wrap(paste0(sqa_qualifications_ordering$qualification, collapse = ", "), width = 60, exdent = 4))) +
 
            ylab(NULL) +
            scale_y_discrete(breaks = time_period_axis_breaks$year, labels = time_period_axis_breaks$tick_label) +
            scale_x_continuous(labels = function(x) scales::percent(abs(x)), breaks = seq(-1, 1, 0.2)) + 
            #scale_shape_manual(values = c(6, 2)) + #-0x2640L, -0x2642L)) + # \u2640 and \u2642 
            scale_colour_manual(values = gender_colour_scheme) + 
            scale_fill_manual(values = gender_colour_scheme) + 
            guides(fill = guide_legend(reverse = TRUE), colour = "none") + 
            dressCodeTheme +
            theme(panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_line(), 
                  axis.text.x = element_text(size = 8, angle = 0, vjust = 0),
                  axis.text.y = element_text(size = 9, margin = margin(0, 0, 0, 0, unit = "cm")), 
                  axis.title = element_text(size = 11),
                  legend.title = element_blank(), legend.position = "bottom")                                               

                               

convertToPlotly(plot1, renameTraces = TRUE, regexpr = genderRegex, height = 450, width = 800, maxMargin = 1) %>% 
    layout(legend = list(orientation = 'h', yanchor = "bottom"),
           margin = list(l = 1)) %>%
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

focus_subject_label <- "computing / information systems / data science"

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
                xlab(paste0("\nGender Distribution - ", str_to_title(focus_subject_label))) +
                     #paste0("\nGender Distribution - ", str_to_title(focus_subject_label), "\n",
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
                      axis.text.y = element_text(size = 9), 
                      axis.title = element_text(size = 11),
                      legend.title = element_blank(), 
                      legend.position = "bottom"
                     )             

convertToPlotly(plot1, renameTraces = TRUE, regexpr = genderRegex, height = 450, width = 800, maxMargin = 1) %>% 
    layout(#legend = list(orientation = 'h', yanchor = "bottom"),
           margin = list(l = 1)) %>%
    config(displayModeBar = FALSE)

rm(focus_subject_label)


## ---- summary_tables_gender_distribution_computing --------
       
computing_uptake %>%

    group_by(gender) %>%
    summarise(NoOfStudents = sum(NoOfStudents, na.rm = TRUE),
              AllEntries = sum(AllEntries, na.rm = TRUE),
              PercentageOfStudents = (NoOfStudents / AllEntries)
              ) %>%
    mutate_at(vars(PercentageOfStudents), ~ (round(., 3) * 100)) %>%
    select(c(gender, PercentageOfStudents)) %>%
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

                           
## ---- teacher_demographics_age_median_fte_all_local_authorities --------

options(repr.plot.width = 10, repr.plot.height = 4.5)

teacher_fte_local_authority_by_age %>%
    filter((Age != "Average") & (LocalAuthority != "All local authorities")) %>%
    group_by(Year, Age) %>%
    summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE)) %>%
    ungroup() %>%
    #mutate(across(Year, ~ fct_explicit_na(., "All LocalAuthorities"))) %>%
    
    ggplot(aes(Age, TeacherFTE_median, colour = Year, group = Year)) +
        #geom_point(size = 0.5, show.legend = FALSE) +
        geom_line() + 
        ylab("Median FTE - All Local Authorities") + xlab("") + 
        dressCodeTheme +
        #scale_colour_brewer("Year", palette = "Dark2") + #, direction = -1) +
        scale_colour_romaO(discrete = TRUE) +
        #scale_fill_tofino(discrete = TRUE, reverse = TRUE) +

        theme(#legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0)
              ) 


                           
## ---- teacher_demographics_age_fte_all_local_authorities --------

teacher_fte_local_authority_by_age %>%
    filter((Age != "Average") & (LocalAuthority == "All local authorities")) %>%
    #mutate(across(Year, ~ fct_explicit_na(., "All LocalAuthorities"))) %>%
    
    ggplot(aes(Age, TeacherFTE, colour = Year, group = Year)) +
        #geom_point(size = 0.5, show.legend = FALSE) +
        geom_line() + 
        ylab("Total FTE - All Local Authorities") + xlab("") + 
        dressCodeTheme +
        #scale_colour_brewer("Year", palette = "Dark2") + #, direction = -1) +
        scale_colour_romaO(discrete = TRUE) +
        #scale_fill_tofino(discrete = TRUE, reverse = TRUE) +

        theme(#legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0)
              ) 


                           
## ---- teacher_demographics_age_fte_computing_cf_english_maths_sciences_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Age) %>%
    summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(Age != "Average") %>%
                filter((SubjectGroup %in% c("Computing", "Sciences", "ModernLanguages", "English", "Maths")) & 
                       !(Subject %in% c("General Science", "Science (General)", "Community Languages", "Other Modern Languages"))) %>%

                group_by(Age, SubjectGroup) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(SubjectGroup, ~ fct_recode(., "Sciences (excl. General)" = "Sciences")),
           across(SubjectGroup, ~ fct_explicit_na(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "ModernLanguages", "English", "Maths", after = Inf)),
          ) %>%
    
    ggplot(aes(Age, TeacherFTE_median, colour = SubjectGroup, group = SubjectGroup, shape = SubjectGroup)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Computing cf. English, Maths, Sciences & Modern Languages") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0)
              )


                           
## ---- teacher_demographics_age_fte_computing_cf_sciences_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Age) %>%
    summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(Age != "Average") %>%
                filter((SubjectGroup %in% c("Computing", "Sciences", "ModernLanguages")) & 
                       !(Subject %in% c("General Science", "Science (General)", "Community Languages", "Other Modern Languages"))) %>%
                group_by(Age, SubjectGroup) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(SubjectGroup, ~ fct_recode(., "Sciences (excl. General)" = "Sciences")),
           across(SubjectGroup, ~ fct_explicit_na(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "ModernLanguages", after = Inf)),
          ) %>%
    
    ggplot(aes(Age, TeacherFTE_median, colour = SubjectGroup, group = SubjectGroup, shape = SubjectGroup)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Computing cf. Sciences & Modern Languages") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0)
              )


                           
## ---- teacher_demographics_age_fte_computing_cf_all_sciences_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Age) %>%
    summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(Age != "Average") %>%
                filter(SubjectGroup %in% c("Computing", "Sciences", "ModernLanguages")) %>%
                group_by(Age, SubjectGroup) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(SubjectGroup, ~ fct_explicit_na(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "ModernLanguages", after = Inf)),
           across(SubjectGroup, ~ fct_recode(., "All Sciences" = "Sciences")),
           across(SubjectGroup, ~ fct_recode(., "Modern & Community Languages" = "ModernLanguages")), 
          ) %>%
    
    ggplot(aes(Age, TeacherFTE_median, colour = SubjectGroup, group = SubjectGroup, shape = SubjectGroup)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Computing cf. Sciences, Modern & Community Languages") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0)
              )


                           
## ---- teacher_demographics_age_fte_computing_cf_english_maths_all_sciences_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Age) %>%
    summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(Age != "Average") %>%
                filter(SubjectGroup %in% c("Computing", "Sciences", "ModernLanguages", "English", "Maths")) %>%

                group_by(Age, SubjectGroup) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(SubjectGroup, ~ fct_explicit_na(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "All Subjects")),
           across(SubjectGroup, ~ fct_relevel(., "ModernLanguages", "English", "Maths", after = Inf)),
           across(SubjectGroup, ~ fct_recode(., "All Sciences" = "Sciences")),
           across(SubjectGroup, ~ fct_recode(., "Modern & Community Languages" = "ModernLanguages")),          
          ) %>%
    
    ggplot(aes(Age, TeacherFTE_median, colour = SubjectGroup, group = SubjectGroup, shape = SubjectGroup)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Computing cf. English, Maths, Sciences, Modern & Community Languages") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0)
              )


                           
## ---- teacher_demographics_age_fte_computing_cf_sciences --------

teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Age) %>%
    summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(Age != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (Subject %in% c("Biology", "Chemistry", "Physics"))) %>%
                group_by(Age, Subject) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(Subject, ~ fct_explicit_na(., "All Subjects")),
           across(Subject, ~ fct_relevel(., "All Subjects")),
          ) %>%
    arrange(Age, Subject) %>%
    
    ggplot(aes(Age, TeacherFTE_median, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Median Teacher FTE - Computing cf. Sciences") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0)
              )


                          
## ---- teacher_demographics_age_fte_computing_cf_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Age) %>%
    summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                mutate(across(CommonSubjectLabel, as.character),
                       across(CommonSubjectLabel, ~ case_when(str_detect(Subject, regex("gaidhlig", ignore_case = TRUE)) ~ "Gaelic",
                                                          TRUE ~ .
                                                         )),
                       across(CommonSubjectLabel, as.factor)
                      ) %>%
                
                filter(Age != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Gaelic", "French", "German", "Spanish", "Italian"))) %>%
                group_by(Age, Subject) %>%
                summarise(across(TeacherFTE, list(median = median, average = mean), na.rm = TRUE))
    ) %>%
    mutate(across(Subject, ~ fct_explicit_na(., "All Subjects")),
           across(Subject, ~ fct_relevel(., "All Subjects")),
          ) %>%
    arrange(Age, Subject) %>%
    
    ggplot(aes(Age, TeacherFTE_median, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Median Teacher FTE - Computing cf. Modern Languages") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0)
              )

        
                           
## ---- teacher_demographics_age_over_time_computing_vs_sciences --------


teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Year, Age) %>%
    summarise(across(TeacherFTE, median, na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(Age != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Biology", "Chemistry", "Physics"))) %>%
                select(Year, Age, Subject, TeacherFTE)
    ) %>%

    mutate(across(Subject, ~ fct_explicit_na(., "Median - All Subjects")),
           across(Subject, ~ fct_relevel(., "Median - All Subjects")),
          ) %>%
     
    ggplot(aes(Age, TeacherFTE, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Teacher FTE Change 2011-2021 - Computing cf. Sciences") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5),
              ) +
        facet_wrap(~ Year, nrow = 2)


                           
## ---- teacher_demographics_age_over_time_computing_vs_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Year, Age) %>%
    summarise(across(TeacherFTE, median, na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
               mutate(across(CommonSubjectLabel, as.character),
                       across(CommonSubjectLabel, ~ case_when(str_detect(Subject, regex("gaidhlig", ignore_case = TRUE)) ~ "Gaelic",
                                                          TRUE ~ .
                                                         )),
                       across(CommonSubjectLabel, as.factor)
                      ) %>%
                
                filter(Age != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Gaelic", "French", "German", "Spanish", "Italian"))) %>%
                select(Year, Age, Subject, TeacherFTE)
    ) %>%

    mutate(across(Subject, ~ fct_explicit_na(., "Median - All Subjects")),
           across(Subject, ~ fct_relevel(., "Median - All Subjects")),
          ) %>%
    
    ggplot(aes(Age, TeacherFTE, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Teacher FTE Change 2011-2021 - Computing cf. Modern Languages") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
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
    filter(Age != "Average") %>%
    group_by(Age, Year) %>%
    summarise(across(TeacherFTE, median, na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
                filter(Age != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Biology", "Chemistry", "Physics"))) %>%
                select(Year, Age, Subject, TeacherFTE)
    ) %>%

    mutate(across(Subject, ~ fct_explicit_na(., "Median - All Subjects")),
           across(Subject, ~ fct_relevel(., "Median - All Subjects")),
          ) %>%
    #arrange(Year, Age, Subject) %>%
    
    ggplot(aes(Year, TeacherFTE, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Teacher FTE Change 2011-2021 - Computing cf. Sciences") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0),
              #plot.title = element_text(size = 16),
              axis.text.y = element_text(size = 14), # element_markdown
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5),
              ) +
        facet_wrap(~ Age, nrow = 2)


                          
## ---- teacher_demographics_age_change_over_time_computing_vs_modern_languages --------

teacher_fte_main_subject_by_age %>%
    filter(Age != "Average") %>%
    group_by(Age, Year) %>%
    summarise(across(TeacherFTE, median, na.rm = TRUE)) %>%

    bind_rows(teacher_fte_main_subject_by_age %>%
               mutate(across(CommonSubjectLabel, as.character),
                       across(CommonSubjectLabel, ~ case_when(str_detect(Subject, regex("gaidhlig", ignore_case = TRUE)) ~ "Gaelic",
                                                          TRUE ~ .
                                                         )),
                       across(CommonSubjectLabel, as.factor)
                      ) %>%
                
                filter(Age != "Average") %>%
                filter((SubjectGroup  == "Computing") | 
                       (CommonSubjectLabel %in% c("Gaelic", "French", "German", "Spanish", "Italian"))) %>%
                select(Year, Age, Subject, TeacherFTE)
    ) %>%

    mutate(across(Subject, ~ fct_explicit_na(., "Median - All Subjects")),
           across(Subject, ~ fct_relevel(., "Median - All Subjects")),
          ) %>%
    #arrange(Year, Age, Subject) %>%
    
    ggplot(aes(Year, TeacherFTE, colour = Subject, group = Subject, shape = Subject)) +
        geom_point(size = 1.05, show.legend = FALSE) +
        geom_line() + 
        ggtitle("Teacher FTE Change 2011-2021 - Computing cf. Modern Languages") + 
        ylab("Median FTE") + xlab("") + 
        dressCodeTheme +
        scale_colour_brewer("subject", palette = "Dark2") + #, direction = -1) +
        scale_shape_manual(values = 0:6) +
        theme(legend.position = "bottom", 
              legend.title = element_blank(), 
              legend.margin = margin(0),
              axis.text.y = element_text(size = 14), # element_markdown
              axis.text.x = element_text(size = 11, angle = 45, vjust = 0.5),
              ) +
        facet_wrap(~ Age, nrow = 2)



                           
                           
## ----  --------




