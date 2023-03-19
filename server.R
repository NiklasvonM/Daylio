
shinyServer(function(input, output) {

    cur_activity <- reactive({
        req(input$activity)
        input$activity
    })
    
    plot_mood <- reactive({
        activity <- cur_activity()
        useNames <- intersect(c("Day", "Weekday", "Mood", "Note", activity), names(DATA))
        dtPlot <- DATA[, useNames, with = FALSE]
        dtPlot[, (activity) := as.ordered(get(activity))]
        dtPlot[, RunningAvg7 := frollmean(Mood, 7, algo = "exact", align = "left")]
        dtPlot[, RunningAvg30 := frollmean(Mood, 30, algo = "exact", align = "left")]
        
        dtDailyPlacesVisitedHovertext <- DT_LOCATION[, .(HovertextOrte = paste0(
          paste0(gsub("\n", " ", unique(AdresseFull))),
          collapse = "<br>")), .(Day)]
        dtPlot <- merge(dtPlot, dtDailyPlacesVisitedHovertext, all.x = TRUE, by = "Day")
        
        
        
        fnSwitch <- if(LANGUAGE == "de") {
          function(x) switch(
            as.character(x),
            "1" = "Lausig",
            "2" = "Schlecht",
            "3" = "Ok",
            "4" = "Gut",
            "5" = "Super",
            "NA"
          )
        } else if (LANGUAGE == "en") {
          function(x) switch(
            as.character(x),
            "1" = "awful",
            "2" = "bad",
            "3" = "meh",
            "4" = "good",
            "5" = "rad",
            "NA"
          )
        } else {
          stop("LANGUAGE in config.R unknown.")
        }
        
        activity <- as.name(activity)
        p <- if("Note" %in% names(dtPlot)) {
          ggplot(
            data = dtPlot
          ) +
            geom_point(
              mapping = eval(bquote(aes(
                x = Day,
                y = Mood,
                color = .(activity),
                text = paste0(
                  "Day: ", format(Day, "%d.%m.%Y"), "<br>",
                  "Mood: ", sapply(Mood, fnSwitch), "<br>",
                  activity, ": ", .(activity), "<br>",
                  "7-day mood average: ", round(RunningAvg7, 2), "<br>",
                  "30-day mood average: ", round(RunningAvg30, 2), "<br>",
                  ifelse(Note == "", "", "<br>"), Note, ifelse(Note == "", "", "<br><br>"),
                  'Places visited :<br>', HovertextOrte
                )
              )))
            )
        } else {
          ggplot(
            data = dtPlot
          ) +
            geom_point(
              mapping = eval(bquote(aes(
                x = Day,
                y = Mood,
                color = .(activity),
                text = paste0(
                  "Day: ", format(Day, "%d.%m.%Y"), "<br>",
                  "Mood: ", sapply(Mood, fnSwitch), "<br>",
                  activity, ": ", .(activity), "<br>",
                  "7-day mood average: ", round(RunningAvg7, 2), "<br>",
                  "30-day mood average: ", round(RunningAvg30, 2), "<br>",
                  'Places visited :<br>', HovertextOrte
                )
              )))
            )
        }
        p <- p +
            geom_line(aes(x = Day, y = RunningAvg7), color = "red", alpha = 0.6) +
            geom_line(aes(x = Day, y = RunningAvg30), color = "blue", alpha = 0.6)
        p <- ggplotly(p, tooltip = "text")
        p
    })
    
    output$date_dots <- renderPlotly({
        plot_mood()
    })
    
    output$correlation <- renderValueBox({
        activity <- cur_activity()
        correlation <- measureOfDependence(DATA$Mood, DATA[[activity]])
        correlation <- round(correlation, 2)
        valueBox(
            value = correlation,
            subtitle = paste0("Correlation mood and ", activity)
        )
    })
    
    output$avg_mood <- renderInfoBox({
        activity <- cur_activity()
        valWith <- round(mean(DATA[get(activity) == 1, Mood], na.rm = TRUE), 2)
        valWithout <- round(mean(DATA[get(activity) == 0, Mood], na.rm = TRUE), 2)
        infoBox(
            title = "Average mood with/without activity",
            value = paste(valWith, valWithout, sep = "/")
        )
    })
    
    output$roll_sum_activity <- renderPlotly({
      activity <- input$activity
      dtPlot <- DATA[, c("Day", activity), with = FALSE]
      setorder(dtPlot, "Day")
      dtPlot[, `14 days rolling sum` := frollsum(get(activity), n = 14)]
      # https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/
      ggplot(dtPlot, aes(Day, `14 days rolling sum`)) +
        geom_line(color="#69b3a2", size=2) +
        geom_point(size = 5) +
        geom_point(
          aes(
            color = `14 days rolling sum`,
            color = after_scale(invert_color(color))
          ), size = 2) +
        scale_color_scico(palette = "hawaii", guide = "none") +
        ggtitle(activity) +
        theme_minimal()
    })

    
    output$download <- downloadHandler(
        filename = function() {
            paste('data-', Sys.Date(), '.html', sep='')
        },
        content = function(con) {
         htmlwidgets::saveWidget(plot_mood(), con)
        }
    )
    
    output$year_heatmap <- renderPlotly({
        dtPlot <- DATA[, .(Day, Mood)]
        dtPlot[, Day := format(Day, "%m-%d")]
        dtPlot <- dtPlot[, .(
            Mood = ifelse(length(Mood) > 0, mean(Mood, na.rm = TRUE), NA)
        ), by = .(Day)]
        # Merge all possible days
        dtMerge <- data.table(Day = seq(as.Date("2004-01-01"), as.Date("2004-12-31"), by = "days"))
        dtMerge[, DayOfMonth := mday(Day)]
        dtMerge[, Month := month(Day)]
        dtMerge[, Day := format(Day, format = "%m-%d")]
        dtPlot <- merge(dtPlot, dtMerge, all.y = TRUE, by = c("Day"))
        
        p <- ggplot(
            data = dtPlot,
            mapping = aes(
                x = Month,
                y = DayOfMonth,
                fill = Mood,
                text = paste0(
                    "Day: ", Day, "<br>",
                    "Average mood: ", Mood
                )
            )
        ) +
            geom_tile() +
            scale_fill_gradient(low = "red", high = "green") +
            scale_x_continuous(breaks = 1:12) +
            scale_y_reverse(breaks = 1:31)
        p <- ggplotly(p, tooltip = "text")
        p
    })
    
    output$day_of_year_activity_plot <- renderPlotly({
      activity <- input$activity
      dtPlot <- DATA[, c("Day", activity), with = FALSE]
      dtPlot[, Year := year(Day)]
      dtPlot[, Day := format(Day, "%m-%d")]
      activityName <- as.name(activity)
      
      breaks <- paste0(ifelse(1:12 > 10, "", "0"), 1:12, "-01")
      
      p <- ggplot(
        data = dtPlot,
        mapping = eval(bquote(aes(
          x = Day,
          y = Year,
          fill = .(activityName),
          text = paste0(
            "Day: ", paste0(Year, "-", Day), "<br>",
            activity, ": ", .(activityName)
          )
        )))
      ) +
        geom_tile() +
        scale_fill_gradient(low = "red", high = "green") +
        scale_x_discrete(breaks = breaks) +
        scale_y_reverse()
      p <- ggplotly(p, tooltip = "text")
      p
    })
    
    output$weekday_hist <- renderPlotly({
        req(input$activity)
        activity <- input$activity
        dtPlot <- DATA[, c("Weekday", activity), with = FALSE]
        dtPlot[, Weekday := factor(Weekday, levels = WEEKDAYS)]
        setnames(dtPlot, activity, "TempColumnName")
        dtPlot <- dtPlot[, .(TempColumnName = sum(TempColumnName, na.rm = TRUE)), by = .(Weekday)]
        setnames(dtPlot, "TempColumnName", activity)
        p <-
            ggplot(
                dtPlot,
                eval(bquote(aes(
                    x = Weekday,
                    y = get(activity),
                    text = paste0(
                        "Weekday: ", Weekday, "<br>",
                        .(activity), ": ", get(activity)
                    )
                )))
            ) +
            geom_bar(stat = "identity", fill = "#0a3b7e") +
            ylab(activity) +
            theme_minimal()
        ggplotly(p, tooltip = "text")
    })
    
    output$correlation_table <- renderRHandsontable({
        tbl <- rhandsontable(DT_COR)
    
        tbl <- hot_cols(tbl,
        renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (col == 1 && value < -0.1) {
              td.style.background = 'pink';
             } else if (col == 1 && value > 0.1) {
              td.style.background = 'lightgreen';
             }
           }")
        tbl
    })
    
    output$correlation_matrix <- renderRHandsontable({#renderDataTable({
        # tbl <- rhandsontable(MAT_COR)
        # tbl <- hot_cols(tbl, fixedColumnsLeft = 0)
        # tbl <- hot_rows(tbl, fixedRowsTop = 0)
        tbl <- rhandsontable(MAT_COR)
        tbl <- hot_cols(tbl,
            renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (col != row && value < -0.1) {
              td.style.background = 'pink';
             } else if (col != row && value > 0.1) {
              td.style.background = 'lightgreen';
             }
           }")
        tbl
    })
    
    output$correlation_matrix_plot <- renderPlotly({
      dtPlot <- data.table(
        Activity = rownames(MAT_COR)[row(MAT_COR)],
        `Activity2` = colnames(MAT_COR)[col(MAT_COR)],
        CorrelationPlot = c(MAT_COR)
      )
      for(col in ACTIVITIES_MOOD) {
        maxCor <- max(abs(dtPlot[Activity == col & Activity != Activity2]$CorrelationPlot))
        dtPlot[Activity == col, Shape := as.factor(as.integer(abs(CorrelationPlot) == maxCor))]
      }
      dtPlot[, Correlation := CorrelationPlot]
      dtPlot[!(input$correlationThreshold[1] <= Correlation & Correlation <= input$correlationThreshold[2]), CorrelationPlot := NA]
      dtPlot[Correlation < 0, Color := -1]
      dtPlot[Correlation > 0, Color := 1]
      dtPlot[is.na(Color), Color := 0]
      p <- ggplot(
        dtPlot,
        aes(
          Activity,
          Activity2,
          size = abs(CorrelationPlot),
          color = Color,
          shape = Shape,
          text = paste0(
            "Activity 1: ", Activity, "<br>",
            "Activity 2: ", Activity2, "<br>",
            "Correlation: ", Correlation
          )
        )
      ) +
        geom_point() +
        scale_color_continuous(low = "#3794bf", high = "#df8640") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab("Activity 1") +
        ylab("Activity 2") +
        scale_size_continuous(limits = c(0, 1))
      ggplotly(p, tooltip = "text")
    })
    
    output$correlation_matrix_lag <- renderRHandsontable({#renderDataTable({
      # tbl <- rhandsontable(MAT_COR)
      # tbl <- hot_cols(tbl, fixedColumnsLeft = 0)
      # tbl <- hot_rows(tbl, fixedRowsTop = 0)
      tbl <- rhandsontable(MAT_COR_LAG, readOnly = TRUE)
      tbl <- hot_cols(tbl,
                      renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (col != row && value < -0.1) {
              td.style.background = 'pink';
             } else if (col != row && value > 0.1) {
              td.style.background = 'lightgreen';
             }
           }")
      tbl <- hot_rows(tbl, fixedRowsTop = 1)
      tbl
    })
    
    output$correlation_matrix_lag_plot <- renderPlotly({
      dtPlot <- data.table(
        Activity = rownames(MAT_COR_LAG)[row(MAT_COR_LAG)],
        `Activity_Lag_1` = colnames(MAT_COR_LAG)[col(MAT_COR_LAG)],
        CorrelationPlot = c(MAT_COR_LAG)
      )
      for(col in ACTIVITIES_MOOD) {
        maxCor <- max(abs(dtPlot[Activity == col & Activity != Activity_Lag_1]$CorrelationPlot))
        dtPlot[Activity == col, Shape := as.factor(as.integer(abs(CorrelationPlot) == maxCor))]
      }
      dtPlot[, Correlation := CorrelationPlot]
      dtPlot[!(input$correlationThresholdLag[1] <= Correlation & Correlation <= input$correlationThresholdLag[2]), CorrelationPlot := NA]
      dtPlot[Correlation < 0, Color := -1]
      dtPlot[Correlation > 0, Color := 1]
      dtPlot[is.na(Color), Color := 0]
      p <- ggplot(
        dtPlot,
        aes(
          Activity,
          Activity_Lag_1,
          size = abs(CorrelationPlot),
          color = Color,
          shape = Shape,
          text = paste0(
            "Activity: ", Activity, "<br>",
            "Activity next day: ", Activity_Lag_1, "<br>",
            "Correlation: ", Correlation
          )
        )
      ) +
        geom_point() +
        scale_color_continuous(low = "#3794bf", high = "#df8640") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab("Activity") +
        ylab("Activity next day") +
        scale_size_continuous(limits = c(0, 1))
      ggplotly(p, tooltip = "text")
    })
    
    output$single_day <- renderRHandsontable({
        date <- input$day
        tbl <- DATA[Day == date]
        req(nrow(tbl) > 0)
        tbl <- melt.data.table(
            data = tbl,
            id.vars = character(0),
            measure.vars = names(tbl),
            variable.name = "Activity",
            value.name = "Value",
            value.factor = TRUE
        )
        tbl[Activity == "Day", Value := format(as.Date(as.integer(Value), origin = "1970-01-01"), format = "%d.%m.%Y")]
        rhandsontable(tbl)
    })
    
    output$lookback_pointplot <- renderPlotly({
        dt <- DATA[, c("Mood", input$activity), with = FALSE]
        dt[, Count := countEntriesLookback(get(input$activity), nLookback = input$lookback_pointplot_n - 1)]
        dt <- dt[!is.na(Count)]
        dt[, N := .N, by = c("Count")]
        dtPlot <- dt[, .(Mood = mean(Mood, na.rm = TRUE)), by = c("Count", "N")]
        dtForLM <- data.table()
        for(i in seq_len(nrow(dtPlot))) {
          dtForLM <- rbindlist(list(dtForLM, data.table(Count = rep(dtPlot[i]$Count, dtPlot[i]$N), Mood = dtPlot[i]$Mood)))
        }
        mdl <- lm(Mood ~ Count + 1, dtForLM)
        dtPlotLM <- data.table(Count = range(dtPlot$Count), Mood = mdl$coefficients["(Intercept)"] + range(dtPlot$Count) * mdl$coefficients["Count"])
        p <- ggplot(dtPlot, aes(Count, Mood)) +
            geom_point(aes(size = N)) +
            geom_line() +
            geom_line(aes(Count, Mood), dtPlotLM, linetype = "dashed") +
            scale_y_continuous(limits = c(1, 5))
        p <- ggplotly(p)
        p
    })
    
    output$mood_distribution_by_activity_value <- renderPlotly({
      activity <- input$activity
      dtPlot <- DATA[, c("Mood", activity), with = FALSE]
      if (is.numeric(dtPlot[[activity]])) {
          dtPlot[, (activity) := ordered(as.character(get(activity)))]
      } else {
          dtPlot[, (activity) := factor(get(activity))]
      }
      dtPlot[, n_activity := .N, by = c(activity)]
      dtPlot[, `n data points` := .N, by = c("Mood", activity)]
      dtPlot <- unique(dtPlot)
      dtFill <- as.data.table(expand.grid(
          Mood = unique(dtPlot$Mood),
          V1 = unique(dtPlot[[activity]])
      ))
      setnames(dtFill, "V1", activity)
      dtPlot <- merge(dtPlot, dtFill, by = c("Mood", activity), all = TRUE)
      dtPlot[, `MoodDistribution` := `n data points` / n_activity]
      dtPlot[is.na(`MoodDistribution`), `MoodDistribution` := 0]
      dtPlot[is.na(`n data points`), `n data points` := 0]
      if(activity == "Schlafqualitaet") {
        if(all(c("Schlecht", "Mäßig", "Gut") %in% dtPlot$Schlafqualitaet)) {
          dtPlot[get(activity) == -1, (activity) := "Schlecht"]
          dtPlot[get(activity) == 0, (activity) := "Mäßig"]
          dtPlot[get(activity) == 1, (activity) := "Gut"]
        }
      }
      setnames(dtPlot, activity, make.names(activity))
      p <- ggplot(dtPlot, aes_string(x = "Mood", y = "MoodDistribution", color = make.names(activity))) +
          geom_line() +
          geom_point(aes(size = `n data points`)) +
          scale_y_continuous(limits = c(0, 1)) +
          scale_x_continuous(limits = c(1, 5)) +
          scale_size_continuous(limits = c(0, max(dtPlot$`n data points`)), guide = "none") +
          theme_minimal()
      p <- ggplotly(p)
      
      p
    })
    
    output$mood_distribution_wwo_activity <- renderPlotly({
      dtCur <- DATA
      use <- input$dropzone_w
      use_not <- input$dropzone_wo
      if(length(use) > 0) {
        for(activity in use) {
          dtCur <- dtCur[get(activity) >= 1]
        }
      }
      if(length(use_not) > 0) {
        for(activity in use_not) {
          dtCur <- dtCur[get(activity) <= 0]
        }
      }
      req(nrow(dtCur) > 0)
      dtPlot <- dtCur[, .(Dichte = .N / nrow(dtCur)), by = .(Mood)]
      dtPlot <- merge(dtPlot, data.table(Mood = 1:5), by = c("Mood"), all = TRUE)
      dtPlot[is.na(Dichte), Dichte := 0]
      p <- ggplot(dtPlot, aes(Mood, Dichte, text = paste0(
        "Mood: ", Mood, "<br>",
        "Dichte: ", round(100 * Dichte, 2), "%"
      ))) +
        geom_bar(stat = "identity", fill = "#0a3b7e") +
        geom_text(aes(label=paste0(round(100 * Dichte), "%"), y = Dichte + 0.05)) +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_blank(), axis.title.y = element_blank(),
              axis.ticks = element_blank(),
              axis.text.y = element_blank()) +
        #scale_x_continuous(limits = c(1, 5)) +
        scale_y_continuous(limits = c(0, 1))
      g <- ggplotly(p, tooltip = "text")
      g
    })
    
    output$days_distr_w_activity_plot <- renderPlotly({
      activity <- input$activity
      dtPlot <- DT_ACTIVITY_LENGTH_DISTR[Activity == activity, .(Days, `Count` = `n with activity`)]
      
      # Estimate exponential distribution using maximum likelihood estimation
      lambda_mle <- 1/ (sum(dtPlot[, .(Days * Count)]) / sum(dtPlot$Count))
      # bias corrected maximum likelihood estimation
      lambda_mle_bias_corrected <- lambda_mle - lambda_mle / (sum(dtPlot$Count) - 1)
      dtPlot[, EstimatedDensity := dexp(Days, rate = lambda_mle_bias_corrected) * sum(dtPlot$Count)]
      
      maxDay <- max(dtPlot[Count > 0]$Days)
      dtPlot <- dtPlot[Days <= maxDay]
      p <- ggplot(dtPlot, aes(Days, Count)) +
        geom_bar(stat = "identity") +
        geom_line(aes(y = EstimatedDensity)) +
        ggtitle(paste0("Distribution of the number of consecutive days with activity '", activity, "'"))
      ggplotly(p)
    })
    
    output$days_distr_wo_activity_plot <- renderPlotly({
      activity <- input$activity
      dtPlot <- DT_ACTIVITY_LENGTH_DISTR[Activity == activity, .(Days, `Count` = `n without activity`)]
      
      # Estimate exponential distribution using maximum likelihood estimation
      lambda_mle <- 1/ (sum(dtPlot[, .(Days * Count)]) / sum(dtPlot$Count))
      # bias corrected maximum likelihood estimation
      lambda_mle_bias_corrected <- lambda_mle - lambda_mle / (sum(dtPlot$Count) - 1)
      dtPlot[, EstimatedDensity := dexp(Days, rate = lambda_mle_bias_corrected) * sum(dtPlot$Count)]
      
      maxDay <- max(dtPlot[Count > 0]$Days)
      dtPlot <- dtPlot[Days <= maxDay]
      p <- ggplot(dtPlot, aes(Days, Count)) +
        geom_bar(aes(y = Count), stat = "identity") +
        geom_line(aes(y = EstimatedDensity)) +
        ggtitle(paste0("Distribution of the number of consecutive days without activity '", activity, "'"))
      ggplotly(p)
    })
    
    output$forcenetwork <- renderVisNetwork({
      
      links <- data.table(reshape2::melt(MAT_COR))
      setnames(links, c("Activity1", "Activity2", "value"))
      links <- unique(links)
      index1 <- data.table(Activity1 = ACTIVITIES)
      index2 <- data.table(Activity2 = ACTIVITIES)
      index1[, from := .I-1]
      index2[, to := .I-1]
      links <- merge(links, index1, all.x = TRUE, by = "Activity1")
      links <- merge(links, index2, all.x = TRUE, by = "Activity2")
      links <- links[from < to]
      links <- links[value > 0.1]
      
      nodes <- data.table(label = ACTIVITIES)
      nodes[, id := .I-1]
      
      if(file.exists("ActivityGroups.csv")) {
        activityGroups <- fread("ActivityGroups.csv", encoding = "UTF-8")
        nodes <- merge(nodes, activityGroups, by.x = "label", by.y = "Activity", all.x = TRUE)
        #setnames(nodes, "ActivityGroup", "group")
      }
      
      nodes <- merge(
        nodes[, -"group", with = FALSE],
        DT_COR[, .(label = Activity, Correlation)],
        all.x = TRUE, by = "label"
      )
      nodes[Correlation < -0.05, group := -1]
      nodes[-0.05 <= Correlation & Correlation <= 0.05, group := 0]
      nodes[0.05 < Correlation, group := 1]
      nodes[is.na(group), group := 99]
      
      nodes[, title := paste0(
        "Activity: ", label, "<br>",
        "Correlation with mood: ", Correlation
      )]
      
      if("ActivityGroup" %in% names(nodes)) {
        nodes[, title := paste0(
          title, "<br>",
          "Group: ", ActivityGroup
        )]
      }
      
      
      visNetwork(nodes, links)
      
    })
    
    
    output$count_activities_plot <- renderPlot({
      dtPlot <- copy(DT_COR)
      
      dtPlot[Correlation < -0.05, `correlation with mood` := "negative"]
      dtPlot[-0.05 <= Correlation & Correlation <= 0.05, `correlation with mood` := "neutral"]
      dtPlot[0.05 < Correlation, `correlation with mood` := "positive"]
      dtPlot[, `correlation with mood` := as.factor(`correlation with mood`)]
      
      
      empty_bar <- ceiling(nrow(dtPlot) / 20)
      to_add <- as.data.table(matrix(NA, empty_bar*nlevels(dtPlot$`correlation with mood`), ncol(dtPlot)))
      colnames(to_add) <- copy(colnames(dtPlot))
      to_add[, `correlation with mood` := rep(levels(dtPlot$`correlation with mood`), each=empty_bar)]
      dtPlot <- rbindlist(list(dtPlot, to_add))
      
      setorder(dtPlot, "correlation with mood", -"Average Activity")
      dtPlot[, id := .I]
      
      maxVal <- max(dtPlot$`Average Activity`, na.rm = TRUE)*10
      label_data <- copy(dtPlot)
      number_of_bar <- nrow(label_data)
      angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
      label_data$hjust <- ifelse(angle < -90, 1, 0)
      label_data$angle <- ifelse(angle < -90, angle+180, angle)
      
      setnames(dtPlot, "Correlation", "Correlation with Mood")
      p <- ggplot(dtPlot, aes(x=id, y = `Average Activity`*10, fill = `Correlation with Mood`)) +       
        geom_bar(stat="identity") +
        ylim(c(-8, maxVal*1.1+1)) +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(rep(-1,4), "cm")
        ) +
        scale_fill_gradient2() +
        coord_polar(start = 0) + 
        geom_text(data=label_data, aes(x=id, y=`Average Activity`*10+maxVal/10, label=Activity, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE) 
      
      p
    })
    
    output$wordcloud <- renderWordcloud2({
      dt <- data.table(Word = unlist(str_split(paste0(DATA$Note, collapse = " "), " ")))
      
      # Remove all entries that contain two digits in a row
      dt <- dt[!grepl("[[:digit:][:digit:]]", Word)]
      # Remove most common English and German words
      dt <- dt[!tolower(Word) %in% stopwords('english')]
      dt <- dt[!tolower(Word) %in% stopwords('de')]
      # Remove punctuation
      dt[, Word := gsub("[[:punct:]]", "", Word)]
      
      dt <- dt[, .(N = .N), by = .(Word)]
      
      wordcloud2(dt, size = 20)
    })
    
    
    
    
    # world map with daily visits 
    output$worldmap <- renderLeaflet({
      
      dateSelected <- input$day
      
      basemap <- leaflet()  %>%
        addTiles()
        #addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE))
      
      dtMovement <- DT_MOVEMENT[Day == dateSelected]
      for(i in seq(length = nrow(dtMovement))) {
        curMovement <- dtMovement[i]
        
        dtWayPoint <- rbindlist(list(
          data.table(latE7 = curMovement$StartLatitude * 1e7, lngE7 = curMovement$StartLongitude * 1e7),
          curMovement$wayPoints[[1]],
          data.table(latE7 = curMovement$EndLatitude * 1e7, lngE7 = curMovement$EndLongitude * 1e7)
        ))
        dtWayPoint[, `:=`(Lat = latE7 / 1e7, Lng = lngE7 / 1e7)]
        
        js <- nrow(dtWayPoint)
        for(j in seq(length = js-1)) {
          if(dtWayPoint[j]$Lat != dtWayPoint[j+1]$Lat |
             dtWayPoint[j]$Lng != dtWayPoint[j+1]$Lng) {
            basemap <- basemap %>%
              addFlows(
                lng0 = dtWayPoint[j]$Lng,
                lat0 = dtWayPoint[j]$Lat,
                lng1 = dtWayPoint[j+1]$Lng,
                lat1 = dtWayPoint[j+1]$Lat,
                opacity = 0.5,
                flow = 0.1,
                color = "#848484",
                popup = popupArgs(
                  html = paste0(
                    "Startzeit: ",
                    ifelse(curMovement$DatumStart != curMovement$DatumEnde, paste0(format(curMovement$DatumStart, "%d.%m.%Y"), " "), ""),
                    format(curMovement$Startzeit, "%H:%M"), " Uhr", "<br>",
                    "Endzeit: ",
                    ifelse(curMovement$DatumStart != curMovement$DatumEnde, paste0(format(curMovement$DatumEnde, "%d.%m.%Y"), " "), ""),
                    format(curMovement$Endzeit, "%H:%M"), " Uhr", "<br>",
                    "Distanz: ", ifelse(
                      curMovement$Distanz >= 1000,
                      paste0(round(curMovement$Distanz / 1000, 2), "km"),
                      paste0(curMovement$Distanz, "m")
                    ), "<br>",
                    "Fortbewegungsmittel: ", curMovement$Fortbewegungsmittel, "<br>",
                    "Startbreitengrad: ", dtWayPoint[j]$Lat, "<br>",
                    "Startlängengrad: ", dtWayPoint[j]$Lng, "<br>",
                    "Endbreitengrad: ", dtWayPoint[j+1]$Lat, "<br>",
                    "Endlängengrad: ", dtWayPoint[j+1]$Lng, "<br>"
                  )
                )
              )
          }
        }

        
      }
      
      
      dtLocations <- DT_LOCATION[Day == dateSelected]
      # TODO: Handle overwriting if a location is visited more than once.
      for(i in seq(length = nrow(dtLocations))) {
        curLocation <- dtLocations[i]
        curStartzeit <- curLocation$Startzeit
        curEndzeit <- curLocation$Endzeit
        basemap <- basemap %>%
          addMinicharts(
            lng = curLocation$Longitude,
            lat = curLocation$Latitude,
            #chartdata = curEndzeit - curStartzeit,
            time = curEndzeit - curStartzeit,
            opacity = 0.5,
            # Currently not working
            labelText = paste0(format(curStartzeit, "%H:%M"), " - ", format(curEndzeit, "%H:%M")),
            showLabels = TRUE,
            # Hovertext
            popup = popupArgs(
              html = paste0(
                "Adresse: ", curLocation$AdresseFull, "<br>",
                "Startzeit: ",
                  ifelse(curLocation$DatumStart != curLocation$DatumEnde, paste0(format(curLocation$DatumStart, "%d.%m.%Y"), " "), ""),
                  format(curStartzeit, "%H:%M"), " Uhr", "<br>",
                "Endzeit: ",
                  ifelse(curLocation$DatumStart != curLocation$DatumEnde, paste0(format(curLocation$DatumEnde, "%d.%m.%Y"), " "), ""),
                  format(curEndzeit, "%H:%M"), " Uhr<br>"
              )
            )
          )
      }
      
      basemap
    })
    
    # world map with daily visits 
    output$plz_visited <- renderLeaflet({
      
      basemap <- leaflet()  %>%
        addTiles()
        #addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE))
      
      for(i in seq(length = nrow(PLZ_VISITED))) {
        curLocation <- PLZ_VISITED[i]
        curStartzeit <- curLocation$Startzeit
        curEndzeit <- curLocation$Endzeit
        basemap <- basemap %>%
          addMinicharts(
            lng = curLocation$Longitude,
            lat = curLocation$Latitude,
            opacity = 0.5,
            # Hovertext
            popup = popupArgs(
              html = paste0(
                "PLZ: ", curLocation$PLZ, "<br>",
                "Erster Besuch: ", format(curLocation$FirstVisit, "%d.%m.%Y"), "<br>",
                "Letzter Besuch: ", format(curLocation$LastVisit, "%d.%m.%Y"), "<br>",
                "Anzahl Besuche: ", curLocation$TimesVisited, "<br>",
                "Latitude: ", curLocation$Latitude, "<br>",
                "Longitude: ", curLocation$Longitude
              )
            )
          )
      }
      
      basemap
    })
    
    # timeline animation of places visited within the last year
    output$plz_visited_timeline <- renderLeaflet({
      #dtTimeline <- PLZ_VISITED[, .(Latitude, Longitude, start = FirstVisit, end = today())]
      #dtTimeline <- dtAllPlacesVisited[, .(Latitude, Longitude, start = Zeit, end = today())]
      dtTimeline <- PLACES_VISITED[, .(Latitude, Longitude, start = MinDate, end = pmin(max(MaxDate), MinDate+365))]
      geoTimeline <- geojsonio::geojson_json(dtTimeline,lat="Latitude",lon="Longitude")
      leaflet() %>%
        addTiles() %>%
        setView(44.0665,23.74667,2) %>%
        addTimeline(data = geoTimeline)
    })
    
    output$places_visited <- renderLeaflet({
      #https://bhaskarvk.github.io/leaflet-talk-rstudioconf-2017/RstudioConf2017.html#13
      DT_ALL_PLACES_VISITED.df <- as.data.frame(DT_ALL_PLACES_VISITED) %>%
        split(.$Standortinformationen)
      l <- leaflet() %>%
        addTiles()
      names(DT_ALL_PLACES_VISITED.df) %>%
        purrr::walk(function(df) {
          dataCur <- DT_ALL_PLACES_VISITED.df[[df]]
          l <<- l %>%
            addMarkers(
              data=dataCur,
              lng=~Longitude,
              lat=~Latitude,
              label=~as.character(Standortinformationen),
              popup=paste0(
                    "Adresse: ", dataCur$AdresseFull, "<br>",
                    "PLZ: ", dataCur$PLZ, "<br>",
                    "Erster Besuch: ", dataCur$`Erster Besuch`, "<br>",
                    "Letzter Besuch: ", dataCur$`Letzter Besuch`, "<br>",
                    "Latitude: ", dataCur$Latitude, "<br>",
                    "Longitude: ", dataCur$Longitude
              ),
              group = df,
              clusterOptions = markerClusterOptions()
            )
        })
      l <- l %>%
        addLayersControl(
          overlayGroups = names(DT_ALL_PLACES_VISITED.df),
          options = layersControlOptions(collapsed = FALSE)) %>%
        addMiniMap(width = 120, height=80)
      l
    })
    
    output$strucutral_break_test <- renderPlotly({
      activity <- cur_activity()
      
      dtCur <- DATA[, c("Day", activity, "Mood"), with = FALSE]
      
      dtCoeff <- as.data.table(expand.grid(
        l = DATA[mday(Day) == 1]$Day, u = DATA[mday(Day) == 1]$Day
      ))
      dtCoeff <- dtCoeff[l < u]
      
      for (i in 1:nrow(dtCoeff)) {
        l <- dtCoeff[i]$l
        u <- dtCoeff[i]$u
        dt1 <- dtCur[l <= Day & Day <= u]
        dt2 <- dtCur[!dt1, on = names(dtCur)]
        if (nrow(dt1) < 5 | nrow(dt2) < 5)
          next
        cTestCur <- as.list(chow.test(
          y1 = dt1$Mood,
          y2 = dt2$Mood,
          x1 = dt1[[activity]],
          x2 = dt2[[activity]]
        ))
        dtCoeff[i, F.value := ifelse(is.null(cTestCur$`F value`), NA, cTestCur$`F value`)]
        dtCoeff[i, p.value := ifelse(is.null(cTestCur$`P value`), NA, cTestCur$`P value`)]
      }
      
      p <- ggplot(dtCoeff, aes(x = l, y = u, fill = F.value)) +
        geom_tile() +
        scale_fill_gradient(low = "red", high = "green")
      ggplotly(p)
    })
    
    output$animation_demo <- renderPlotly({
      t <- seq(0, 2 * pi, length.out = 100)
      sig2 <- 0.001
      x <- rnorm(n = length(t) - 1, sd = sqrt(sig2))
      bm <- c(0, cumsum(x))
      bb <- (bm - t / (2 * pi) * bm)
      plot(t, bb)
      dt <- data.table()
      t1 <- t[1:10]
      for(i in 1:10) {
        tCur <- t[(10*(i-1)+1):(10*i)]
        x <- rnorm(n = length(t1) - 1, sd = sqrt(sig2))
        bm <- c(0, cumsum(x))
        bb <- (bm - t1 / (2 * pi / 10) * bm)
        dt <- rbindlist(list(dt, data.table(t = tCur, bb = bb)))
      }
      
      dt[, const := 1]
      dt <- merge(dt, data.table(const = 1, j = 0:7), by = "const", all = TRUE, allow.cartesian = TRUE)
      dt[, size := ifelse(j == 0, 20, 10)]
      dt[, x2 := cos(t + 2 * pi * j / 8)]
      dt[, y2 := sin(t + 2 * pi * j / 8)]
      dt[, x0 := cos(t)]
      dt[, y0 := sin(t)]
      dt[, x := x2 * (bb+0.5)]
      dt[, y := y2 * (bb+0.5)]
      dt[, x1 := x2 * (cos(t*5) / 10 + 1)]
      dt[, y1 := y2 * (cos(t*5) / 10 + 1)]
      p <- ggplot(data = dt, aes(x2, y2, size = size)) +
        geom_point(aes(frame = t)) +
        xlim(c(-2, 2)) +
        ylim(c(-2, 2)) +
        theme_minimal() +
        scale_size_continuous(range = c(10, 20))
      
      p <- ggplotly(p)
      animation_opts(p, frame = 2)
    })
    
    output$chord_diagram <- renderChorddiag({
      M <- length(ACTIVITIES)
      dtCur <- DATA[Mood == input$mood]
      mat <- matrix(0, nrow = (M+1), ncol = (M+1))
      rownames(mat) <- colnames(mat) <- c("Mood", ACTIVITIES)
      mat[1,1] <- nrow(dtCur) / nrow(DATA)
      for(i in 1:M) {
        mat[i+1,1] <- nrow(dtCur[get(ACTIVITIES[i]) > 0]) / nrow(DATA[get(ACTIVITIES[i]) > 0])
      }
      chorddiag(mat, showTicks = FALSE, precision = 2, groupnameFontsize = 14, groupThickness = 0.2)
      #chorddiag(MAT_COR)
    })
    
    output$dependencies <- renderRHandsontable({
      tbl <- rhandsontable(dtDependencies)
      tbl
    })
    
    output$cycles_heatmap <- renderPlotly({
      activity <- input$activity_cycles
      cycleLength <- input$slider_cycles
      dtCycles <- DATA[, c("Day", activity), with = FALSE]
      
      minDate <- min(dtCycles$Day)
      offset <- as.integer(minDate) %% cycleLength
      dtCycles[, `day of cycle` := (as.integer(Day) - offset) %% cycleLength]
      dtCycles[, `cycle` := floor(as.integer((Day - minDate)) / cycleLength)]
      
      activityName <- as.name(activity)
      p <- ggplot(
        data = dtCycles,
        mapping = eval(bquote(aes(
          x = `day of cycle`,
          y = cycle,
          fill = .(activityName),
          text = paste0(
            "Cycle: ", `cycle`, "<br>",
            "Day of Cycle: ", `day of cycle`, "<br>",
            "Date: ", Day, "<br>",
            activity, ": ", get(activity)
          )
        )))
      ) +
        geom_tile() +
        scale_fill_gradient(low = "red", high = "green") +
        scale_x_continuous() +
        scale_y_reverse()
      p <- ggplotly(p, tooltip = "text")
      p
    })
    
    output$autocorrelation_plot <- renderPlotly({
      autoCorrelation <- stats::acf(DATA[[input$activity]], plot = FALSE)
      dtAutoCorrelation <- data.table(Lag = as.integer(autoCorrelation$lag), `Auto Correlation` = as.numeric(autoCorrelation$acf))[Lag > 0]
      p <- ggplot(dtAutoCorrelation, aes(Lag, `Auto Correlation`)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_hline(yintercept = 0.1, color = "blue", linetype = "dashed") +
        geom_hline(yintercept = -0.1, color = "blue", linetype = "dashed") +
        geom_hline(yintercept = 0, color = "black") +
        theme_minimal() +
        ggtitle(paste0("Auto Correlation of '", input$activity,"'"))
      ggplotly(p)
    })
    
    output$partial_autocorrelation_plot <- renderPlotly({
      autoCorrelation <- stats::pacf(DATA[[input$activity]], plot = FALSE)
      dtAutoCorrelation <- data.table(Lag = as.integer(autoCorrelation$lag), `Auto Correlation` = as.numeric(autoCorrelation$acf))[Lag > 0]
      p <- ggplot(dtAutoCorrelation, aes(Lag, `Auto Correlation`)) +
        geom_bar(stat = "identity", width = 0.5) +
        geom_hline(yintercept = 0.1, color = "blue", linetype = "dashed") +
        geom_hline(yintercept = -0.1, color = "blue", linetype = "dashed") +
        geom_hline(yintercept = 0, color = "black") +
        theme_minimal() +
        ggtitle(paste0("Partial Auto Correlation of '", input$activity,"'"))
      ggplotly(p)
    })
    
})
