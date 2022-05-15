
shinyServer(function(input, output) {

    cur_activity <- reactive({
        req(input$activity)
        input$activity
    })
    
    plot_mood <- reactive({
        activity <- cur_activity()
        dtPlot <- DATA[, c("Datum", "Wochentag", "Stimmung", "Notiz", activity), with = FALSE]
        dtPlot[, (activity) := as.ordered(get(activity))]
        dtPlot[, RunningAvg7 := frollmean(Stimmung, 7, algo = "exact", align = "left")]
        dtPlot[, RunningAvg30 := frollmean(Stimmung, 30, algo = "exact", align = "left")]
        
        dtDailyPlacesVisitedHovertext <- DT_LOCATION[, .(HovertextOrte = paste0(
          paste0(gsub("\n", " ", unique(AdresseFull))),
          collapse = "<br>")), .(Datum)]
        dtPlot <- merge(dtPlot, dtDailyPlacesVisitedHovertext, all.x = TRUE, by = "Datum")
        
        activity <- as.name(activity)
        p <- ggplot(
            data = dtPlot
        ) +
            geom_point(
                mapping = eval(bquote(aes(
                    x = Datum,
                    y = Stimmung,
                    color = .(activity),
                    text = paste0(
                        "Datum: ", format(Datum, "%d.%m.%Y"), "<br>",
                        "Stimmung: ", sapply(Stimmung, function(x) switch(
                            as.character(x),
                            "1" = "Lausig",
                            "2" = "Schlecht",
                            "3" = "Ok",
                            "4" = "Gut",
                            "5" = "Super",
                            "NA"
                        )), "<br>",
                        activity, ": ", .(activity), "<br>",
                        "7-tägiger Stimmungsdurchschnitt: ", round(RunningAvg7, 2), "<br>",
                        "30-tägiger Stimmungsdurchschnitt: ", round(RunningAvg30, 2), "<br>",
                        ifelse(Notiz == "", "", "<br>"), Notiz, ifelse(Notiz == "", "", "<br><br>"),
                        'Besuchte Orte:<br>', HovertextOrte
                    )
                )))
            ) +
            geom_line(aes(x = Datum, y = RunningAvg7), color = "red", alpha = 0.6) +
            geom_line(aes(x = Datum, y = RunningAvg30), color = "blue", alpha = 0.6)
        p <- ggplotly(p, tooltip = "text")
        p
    })
    
    output$date_dots <- renderPlotly({
        plot_mood()
    })
    
    output$correlation <- renderValueBox({
        activity <- cur_activity()
        correlation <- cor(DATA$Stimmung, DATA[[activity]])
        correlation <- round(correlation, 2)
        valueBox(
            value = correlation,
            subtitle = paste0("Korrelation Stimmung und ", activity)
        )
    })
    
    output$avg_mood <- renderInfoBox({
        activity <- cur_activity()
        valWith <- round(mean(DATA[get(activity) == 1, Stimmung], na.rm = TRUE), 2)
        valWithout <- round(mean(DATA[get(activity) == 0, Stimmung], na.rm = TRUE), 2)
        infoBox(
            title = "Durchschnittliche Stimmung mit/ohne Aktivität",
            value = paste(valWith, valWithout, sep = "/")
        )
    })
    
    output$roll_sum_activity <- renderPlotly({
      activity <- input$activity
      dtPlot <- DATA[, c("Datum", activity), with = FALSE]
      setorder(dtPlot, "Datum")
      dtPlot[, `14 Tage rollierende Summe` := frollsum(get(activity), n = 14)]
      # https://www.cedricscherer.com/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/
      ggplot(dtPlot, aes(Datum, `14 Tage rollierende Summe`)) +
        geom_line(color="#69b3a2", size=2) +
        geom_point(size = 5) +
        geom_point(
          aes(
            color = `14 Tage rollierende Summe`,
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
        dtPlot <- DATA[, .(Datum, Stimmung)]
        dtPlot[, Tag := format(Datum, "%d.%m.")]
        dtPlot <- dtPlot[, .(
            Stimmung = ifelse(length(Stimmung) > 0, mean(Stimmung, na.rm = TRUE), NA)
        ), by = .(Tag)]
        # Merge all possible days
        dtMerge <- data.table(Tag = seq(as.Date("2004-01-01"), as.Date("2004-12-31"), by = "days"))
        dtMerge[, Monatstag := mday(Tag)]
        dtMerge[, Monat := month(Tag)]
        dtMerge[, Tag := format(Tag, format = "%d.%m.")]
        dtPlot <- merge(dtPlot, dtMerge, all.y = TRUE, by = c("Tag"))
        
        p <- ggplot(
            data = dtPlot,
            mapping = aes(
                x = Monat,
                y = Monatstag,
                #x = Monatstag,
                #y = Monat,
                fill = Stimmung,
                text = paste0(
                    "Tag: ", Tag, "<br>",
                    "Durchschnittliche Stimmung: ", Stimmung
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
    
    output$wochentag_hist <- renderPlotly({
        req(input$activity)
        activity <- input$activity
        dtPlot <- DATA[, c("Wochentag", activity), with = FALSE]
        dtPlot[, Wochentag := factor(Wochentag, levels = WOCHENTAGE)]
        setnames(dtPlot, activity, "TempColumnName")
        dtPlot <- dtPlot[, .(TempColumnName = sum(TempColumnName, na.rm = TRUE)), by = .(Wochentag)]
        setnames(dtPlot, "TempColumnName", activity)
        p <-
            ggplot(
                dtPlot,
                eval(bquote(aes(
                    x = Wochentag,
                    y = get(activity),
                    text = paste0(
                        "Wochentag: ", Wochentag, "<br>",
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
        Aktivitaet = rownames(MAT_COR)[row(MAT_COR)],
        `Aktivitaet2` = colnames(MAT_COR)[col(MAT_COR)],
        KorrelationPlot = c(MAT_COR)
      )
      for(col in ACTIVITIES_MOOD) {
        maxCor <- max(abs(dtPlot[Aktivitaet == col & Aktivitaet != Aktivitaet2]$KorrelationPlot))
        dtPlot[Aktivitaet == col, Shape := as.factor(as.integer(abs(KorrelationPlot) == maxCor))]
      }
      dtPlot[, Korrelation := KorrelationPlot]
      dtPlot[!(input$correlationThreshold[1] <= Korrelation & Korrelation <= input$correlationThreshold[2]), KorrelationPlot := NA]
      dtPlot[Korrelation < 0, Farbe := -1]
      dtPlot[Korrelation > 0, Farbe := 1]
      dtPlot[is.na(Farbe), Farbe := 0]
      p <- ggplot(
        dtPlot,
        aes(
          Aktivitaet,
          Aktivitaet2,
          size = abs(KorrelationPlot),
          color = Farbe,
          shape = Shape,
          text = paste0(
            "Aktivität 1: ", Aktivitaet, "<br>",
            "Aktivität 2: ", Aktivitaet2, "<br>",
            "Korrelation: ", Korrelation
          )
        )
      ) +
        geom_point() +
        scale_color_continuous(low = "#3794bf", high = "#df8640") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab("Aktivität 1") +
        ylab("Aktivität 2") +
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
        Aktivitaet = rownames(MAT_COR_LAG)[row(MAT_COR_LAG)],
        `Aktivitaet_Lag_1` = colnames(MAT_COR_LAG)[col(MAT_COR_LAG)],
        KorrelationPlot = c(MAT_COR_LAG)
      )
      for(col in ACTIVITIES_MOOD) {
        maxCor <- max(abs(dtPlot[Aktivitaet == col & Aktivitaet != Aktivitaet_Lag_1]$KorrelationPlot))
        dtPlot[Aktivitaet == col, Shape := as.factor(as.integer(abs(KorrelationPlot) == maxCor))]
      }
      dtPlot[, Korrelation := KorrelationPlot]
      dtPlot[!(input$correlationThresholdLag[1] <= Korrelation & Korrelation <= input$correlationThresholdLag[2]), KorrelationPlot := NA]
      dtPlot[Korrelation < 0, Farbe := -1]
      dtPlot[Korrelation > 0, Farbe := 1]
      dtPlot[is.na(Farbe), Farbe := 0]
      p <- ggplot(
        dtPlot,
        aes(
          Aktivitaet,
          Aktivitaet_Lag_1,
          size = abs(KorrelationPlot),
          color = Farbe,
          shape = Shape,
          text = paste0(
            "Aktivität: ", Aktivitaet, "<br>",
            "Aktivität Folgetag: ", Aktivitaet_Lag_1, "<br>",
            "Korrelation: ", Korrelation
          )
        )
      ) +
        geom_point() +
        scale_color_continuous(low = "#3794bf", high = "#df8640") +
        theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        xlab("Aktivität") +
        ylab("Aktivität Folgetag") +
        scale_size_continuous(limits = c(0, 1))
      ggplotly(p, tooltip = "text")
    })
    
    output$single_day <- renderRHandsontable({
        date <- input$day
        tbl <- DATA[Datum == date]
        req(nrow(tbl) > 0)
        tbl <- melt.data.table(
            data = tbl,
            id.vars = character(0),
            measure.vars = names(tbl),
            variable.name = "Aktivität",
            value.name = "Wert",
            value.factor = TRUE
        )
        tbl[Aktivität == "Datum", Wert := format(as.Date(as.integer(Wert), origin = "1970-01-01"), format = "%d.%m.%Y")]
        rhandsontable(tbl)
    })
    
    output$lookback_pointplot <- renderPlotly({
        dt <- DATA[, c("Stimmung", input$activity), with = FALSE]
        dt[, Anzahl := countEntriesLookback(get(input$activity), nLookback = input$lookback_pointplot_n - 1)]
        dt <- dt[!is.na(Anzahl)]
        dt[, N := .N, by = c("Anzahl")]
        dtPlot <- dt[, .(Stimmung = mean(Stimmung, na.rm = TRUE)), by = c("Anzahl", "N")]
        p <- ggplot(dtPlot, aes(Anzahl, Stimmung)) +
            geom_point(aes(size = N)) +
            geom_line() +
            scale_y_continuous(limits = c(1, 5))
        p <- ggplotly(p)
        p
    })
    
    output$mood_distribution_by_activity_value <- renderPlotly({
      activity <- input$activity
      dtPlot <- DATA[, c("Stimmung", activity), with = FALSE]
      if (is.numeric(dtPlot[[activity]])) {
          dtPlot[, (activity) := ordered(as.character(get(activity)))]
      } else {
          dtPlot[, (activity) := factor(get(activity))]
      }
      dtPlot[, n_activity := .N, by = c(activity)]
      dtPlot[, Datenpunkte := .N, by = c("Stimmung", activity)]
      dtPlot <- unique(dtPlot)
      dtFill <- as.data.table(expand.grid(
          Stimmung = unique(dtPlot$Stimmung),
          V1 = unique(dtPlot[[activity]])
      ))
      setnames(dtFill, "V1", activity)
      dtPlot <- merge(dtPlot, dtFill, by = c("Stimmung", activity), all = TRUE)
      dtPlot[, `Stimmungsverteilung` := Datenpunkte / n_activity]
      dtPlot[is.na(Stimmungsverteilung), Stimmungsverteilung := 0]
      dtPlot[is.na(Datenpunkte), Datenpunkte := 0]
      if(activity == "Schlafqualitaet") {
        dtPlot[get(activity) == -1, (activity) := "Schlecht"]
        dtPlot[get(activity) == 0, (activity) := "Mäßig"]
        dtPlot[get(activity) == 1, (activity) := "Gut"]
      }
      # dtPlot <- DATA[, .(`Durchschnittsstimmung` = mean(Stimmung, na.rm = TRUE), N = .N), by = c(activity)]
      setnames(dtPlot, activity, make.names(activity))
      p <- ggplot(dtPlot, aes_string(x = "Stimmung", y = "Stimmungsverteilung", color = make.names(activity))) +
          geom_line() +
          geom_point(aes(size = Datenpunkte)) +
          scale_y_continuous(limits = c(0, 1)) +
          scale_x_continuous(limits = c(1, 5)) +
          scale_size_continuous(limits = c(0, max(dtPlot$n)), guide = "none") +
          theme_minimal()
      # p <- ggplot(dtPlot, aes_string(x = make.names(activity), y = "Durchschnittsstimmung")) +
      #   geom_line() +
      #   geom_point(aes(size = N)) +
      #   scale_y_continuous(limits = c(1, 5)) +
      #   scale_size_continuous(limits = c(0, max(dtPlot$N)))
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
      dtPlot <- dtCur[, .(Dichte = .N / nrow(dtCur)), by = .(Stimmung)]
      dtPlot <- merge(dtPlot, data.table(Stimmung = 1:5), by = c("Stimmung"), all = TRUE)
      dtPlot[is.na(Dichte), Dichte := 0]
      p <- ggplot(dtPlot, aes(Stimmung, Dichte, text = paste0(
        "Stimmung: ", Stimmung, "<br>",
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
      dtPlot <- DT_ACTIVITY_LENGTH_DISTR[Activity == activity, .(Tage = Days, `Anzahl` = `n with activity`)]
      
      # Estimate exponential distribution using maximum likelihood estimation
      lambda_mle <- 1/ (sum(dtPlot[, .(Tage * Anzahl)]) / sum(dtPlot$Anzahl))
      # bias corrected maximum likelihood estimation
      lambda_mle_bias_corrected <- lambda_mle - lambda_mle / (sum(dtPlot$Anzahl) - 1)
      dtPlot[, EstimatedDensity := dexp(Tage, rate = lambda_mle_bias_corrected) * sum(dtPlot$Anzahl)]
      
      maxDay <- max(dtPlot[Anzahl > 0]$Tage)
      dtPlot <- dtPlot[Tage <= maxDay]
      p <- ggplot(dtPlot, aes(Tage, Anzahl)) +
        geom_bar(stat = "identity") +
        geom_line(aes(y = EstimatedDensity)) +
        ggtitle(paste0("Verteilung der Anzahl aufeinanderfolgender Tage mit der Aktivität ", activity))
      ggplotly(p)
    })
    
    output$days_distr_wo_activity_plot <- renderPlotly({
      activity <- input$activity
      dtPlot <- DT_ACTIVITY_LENGTH_DISTR[Activity == activity, .(Tage = Days, `Anzahl` = `n without activity`)]
      
      # Estimate exponential distribution using maximum likelihood estimation
      lambda_mle <- 1/ (sum(dtPlot[, .(Tage * Anzahl)]) / sum(dtPlot$Anzahl))
      # bias corrected maximum likelihood estimation
      lambda_mle_bias_corrected <- lambda_mle - lambda_mle / (sum(dtPlot$Anzahl) - 1)
      dtPlot[, EstimatedDensity := dexp(Tage, rate = lambda_mle_bias_corrected) * sum(dtPlot$Anzahl)]
      
      maxDay <- max(dtPlot[Anzahl > 0]$Tage)
      dtPlot <- dtPlot[Tage <= maxDay]
      p <- ggplot(dtPlot, aes(Tage, Anzahl)) +
        geom_bar(aes(y = Anzahl), stat = "identity") +
        geom_line(aes(y = EstimatedDensity)) +
        ggtitle(paste0("Verteilung der Anzahl aufeinanderfolgender Tage ohne die Aktivität ", activity))
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
        DT_COR[, .(label = Aktivität, Korrelation)],
        all.x = TRUE, by = "label"
      )
      nodes[Korrelation < -0.05, group := -1]
      nodes[-0.05 <= Korrelation & Korrelation <= 0.05, group := 0]
      nodes[0.05 < Korrelation, group := 1]
      nodes[is.na(group), group := 99]
      
      nodes[, title := paste0(
        "Aktivität: ", label, "<br>",
        "Korrelation mit Stimmung: ", Korrelation
      )]
      
      if("ActivityGroup" %in% names(nodes)) {
        nodes[, title := paste0(
          title, "<br>",
          "Gruppe: ", ActivityGroup
        )]
      }
      
      
      visNetwork(nodes, links)
      
    })
    
    # world map with daily visits 
    output$worldmap <- renderLeaflet({
      
      dateSelected <- input$day
      
      basemap <- leaflet()  %>%
        addTiles()
        #addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE))
      
      dtMovement <- DT_MOVEMENT[Datum == dateSelected]
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
      
      
      dtLocations <- DT_LOCATION[Datum == dateSelected]
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
      
      dtCur <- DATA[, c("Datum", activity, "Stimmung"), with = FALSE]
      
      dtCoeff <- as.data.table(expand.grid(
        l = DATA[mday(Datum) == 1]$Datum, u = DATA[mday(Datum) == 1]$Datum
      ))
      dtCoeff <- dtCoeff[l < u]
      
      for (i in 1:nrow(dtCoeff)) {
        l <- dtCoeff[i]$l
        u <- dtCoeff[i]$u
        dt1 <- dtCur[l <= Datum & Datum <= u]
        dt2 <- dtCur[!dt1, on = names(dtCur)]
        if (nrow(dt1) < 5 | nrow(dt2) < 5)
          next
        cTestCur <- as.list(chow.test(
          y1 = dt1$Stimmung,
          y2 = dt2$Stimmung,
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
      dtCur <- DATA[Stimmung == input$mood]
      mat <- matrix(0, nrow = (M+1), ncol = (M+1))
      rownames(mat) <- colnames(mat) <- c("Stimmung", ACTIVITIES)
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
    
})
