
shinyServer(function(input, output) {

    cur_activity <- reactive({
        req(input$activity)
        input$activity
    })
    
    plot_mood <- reactive({
        activity <- cur_activity()
        dtPlot <- DATA[, c("Datum", "Wochentag", "Stimmung", activity), with = FALSE]
        dtPlot[, (activity) := as.ordered(get(activity))]
        dtPlot[, RunningAvg7 := frollmean(Stimmung, 7, algo = "exact", align = "left")]
        dtPlot[, RunningAvg30 := frollmean(Stimmung, 30, algo = "exact", align = "left")]
        activity <- as.name(activity)
        p <- ggplot(
            data = dtPlot,
            
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
                        "30-tägiger Stimmungsdurchschnitt: ", round(RunningAvg30, 2)
                    )
                )))
            ) +
            geom_line(aes(x = Datum, y = RunningAvg7), color = "red", alpha = 0.6) +
            geom_line(aes(x = Datum, y = RunningAvg30), color = "blue", alpha = 0.6)
        p <- ggplotly(p, tooltip = "text")
        # p <- layout(
        #     p,
        #     xaxis = list(rangeslider = list()),
        #     yaxis = list(fixedrange = FALSE)
        # )
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
                x = Monatstag,
                y = Monat,
                fill = Stimmung,
                text = paste0(
                    "Tag: ", Tag, "<br>",
                    "Durchschnittliche Stimmung: ", Stimmung
                )
            )
        ) +
            geom_tile() +
            scale_fill_gradient(low = "red", high = "green") +
            scale_y_reverse(breaks = 1:12)
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
            geom_bar(stat = "identity") +
            ylab(activity)
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
        dt[, Anzahl := countEntriesLookback(get(input$activity), nLookback = input$lookback_pointplot_n)]
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
})
