
shinyServer(function(input, output) {

    cur_activity <- reactive({
        req(input$activity)
        input$activity
    })
    
    output$date_dots <- renderPlotly({
        activity <- cur_activity()
        dtPlot <- DATA[, c("Datum", "Wochentag", "Stimmung", activity), with = FALSE]
        dtPlot[, (activity) := as.ordered(get(activity))]
        activity <- as.name(activity)
        p <- ggplot(
            data = dtPlot,
            eval(bquote(aes(
                x = Datum,
                y = Stimmung,
                color = .(activity)
            )))
        ) +
            geom_point() #+
            #geom_bar(stat = "identity")
        p <- ggplotly(p)
        p
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
        infoBox(
            title = "Durchschnittliche Stimmung mit/ohne Aktivität",
            value = 0
        )
    })

})
