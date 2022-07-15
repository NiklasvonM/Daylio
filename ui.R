
shinyUI(
    dashboardPage(
        dashboardHeader(
        ),
        dashboardSidebar(
            selectInput(
                inputId = "activity",
                label = "Aktivitaet",
                choices = ACTIVITIES,
                multiple = FALSE,
                selectize = FALSE,
                size = min(length(ACTIVITIES), 15)
            ),
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard"),
                menuItem("Day of year", tabName = "jahrestag", icon = icon("th")),
                menuItem("Day of year acitvity", tabName = "day_of_year_activity"),
                menuItem("Weekday", tabName = "wochentag", icon = icon("calendar-alt")),
                menuItem("Correlation table", tabName = "correlation_table"),
                menuItem("Correlation matrix", tabName = "correlation_matrix"),
                menuItem("Correlation matrix lag 1", tabName = "correlation_matrix_lag"),
                menuItem("Single day", tabName = "single_day"),
                menuItem("Number activities foredays", tabName = "lookback"),
                menuItem("Mood by activity value", tabName = "mood_distribution_by_activity_value"),
                menuItem("Distribution mood with/without activity", tabName = "mood_distribution_wwo_activity"),
                menuItem("Distribution day with/without activity", tabName = "days_distr_wwo_activity"),
                menuItem("Network", tabName = "network"),
                menuItem("Location data", tabName = "worldmap"),
                menuItem("Visited places (coarse)", tabName = "plz_visited"),
                menuItem("Visited places (fine)", tabName = "places_visited"),
                menuItem("Structural break", tabName = "strucutral_break_test"),
                #menuItem("Animationsdemo", tabName = "animation"),
                menuItem("Accord diagram", tabName = "chord_diagram"),
                menuItem("Dependencies", tabName = "dependencies"),
                menuItem("Cycles", tabName = "cycles"),
                width = "400px"
            ),
            downloadButton("download"),
            dateInput(
                "day",
                "Tag",
                format = "dd.mm.yyyy",
                value = today() - 1,
                max = today(),
                language = "de",
                datesdisabled = DATES_NO_LOCATION_DATA # currently not working
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                    fluidPage(
                        fluidRow(
                            plotlyOutput("date_dots")
                        ),
                        fluidRow(
                            valueBoxOutput("correlation"),
                            infoBoxOutput("avg_mood")
                        ),
                        fluidRow(
                            plotlyOutput("roll_sum_activity")   
                        )
                    )
                ),
                
                tabItem(tabName = "jahrestag",
                    shiny::h2("Durchschnittliche Stimmung nach Jahrestag"),
                    plotlyOutput("year_heatmap")
                ),
                tabItem(tabName = "day_of_year_activity",
                        shiny::h2(""),
                        plotlyOutput("day_of_year_activity_plot")
                ),
                tabItem(tabName = "wochentag",
                    shiny::h2("Vorkommnisse während der Woche"),
                    plotlyOutput("wochentag_hist")
                ),
                tabItem(tabName = "correlation_table",
                    shiny::h2("Korrelation Aktivitäten und Stimmung"),
                    rHandsontableOutput("correlation_table")
                ),
                tabItem(tabName = "correlation_matrix",
                    shiny::h2("Korrelation der einzelnen Aktivitäten"),
                    plotlyOutput("correlation_matrix_plot", height = "1200px"),
                    sliderInput("correlationThreshold",
                                "Mindestkorrelation",
                                min = -1, max = 1, value = c(-1, 1), step = 0.01),
                    #dataTableOutput("correlation_matrix")
                    rHandsontableOutput("correlation_matrix")
                ),
                tabItem(tabName = "correlation_matrix_lag",
                        shiny::h2("Korrelation der einzelnen Aktivitäten"),
                        plotlyOutput("correlation_matrix_lag_plot", height = "1200px"),
                        sliderInput("correlationThresholdLag",
                                    "Mindestkorrelation",
                                    min = -1, max = 1, value = c(-1, 1), step = 0.01),
                        h3("Spalte repräsentiert Folgetag"),
                        #dataTableOutput("correlation_matrix")
                        rHandsontableOutput("correlation_matrix_lag")
                ),
                tabItem(tabName = "single_day",
                    shiny::h2("Was ist an diesem Tag passiert?"),
                    rHandsontableOutput("single_day")
                    #dataTableOutput("single_day")
                ),
                tabItem(tabName = "lookback",
                    shiny::h2("Durchschnittliche Stimmung nach Aktivitäten in den Vortagen"),
                    plotlyOutput("lookback_pointplot"),
                    sliderInput("lookback_pointplot_n", "Anzahl Tage: ", min = 1, max = 365, step = 1, value = 14)
                ),
                tabItem(tabName = "mood_distribution_by_activity_value",
                    shiny::h2("Stimmungsverteilung nach Aktivitätswert"),
                    plotlyOutput("mood_distribution_by_activity_value")
                ),
                tabItem(tabName = "mood_distribution_wwo_activity",
                    shiny::h2("Stimmungsverteilung mit oder ohne gewisse Aktivitäten"),
                    fluidRow(
                        plotlyOutput("mood_distribution_wwo_activity")
                    ),
                    fluidRow(
                        column(6,
                               h3("Einschließen"),
                               dropZoneInput("dropzone_w",
                                             choices = ACTIVITIES
                               )
                        ),
                        column(6,
                               h3("Ausschließen"),
                               dropZoneInput("dropzone_wo",
                                             choices = ACTIVITIES
                               )
                        )
                    ),
                    fluidRow(
                        column(6,
                               h3("Aktivitäten"),
                               dragZone("dragzone_activity",
                                        choices = ACTIVITIES
                               )
                        )
                    )
                ),
                tabItem(tabName = "days_distr_wwo_activity",
                    fluidRow(
                        plotlyOutput("days_distr_w_activity_plot")
                    ),
                    fluidRow(
                        plotlyOutput("days_distr_wo_activity_plot")
                    )
                ),
                tabItem(tabName = "network",
                      visNetworkOutput("forcenetwork", height = "1000px")  
                ),
                tabItem(tabName = "worldmap",
                        leafletOutput("worldmap", height = "1000px")
                ),
                tabItem(tabName = "plz_visited",
                        leafletOutput("plz_visited", height = "1000px"),
                        leafletOutput("plz_visited_timeline", height = "1000px")
                ),
                tabItem(tabName = "places_visited",
                        leafletOutput("places_visited", height = "1000px")
                ),
                tabItem(tabName = "strucutral_break_test",
                        plotlyOutput("strucutral_break_test", height = "800px")
                ),
                tabItem(tabName = "animation",
                        plotlyOutput("animation_demo", height = "1000px")
                ),
                tabItem(tabName = "chord_diagram",
                        shiny::h2("Welcher Anteil der Aktivität tritt bei dieser Stimmung auf?"),
                        selectInput("mood", "Stimmung", choices = 1:5),
                        chorddiagOutput("chord_diagram", width = "80%", height = "1000px")
                ),
                tabItem(tabName = "dependencies",
                        rHandsontableOutput("dependencies")
                ),
                tabItem(tabName = "cycles",
                        selectInput(
                          inputId = "activity_cycles",
                          label = "Aktivitaet",
                          choices = ACTIVITIES_MOOD,
                          multiple = FALSE,
                          selectize = FALSE,
                          size = min(length(ACTIVITIES_MOOD), 15)
                        ),
                        sliderInput(
                          "slider_cycles", 
                          label = "Cycle Length",
                          min = 1,
                          max = 50,
                          value = 30,
                          step = 1
                        ),
                        plotlyOutput("cycles_heatmap")
                )
            )
        )
    )
)