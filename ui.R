

# shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("Daylio"),
# 
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(
#                 inputId = "activity",
#                 label = "Aktivitaet",
#                 choices = ACTIVITIES,
#                 multiple = FALSE
#             )
#         ),
# 
#         mainPanel(
#             plotlyOutput("date_dots")
#         )
#     )
# ))

shinyUI(
    dashboardPage(
        dashboardHeader(
        ),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Jahrestag", tabName = "jahrestag", icon = icon("th")),
                menuItem("Wochentag", tabName = "wochentag", icon = icon("calendar-alt")),
                menuItem("Korrelationstabelle", tabName = "correlation_table", icon = icon("cannabis")),
                menuItem("Korrelationsmatrix", tabName = "correlation_matrix", icon = icon("cannabis")),
                menuItem("Korrelationsmatrix Lag 1", tabName = "correlation_matrix_lag", icon = icon("cannabis")),
                menuItem("Einzelne Tage", tabName = "single_day", icon = icon("calendar")),
                menuItem("Aktivitätenanzahl Vortage", tabName = "lookback", icon = icon("cannabis")),
                menuItem("Stimmung nach Aktivitätswert", tabName = "mood_distribution_by_activity_value", icon = icon("cannabis")),
                menuItem("Verteilung Stimmung mit/ohne Aktivitäten", tabName = "mood_distribution_wwo_activity", icon = icon("cannabis")),
                menuItem("Verteilung Tage mit/ohne Aktivität", tabName = "days_distr_wwo_activity"),
                menuItem("Netzwerk", tabName = "network")
            ),
            selectInput(
                inputId = "activity",
                label = "Aktivitaet",
                choices = ACTIVITIES,
                multiple = FALSE
            ),
            downloadButton("download")
        ),
        dashboardBody(
            tabItems(
                # First tab content
                tabItem(tabName = "dashboard",
                    fluidPage(
                        plotlyOutput("date_dots"),
                        valueBoxOutput("correlation"),
                        infoBoxOutput("avg_mood")
                    )
                ),
                
                # Second tab content
                tabItem(tabName = "jahrestag",
                    h2("Durchschnittliche Stimmung nach Jahrestag"),
                    plotlyOutput("year_heatmap")
                ),
                tabItem(tabName = "wochentag",
                    h2("Vorkommnisse während der Woche"),
                    plotlyOutput("wochentag_hist")
                ),
                tabItem(tabName = "correlation_table",
                    h2("Korrelation Aktivitäten und Stimmung"),
                    rHandsontableOutput("correlation_table")
                ),
                tabItem(tabName = "correlation_matrix",
                    h2("Korrelation der einzelnen Aktivitäten"),
                    plotlyOutput("correlation_matrix_plot", height = "1200px"),
                    sliderInput("correlationThreshold",
                                "Mindestkorrelation",
                                min = -1, max = 1, value = c(-1, 1), step = 0.01),
                    #dataTableOutput("correlation_matrix")
                    rHandsontableOutput("correlation_matrix")
                ),
                tabItem(tabName = "correlation_matrix_lag",
                        h2("Korrelation der einzelnen Aktivitäten"),
                        plotlyOutput("correlation_matrix_lag_plot", height = "1200px"),
                        sliderInput("correlationThresholdLag",
                                    "Mindestkorrelation",
                                    min = -1, max = 1, value = c(-1, 1), step = 0.01),
                        h3("Spalte repräsentiert Folgetag"),
                        #dataTableOutput("correlation_matrix")
                        rHandsontableOutput("correlation_matrix_lag")
                ),
                tabItem(tabName = "single_day",
                    h2("Was ist an diesem Tag passiert?"),
                    dateInput(
                        "day",
                        "Tag",
                        format = "dd.mm.yyyy",
                        value = today() - 1,
                        language = "de"
                    ),
                    rHandsontableOutput("single_day")
                    #dataTableOutput("single_day")
                ),
                tabItem(tabName = "lookback",
                    h2("Durchschnittliche Stimmung nach Aktivitäten in den Vortagen"),
                    plotlyOutput("lookback_pointplot"),
                    sliderInput("lookback_pointplot_n", "Anzahl Tage: ", min = 1, max = 365, step = 1, value = 14)
                ),
                tabItem(tabName = "mood_distribution_by_activity_value",
                    h2("Durchschnittliche Stimmung nach Aktivitätswert"),
                    plotlyOutput("mood_distribution_by_activity_value")
                ),
                tabItem(tabName = "mood_distribution_wwo_activity",
                    fluidRow(
                        plotlyOutput("mood_distribution_wwo_activity")
                    ),
                    fluidRow(
                        column(4,
                               h3("Aktivitäten"),
                               dragZone("dragzone_activity",
                                        choices = ACTIVITIES
                               )
                        ),
                        column(4,
                               h3("Einschließen"),
                               dropZoneInput("dropzone_w",
                                             choices = ACTIVITIES
                               )
                        ),
                        column(4,
                               h3("Ausschließen"),
                               dropZoneInput("dropzone_wo",
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
                      forceNetworkOutput("forcenetwork", height = "1000px")  
                )
            )
        )
    )
)