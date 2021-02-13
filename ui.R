

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
                menuItem("Korrelationstabelle", tabName = "correlation_table", icon = icon("cannabis"))
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
                    rHandsontableOutput("correlation_table"))
            )
        )
    )
)