

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
                menuItem("Widgets", tabName = "widgets", icon = icon("th"))
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
                tabItem(tabName = "widgets",
                    h2("Durchschnittliche Stimmung nach Jahrestag"),
                    plotlyOutput("year_heatmap")
                )
            )
        )
    )
)