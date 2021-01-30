

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
        dashboardHeader(),
        dashboardSidebar(
            selectInput(
                inputId = "activity",
                label = "Aktivitaet",
                choices = ACTIVITIES,
                multiple = FALSE
            )
        ),
        dashboardBody(
            plotlyOutput("date_dots"),
            valueBoxOutput("correlation")
        )
    )
)