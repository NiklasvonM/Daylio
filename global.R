library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(lubridate)
library(rhandsontable)

fileName <- "Daten aufbereitet 2021-02-13"
DATA <- fread(paste0("data/", fileName, ".csv"), encoding = "UTF-8")
DATA[, Datum := as.Date(Datum)]
ACTIVITIES <- names(DATA)
ACTIVITIES <- ACTIVITIES[!ACTIVITIES %in% c("Datum", "Wochentag", "Stimmung")]
ACTIVITIES <- sort(ACTIVITIES)

DT_COR <- data.table(
  Aktivität = ACTIVITIES
)
for (activity in ACTIVITIES) {
  DT_COR[Aktivität == activity, Korrelation := round(cor(DATA$Stimmung, DATA[[activity]]), 2)]
  DT_COR[
    Aktivität == activity,
    `Durchschnittliche Stimmung mit Aktivität` := round(mean(DATA[get(activity) > 0, Stimmung], na.rm = TRUE), 2)
  ]
  DT_COR[
    Aktivität == activity,
    `Durchschnittliche Stimmung ohne Aktivität` := round(mean(DATA[get(activity) <= 0, Stimmung], na.rm = TRUE), 2)
  ]
  DT_COR[Aktivität == activity, `Summe Aktivität` := sum(DATA[[activity]], na.rm = TRUE)]
}

WOCHENTAGE <- c(
  "Montag",
  "Dienstag",
  "Mittwoch",
  "Donnerstag",
  "Freitag",
  "Samstag",
  "Sonntag"
)