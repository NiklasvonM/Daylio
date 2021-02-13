library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(lubridate)

DATA <- fread("Daten aufbereitet 2021-01-30.csv", encoding = "UTF-8")
DATA[, Datum := as.Date(Datum)]
ACTIVITIES <- names(DATA)
ACTIVITIES <- ACTIVITIES[!ACTIVITIES %in% c("Datum", "Wochentag", "Stimmung")]
ACTIVITIES <- sort(ACTIVITIES)

WOCHENTAGE <- c(
  "Montag",
  "Dienstag",
  "Mittwoch",
  "Donnerstag",
  "Freitag",
  "Samstag",
  "Sonntag"
)