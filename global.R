library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(lubridate)
library(rhandsontable)

fileName <- "Daten aufbereitet 2021-07-18"
DATA <- fread(paste0("data/", fileName, ".csv"), encoding = "UTF-8")
DATA[, Datum := as.Date(Datum)]
ACTIVITIES <- names(DATA)
ACTIVITIES <- ACTIVITIES[!ACTIVITIES %in% c("Datum", "Wochentag", "Stimmung")]
ACTIVITIES <- sort(ACTIVITIES)

# correlation with Stimmung
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

# correlation of activities with each other
MAT_COR <- matrix(data = 0, nrow = length(ACTIVITIES), ncol = length(ACTIVITIES))
colnames(MAT_COR) <- ACTIVITIES
rownames(MAT_COR) <- ACTIVITIES
for (i in ACTIVITIES) {
  for (j in ACTIVITIES) {
    MAT_COR[i, j] <- round(cor(DATA[[i]], DATA[[j]]), 2)
  }
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