library(visNetwork)
library(reshape2)
library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(lubridate)
library(rhandsontable)
library(dndselectr)

fileName <- "Daten aufbereitet 2021-12-08"
DATA <- fread(paste0("data/", fileName, ".csv"), encoding = "UTF-8")
DATA[, Datum := as.Date(Datum)]
DATA[, Monat := month(Datum)]
DATA[, `Wochentag Zahl` := wday(Datum, week_start = 1)]
DATA[, `Tag im Monat` := mday(Datum)]
ACTIVITIES <- names(DATA)
ACTIVITIES <- ACTIVITIES[!ACTIVITIES %in% c("Datum", "Wochentag", "Stimmung")]
ACTIVITIES <- sort(ACTIVITIES)
N <- nrow(DATA)

# correlation with Stimmung
DT_COR <- data.table(
  Aktivität = ACTIVITIES
)
for (activity in ACTIVITIES) {
  if(is.numeric(DATA[[activity]]))
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


# correlation of activities with each other with lag 1 for columns
MAT_COR_LAG <- matrix(data = 0, nrow = length(ACTIVITIES), ncol = length(ACTIVITIES))
colnames(MAT_COR_LAG) <- ACTIVITIES
rownames(MAT_COR_LAG) <- ACTIVITIES
for (i in ACTIVITIES) {
  for (j in ACTIVITIES) {
    MAT_COR_LAG[i, j] <- round(cor(DATA[1:(N-1)][[i]], DATA[2:N][[j]]), 2)
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



# Count number of appearences (using fun as a counting function)
# in a vector v of the last nLookback entries.
# The first nLookback entries of the result are always NA.
# Alternative for fun = sum:
# function(x) sum(x > 0)
countEntriesLookback <- function(v, nLookback = 14, fun = sum) {
  n <- length(v)
  if (n <= nLookback)
    return(rep_len(NA, length.out = n))
  res <- c(rep_len(NA, length.out = nLookback), vector("integer", length = n - nLookback))
  for(i in (nLookback+1):n) {
    res[i] <- fun(v[(i - nLookback):i], na.rm = TRUE)
  }
  return(res)
}


DT_ACTIVITY_LENGTH_DISTR <- as.data.table(expand.grid(
  Activity = ACTIVITIES,
  Days = 1:N
))
DT_ACTIVITY_LENGTH_DISTR[, `:=`(`n with activity` = 0, `n without activity` = 0)]

# for(activity in ACTIVITIES) {
#   cntWith <- 0
#   cntWithout <- 0
#   for(i in 1:N) {
#     activityValue <- DATA[i, get(activity)]
#     if (activityValue > 0) {
#       cntWith <- cntWith + 1
#       if (cntWithout > 0) {
#         DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWithout, `n without activity` := `n without activity` + 1]
#       }
#       cntWithout <- 0
#     } else {
#       cntWithout <- cntWithout + 1
#       if (cntWith > 0) {
#         DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWith, `n with activity` := `n with activity` + 1]
#       }
#       cntWith <- 0
#     }
#   }
#   if (cntWith > 0) {
#     DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWith, `n with activity` := `n with activity` + 1]
#   }
#   if (cntWithout > 0) {
#     DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWithout, `n without activity` := `n without activity` + 1]
#   }
# }




