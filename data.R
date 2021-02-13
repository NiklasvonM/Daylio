library(data.table)
library(lubridate)

fileName <- "daylio_export_2021_02_13.csv"
dtOrig <- fread(paste0("data/", fileName), encoding = "UTF-8")
dt <- dtOrig[, .(
  Datum = as.Date(full_date),
  Wochentag = weekday,
  Stimmung = mood,
  Activities = activities
)]
dt[, Stimmung := sapply(Stimmung, function(x) switch(x,
  "Super" = 5,
  "Gut" = 4,
  "Ok" = 3,
  "Schlecht" = 2,
  "Lausig" = 1
))]

activities <- paste0(dt$Activities, collapse = " | ")
activities <- base::strsplit(activities, split = " | ", fixed = TRUE)
activities <- unlist(activities)
activities <- unique(activities)
for (activity in activities) {
  dt[, (activity) := as.integer(grepl(activity, Activities))]
}
dt[, Activities := NULL]
dt[, Schlafqualitaet := (-1 * Schlecht + 1 * Gut) / (Schlecht + Mäßig + Gut)]
dt[, `Koerperliche Taetigkeiten` := Yoga + Sport + Laufen]
fwrite(dt, file = paste0("data/Daten aufbereitet ", today(), ".csv"), sep = ";")
