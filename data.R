library(data.table)
library(lubridate)

fileName <- "daylio_export_2021_10_23.csv"
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
# There's some work to be done to ensure that activities are not mixed up with
# ones that contain their respective name (e.gl "Krank" and "Krankengymnastik").
# Therefore, we check for that activity plus the seperation sign (e.g. "Krank |")
# or for that activity ("Krank") but no further sign ("Krank." where the dot 
# represents any character). This is the case for the last activity in the row.
for (activity in activities) {
  dt[, (activity) := as.integer(
    grepl(paste0(activity, " |"), Activities, fixed = TRUE) |
    (
      grepl(activity, Activities, fixed = TRUE) &
      !grepl(paste0(activity, "."), Activities, fixed = FALSE)
    )
  )]
}
dt[, Activities := NULL]
dt[, Schlafqualitaet := (-1 * Schlecht + 1 * Gut) / (Schlecht + Mäßig + Gut)]
dt[, `Koerperliche Taetigkeiten` := Yoga + Sport + Laufen]
fwrite(dt, file = paste0("data/Daten aufbereitet ", today(), ".csv"), sep = ";")
