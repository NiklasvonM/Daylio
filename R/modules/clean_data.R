process_data <- function(file_name) {

  dt_orig <- fread(file_name, encoding = "UTF-8")
  dt <- dt_orig[, .(
    Day = as.Date(full_date),
    Weekday = weekday,
    Mood = mood,
    Activities = activities,
    Note = note
  )]

  # Map mood descriptions to numbers, e.g. awful -> 1, bad -> 2, ..., rad -> 5.
  dt_mood_names_try <- data.table(
    "1" = c("Lausig", "awful"),
    "2" = c("Schlecht", "bad"),
    "3" = c("Ok", "meh"),
    "4" = c("Gut", "good"),
    "5" = c("Super", "rad")
  )
  mood_names_found <- FALSE
  for (i in seq_len(nrow(dt_mood_names_try))) {
    mood_names_cur <- as.character(dt_mood_names_try[i])
    if (all(dt$Mood %in% mood_names_cur)) {
      dt[, Mood := match(Mood, mood_names_cur)]
      mood_names_found <- TRUE
      break
    }
  }
  if (!mood_names_found) {
    warning("Mood names where not found!")
  }


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
  if (all(c("Schlecht", "Mäßig", "Gut") %in% names(dt))) {
    dt[, Schlafqualitaet := (-1 * Schlecht + 1 * Gut) / (Schlecht + Mäßig + Gut)]
  }
  if (all(c("Yoga", "Sport", "Laufen", "Krankengymnastik Übung", "Schwimmen") %in% names(dt))) {
    dt[, `Koerperliche Taetigkeiten` := Yoga + Sport + Laufen + `Krankengymnastik Übung` + Schwimmen]
  }
  fwrite(dt, file = paste0("data/", file_name, "_cleaned", ".csv"), sep = ";")
}
