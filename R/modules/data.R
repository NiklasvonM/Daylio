print("Preparing data for better app performance. This might take a minute...")

cleaned_file_name <- add_suffix_to_filename(FILENAME, "_cleaned")

if (!file.exists(cleaned_file_name)) {
  print(paste0("Processing ", FILENAME, "..."))
  process_data(FILENAME)
}

DATA <- fread(cleaned_file_name, encoding = "UTF-8")
DATA[, Day := as.Date(Day)]
setorder(DATA, "Day")


ACTIVITIES <- names(DATA)
ACTIVITIES <- ACTIVITIES[!ACTIVITIES %in% c("Day", "Weekday", "Mood", "Note")]
ACTIVITIES <- sort(ACTIVITIES)
ACTIVITIES_MOOD <- c(ACTIVITIES, "Mood")
DATA[, Month := month(Day)]
DATA[, `Weekday Number` := wday(Day, week_start = 1)]
DATA[, `Day of Month` := mday(Day)]
N <- nrow(DATA)


# correlation with mood
DT_COR <- data.table(
  Activity = ACTIVITIES
)
for (activity in ACTIVITIES) {
  if (is.numeric(DATA[[activity]]))
    DT_COR[Activity == activity, Correlation := round(measure_of_dependence(DATA$Mood, DATA[[activity]]), 2)]
  DT_COR[
    Activity == activity,
    `Average Mood with Activity` := round(mean(DATA[get(activity) > 0, Mood], na.rm = TRUE), 2)
  ]
  DT_COR[
    Activity == activity,
    `Average Mood without Activity` := round(mean(DATA[get(activity) <= 0, Mood], na.rm = TRUE), 2)
  ]
  DT_COR[Activity == activity, `Sum Activity` := sum(DATA[[activity]], na.rm = TRUE)]
  DT_COR[Activity == activity, `Average Activity` := mean(DATA[[activity]], na.rm = TRUE)]
}


# correlation of activities with each other
MAT_COR <- matrix(data = 0, nrow = length(ACTIVITIES_MOOD), ncol = length(ACTIVITIES_MOOD))
colnames(MAT_COR) <- ACTIVITIES_MOOD
rownames(MAT_COR) <- ACTIVITIES_MOOD
for (i in ACTIVITIES_MOOD) {
  for (j in ACTIVITIES_MOOD) {
    MAT_COR[i, j] <- round(measure_of_dependence(DATA[[i]], DATA[[j]]), 2)
  }
}


# correlation of activities with each other with lag 1 for columns
MAT_COR_LAG <- matrix(data = 0, nrow = length(ACTIVITIES_MOOD), ncol = length(ACTIVITIES_MOOD))
colnames(MAT_COR_LAG) <- ACTIVITIES_MOOD
rownames(MAT_COR_LAG) <- ACTIVITIES_MOOD
for (i in ACTIVITIES_MOOD) {
  for (j in ACTIVITIES_MOOD) {
    MAT_COR_LAG[i, j] <- round(measure_of_dependence(DATA[1:(N - 1)][[i]], DATA[2:N][[j]]), 2)
  }
}
