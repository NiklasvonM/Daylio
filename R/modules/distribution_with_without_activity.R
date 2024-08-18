DT_ACTIVITY_LENGTH_DISTR <- as.data.table(expand.grid(
  Activity = ACTIVITIES,
  Days = 1:N
))
DT_ACTIVITY_LENGTH_DISTR[, `:=`(`n with activity` = 0, `n without activity` = 0)]

if (CALCULATE_DISTR_WWO_ACTIVITY) {
  for (activity in ACTIVITIES) {
    cnt_with <- 0
    cnt_without <- 0
    for (i in 1:N) {
      activity_value <- DATA[i, get(activity)]
      if (activity_value > 0) {
        cnt_with <- cnt_with + 1
        if (cnt_without > 0) {
          DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cnt_without, `n without activity` := `n without activity` + 1]
        }
        cnt_without <- 0
      } else {
        cnt_without <- cnt_without + 1
        if (cnt_with > 0) {
          DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cnt_with, `n with activity` := `n with activity` + 1]
        }
        cnt_with <- 0
      }
    }
    if (cnt_with > 0) {
      DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cnt_with, `n with activity` := `n with activity` + 1]
    }
    if (cnt_without > 0) {
      DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cnt_without, `n without activity` := `n without activity` + 1]
    }
  }
}
