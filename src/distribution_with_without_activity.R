

DT_ACTIVITY_LENGTH_DISTR <- as.data.table(expand.grid(
  Activity = ACTIVITIES,
  Days = 1:N
))
DT_ACTIVITY_LENGTH_DISTR[, `:=`(`n with activity` = 0, `n without activity` = 0)]

if(CALCULATE_DISTR_WWO_ACTIVITY) {
  for(activity in ACTIVITIES) {
    cntWith <- 0
    cntWithout <- 0
    for(i in 1:N) {
      activityValue <- DATA[i, get(activity)]
      if (activityValue > 0) {
        cntWith <- cntWith + 1
        if (cntWithout > 0) {
          DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWithout, `n without activity` := `n without activity` + 1]
        }
        cntWithout <- 0
      } else {
        cntWithout <- cntWithout + 1
        if (cntWith > 0) {
          DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWith, `n with activity` := `n with activity` + 1]
        }
        cntWith <- 0
      }
    }
    if (cntWith > 0) {
      DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWith, `n with activity` := `n with activity` + 1]
    }
    if (cntWithout > 0) {
      DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWithout, `n without activity` := `n without activity` + 1]
    }
  }
}
