
dtDependencies <- data.table(Activity = ACTIVITIES, DependentActivities = "")
for(activity in dtDependencies$Activity) {
  dependentActivities <- NULL
  for(val in unique(DATA[[activity]])) {
    dtCur <- DATA[get(activity) == val]
    # Skip in case of too few observations
    if(nrow(dtCur) < 10)
      next
    for(dependentActivity in dtDependencies$Activity) {
      # Tautologically, an activity implies itself
      # Skip if the (potentially) dependent activity has little variance
      # because in that case, dependence is uninteresting.
      if(activity == dependentActivity | var(DATA[[dependentActivity]], na.rm = TRUE) < 0.1)
        next
      if(length(unique(dtCur[[dependentActivity]])) == 1) {
        dependentVal <- unique(dtCur[[dependentActivity]])
        dependentActivities <- c(dependentActivities, paste0(val, " -> ", dependentActivity, " ", dependentVal))
      }
    }
  }
  dtDependencies[Activity == activity, DependentActivities := paste0(dependentActivities, collapse = ", ")]
}
