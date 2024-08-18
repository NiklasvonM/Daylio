dt_dependencies <- data.table(Activity = ACTIVITIES, DependentActivities = "")
for (activity in dt_dependencies$Activity) {
  dependent_activities <- NULL
  for (val in unique(DATA[[activity]])) {
    dt_cur <- DATA[get(activity) == val]
    # Skip in case of too few observations
    if (nrow(dt_cur) < 10)
      next
      for (dependentActivity in dt_dependencies$Activity) {
        # Tautologically, an activity implies itself
        # Skip if the (potentially) dependent activity has little variance
        # because in that case, dependence is uninteresting.
        if (activity == dependentActivity | var(DATA[[dependentActivity]], na.rm = TRUE) < 0.1)
          next
          if (length(unique(dt_cur[[dependentActivity]])) == 1) {
            dependentVal <- unique(dt_cur[[dependentActivity]])
            dependent_activities <- c(dependent_activities, paste0(val, " -> ", dependentActivity, " ", dependentVal))
          }
      }
  }
  dt_dependencies[Activity == activity, DependentActivities := paste0(dependent_activities, collapse = ", ")]
}
