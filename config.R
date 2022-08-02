# Whether or not to calculate the distribution with and without an activity.
# Currently very slow. Only set to TRUE if you want to view that graph.
CALCULATE_DISTR_WWO_ACTIVITY <- TRUE

# one of "de" and "en"
LANGUAGE <- "en"

if(!LANGUAGE %in% c("en", "de")) {
  stop("LANGUAGE in config.R unknown.")
}

fileName <- 
  "mockdata"
  #"data cleaned 2022-08-02"
