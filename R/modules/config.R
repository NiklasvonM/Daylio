if (!file.exists("config/config.yaml")) {
  print("config.yaml does not seem to exist. Please copy config_template.yaml and follow the README.")
}
CONFIG <- read_yaml("config/config.yaml")

# one of "de" and "en"
LANGUAGE <- CONFIG$LANGUAGE

if (!LANGUAGE %in% c("en", "de")) {
  stop("LANGUAGE in config.R unknown.")
}

FILENAME <- CONFIG$FILENAME

# Whether or not to calculate the distribution with and without an activity.
# Currently very slow. Only set to TRUE if you want to view that graph.
CALCULATE_DISTR_WWO_ACTIVITY <- FALSE
