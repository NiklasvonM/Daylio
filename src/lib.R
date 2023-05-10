print("Loading and installing packages...")
if(!require("pacman")) {
  install.packages("pacman")
  library("pacman")
}
libraries <- c(
  "devtools", # installing from GitHub
  "yaml", # read the config file
  "gap", # chow.test structural break
  "visNetwork", # network visualization
  "reshape2",
  "data.table", # efficient data manipulation
  "ggplot2", # plotting
  "plotly", # interactive plotting
  "shiny", # interactive app
  "shinydashboard", # builds on top of shiny
  "lubridate", # dates
  "rhandsontable", # Excel-like tables in shiny
  "leaflet", # world map
  "leaflet.minicharts",
  "leaftime", # animated map
  "magrittr", # piping
  "jsonlite",
  "scico", # color palettes
  "ggdark", # color palettes
  "DataExplorer", # data exploration report
  "tm", # stopwords
  "wordcloud2", # word cloud
  "stringr" # string manipulation
)

# chord diagram
if(!require("chorddiag")) {
  devtools::install_github("mattflor/chorddiag")
  library(chorddiag)
}
if(!require("dndselectr")) {
  devtools::install_github("serenity-r/dndselectr")
  library(dndselectr)
}
