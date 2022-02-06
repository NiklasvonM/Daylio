#devtools::install_github("mattflor/chorddiag")
library(chorddiag)
library(gap) # chow.test structural break
library(visNetwork) # network visualization
library(reshape2)
library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard) # dashboard UI
library(lubridate) # date manipulation
library(rhandsontable) # interactive table (Excel-like)
#devtools::install_github("serenity-r/dndselectr")
library(dndselectr)
library(leaflet) # world map
library(leaflet.minicharts) # world map
library(magrittr) # pipe
library(jsonlite) # json reading

fileName <- 
  #"mockdata"
  "Daten aufbereitet 2022-01-18"
DATA <- fread(paste0("data/", fileName, ".csv"), encoding = "UTF-8")
DATA[, Datum := as.Date(Datum)]
ACTIVITIES <- names(DATA)
ACTIVITIES <- ACTIVITIES[!ACTIVITIES %in% c("Datum", "Wochentag", "Stimmung")]
ACTIVITIES <- sort(ACTIVITIES)
DATA[, Monat := month(Datum)]
DATA[, `Wochentag Zahl` := wday(Datum, week_start = 1)]
DATA[, `Tag im Monat` := mday(Datum)]
N <- nrow(DATA)

# correlation with Stimmung
DT_COR <- data.table(
  Aktivität = ACTIVITIES
)
for (activity in ACTIVITIES) {
  if(is.numeric(DATA[[activity]]))
    DT_COR[Aktivität == activity, Korrelation := round(cor(DATA$Stimmung, DATA[[activity]]), 2)]
  DT_COR[
    Aktivität == activity,
    `Durchschnittliche Stimmung mit Aktivität` := round(mean(DATA[get(activity) > 0, Stimmung], na.rm = TRUE), 2)
  ]
  DT_COR[
    Aktivität == activity,
    `Durchschnittliche Stimmung ohne Aktivität` := round(mean(DATA[get(activity) <= 0, Stimmung], na.rm = TRUE), 2)
  ]
  DT_COR[Aktivität == activity, `Summe Aktivität` := sum(DATA[[activity]], na.rm = TRUE)]
}

# correlation of activities with each other
MAT_COR <- matrix(data = 0, nrow = length(ACTIVITIES), ncol = length(ACTIVITIES))
colnames(MAT_COR) <- ACTIVITIES
rownames(MAT_COR) <- ACTIVITIES
for (i in ACTIVITIES) {
  for (j in ACTIVITIES) {
    MAT_COR[i, j] <- round(cor(DATA[[i]], DATA[[j]]), 2)
  }
}


# correlation of activities with each other with lag 1 for columns
MAT_COR_LAG <- matrix(data = 0, nrow = length(ACTIVITIES), ncol = length(ACTIVITIES))
colnames(MAT_COR_LAG) <- ACTIVITIES
rownames(MAT_COR_LAG) <- ACTIVITIES
for (i in ACTIVITIES) {
  for (j in ACTIVITIES) {
    MAT_COR_LAG[i, j] <- round(cor(DATA[1:(N-1)][[i]], DATA[2:N][[j]]), 2)
  }
}




WOCHENTAGE <- c(
  "Montag",
  "Dienstag",
  "Mittwoch",
  "Donnerstag",
  "Freitag",
  "Samstag",
  "Sonntag"
)



# Count number of appearences (using fun as a counting function)
# in a vector v of the last nLookback entries.
# The first nLookback entries of the result are always NA.
# Alternative for fun = sum:
# function(x) sum(x > 0)
countEntriesLookback <- function(v, nLookback = 14, fun = sum) {
  n <- length(v)
  if (n <= nLookback)
    return(rep_len(NA, length.out = n))
  res <- c(rep_len(NA, length.out = nLookback), vector("integer", length = n - nLookback))
  for(i in (nLookback+1):n) {
    res[i] <- fun(v[(i - nLookback):i], na.rm = TRUE)
  }
  return(res)
}


DT_ACTIVITY_LENGTH_DISTR <- as.data.table(expand.grid(
  Activity = ACTIVITIES,
  Days = 1:N
))
DT_ACTIVITY_LENGTH_DISTR[, `:=`(`n with activity` = 0, `n without activity` = 0)]

# for(activity in ACTIVITIES) {
#   cntWith <- 0
#   cntWithout <- 0
#   for(i in 1:N) {
#     activityValue <- DATA[i, get(activity)]
#     if (activityValue > 0) {
#       cntWith <- cntWith + 1
#       if (cntWithout > 0) {
#         DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWithout, `n without activity` := `n without activity` + 1]
#       }
#       cntWithout <- 0
#     } else {
#       cntWithout <- cntWithout + 1
#       if (cntWith > 0) {
#         DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWith, `n with activity` := `n with activity` + 1]
#       }
#       cntWith <- 0
#     }
#   }
#   if (cntWith > 0) {
#     DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWith, `n with activity` := `n with activity` + 1]
#   }
#   if (cntWithout > 0) {
#     DT_ACTIVITY_LENGTH_DISTR[Activity == activity & Days == cntWithout, `n without activity` := `n without activity` + 1]
#   }
# }


getLocationData <- function(directory) {
  getLocationDataSingleMonth <- function(fileName) {
    df0 <- fromJSON(fileName, flatten = FALSE)[[1]]$placeVisit
    dt0 <- as.data.table(df0)
    if(! "centerLatE7" %in% names(dt0))
      return(data.table())
    dt <- dt0[!is.na(centerLatE7), .(Latitude = location.latitudeE7,
                                     Longitude = location.longitudeE7,
                                     Adresse = location.address,
                                     Adressname = location.name,
                                     # TODO: Fix lazy hack (+3600): Time is off by one hour.
                                     Startzeit = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.startTimestampMs) / 1000 + 3600,
                                     Endzeit = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.endTimestampMs) / 1000 + 3600
    )]
    dt
  }
  
  dt <- Reduce(
    f = function(x, y) rbindlist(list(x, y)),
    x = lapply(X = list.files(path = directory, recursive = TRUE, full.names = TRUE),
               FUN = getLocationDataSingleMonth),
    init = data.table()
    #fromJSON("Semantic Location History/2021/2021_DECEMBER.json")[[1]]
  )
  
  dt[, DatumStart := as.Date(Startzeit)]
  dt[, DatumEnde := as.Date(Endzeit)]
  dt[, Latitude := Latitude / 10^7]
  dt[, Longitude := Longitude / 10^7]
  #View(dt[DatumStart != DatumEnde])
  minDate <- min(as.Date(dt$Startzeit))
  maxDate <- max(as.Date(dt$Endzeit))
  
  dtFill <- data.table(Datum = seq(minDate, maxDate, by = "days"))
  
  dtFilled <- dtFill[dt, on = .(Datum >= DatumStart, Datum <= DatumEnde), c(names(dt), "Datum"), with = FALSE]
  setorder(dtFilled, "Startzeit")
  #dtFilled[, I := .I, by = .(Startzeit)]
  dtFilled[, Datum := Datum + rowid(Startzeit) - 1]
  #dtFilled[, N := .N, by = .(Startzeit)]
  dtFilled[, AdresseFull := ifelse(is.na(Adresse), Adressname, paste0(Adressname, " (", Adresse, ")"))]
  
  meanLong <- mean(dtFilled$Longitude)
  meanLat <- mean(dtFilled$Latitude)
  dtFilled[, DistanceFromMeanLocation := sqrt((Longitude - meanLong)^2 + (Latitude - meanLat)^2)]
  dtFilled[, PLZ := stringr::str_extract(Adresse, stringr::regex("[0-9]{5}"))]
  # 4 digits and 2 letters for Dutch PLZ
  dtFilled[is.na(PLZ), PLZ := stringr::str_extract(Adresse, stringr::regex("[0-9]{4} [a-zA-Z]{2}"))]
  #View(DT_LOCATION[is.na(PLZ)])
  dtFilled
}

DT_LOCATION <- getLocationData("data/GoogleMaps/Semantic Location History/")
DATES_NO_LOCATION_DATA <- seq(min(DT_LOCATION$DatumStart), max(DT_LOCATION$DatumEnde), by = "days")
DATES_NO_LOCATION_DATA <- DATES_NO_LOCATION_DATA[!DATES_NO_LOCATION_DATA %in% DT_LOCATION$Datum]


PLZ_VISITED <- DT_LOCATION[!is.na(PLZ), .(
  Longitude = mean(Longitude),
  Latitude = mean(Latitude),
  TimesVisited = length(unique(Datum)),
  FirstVisit = min(Datum),
  LastVisit = max(Datum)
  ), by = .(PLZ)]

PLACES_VISITED <- DT_LOCATION[, .(
  Adressen = paste0(unique(Adresse), collapse = "; "),
  Adressname = paste0(unique(Adressname), collapse = "; "),
  AdresseFull = paste0(unique(AdresseFull), collapse = "; "),
  MinDate = min(Datum),
  MaxDate = max(Datum),
  DaysVisited = length(unique(Datum))
  ), by = .(Latitude, Longitude, DistanceFromMeanLocation)]



getMovementData <- function(directory) {
  getMovementSingleMonth <- function(fileName) {
    df0 <- fromJSON(fileName, flatten = FALSE)[[1]]$activitySegment
    dt0 <- as.data.table(df0)
    if(! "startLocation.latitudeE7" %in% names(dt0))
      return(data.table())
    if(! "distance" %in% names(dt0))
      dt0[, distance := NA]
    if(! "activityType" %in% names(dt0))
      dt0[, activityType := NA]
    dt <- dt0[!is.na(startLocation.latitudeE7), .(
      StartLatitude = startLocation.latitudeE7,
      StartLongitude = startLocation.longitudeE7,
      EndLatitude = endLocation.latitudeE7,
      EndLongitude = endLocation.longitudeE7,
      Distanz = distance,
      Fortbewegungsmittel = activityType,
      # TODO: Fix lazy hack (+3600): Time is off by one hour.
      Startzeit = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.startTimestampMs) / 1000 + 3600,
      Endzeit = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.endTimestampMs) / 1000 + 3600
    )]
    dt
  }
  
  dt <- Reduce(
    f = function(x, y) rbindlist(list(x, y)),
    x = lapply(X = list.files(path = directory, recursive = TRUE, full.names = TRUE),
               FUN = getMovementSingleMonth),
    init = data.table()
    #fromJSON("Semantic Location History/2021/2021_DECEMBER.json")[[1]]
  )
  
  dt[, DatumStart := as.Date(Startzeit)]
  dt[, DatumEnde := as.Date(Endzeit)]
  dt[, StartLatitude := StartLatitude / 10^7]
  dt[, StartLongitude := StartLongitude / 10^7]
  dt[, EndLatitude := EndLatitude / 10^7]
  dt[, EndLongitude := EndLongitude / 10^7]
  #View(dt[DatumStart != DatumEnde])
  minDate <- min(as.Date(dt$Startzeit))
  maxDate <- max(as.Date(dt$Endzeit))
  
  dtFill <- data.table(Datum = seq(minDate, maxDate, by = "days"))
  
  dtFilled <- dtFill[dt, on = .(Datum >= DatumStart, Datum <= DatumEnde), c(names(dt), "Datum"), with = FALSE]
  setorder(dtFilled, "Startzeit")
  #dtFilled[, I := .I, by = .(Startzeit)]
  dtFilled[, Datum := Datum + rowid(Startzeit) - 1]
  #dtFilled[, N := .N, by = .(Startzeit)]
  dtFilled
}

DT_MOVEMENT <- getMovementData("data/GoogleMaps/Semantic Location History/")

DT_MOVEMENT[, a := sin(pi / 180 * (EndLatitude - StartLatitude)/2)^2 + cos(pi / 180 * StartLatitude) * cos(pi / 180 * EndLatitude) * sin(pi / 180 * (EndLongitude-StartLongitude)/2)^2]
DT_MOVEMENT[, c := 2 * atan2(sqrt(a),sqrt(1-a))]

DT_MOVEMENT[, `Distance by LatLon` := 6371000 * c]




dtAllPlacesVisited <- unique(rbindlist(list(
  DT_LOCATION[, .(Latitude, Longitude, Zeit = Startzeit)],
  DT_LOCATION[, .(Latitude, Longitude, Zeit = Endzeit)],
  DT_MOVEMENT[, .(Latitude = StartLatitude, Longitude = StartLongitude, Zeit = Startzeit)],
  DT_MOVEMENT[, .(Latitude = EndLatitude, Longitude = EndLongitude, Zeit = Endzeit)]
)))
dtPlaceInformation <- unique(DT_LOCATION[
  !is.na(PLZ) | !is.na(Adresse),
  .(Adresse = Adresse[!is.na(Adresse)][1], Adressname = Adressname[!is.na(Adressname)][1], AdresseFull = AdresseFull[!is.na(AdresseFull)][1], PLZ = PLZ[!is.na(PLZ)][1]),
  by = .(Latitude, Longitude)])
dtFirstLastVisit <- dtAllPlacesVisited[, .(`Erster Besuch` = min(Zeit), `Letzter Besuch` = max(Zeit)), by = .(Latitude, Longitude)]

DT_ALL_PLACES_VISITED <- merge(unique(dtAllPlacesVisited[, .(Latitude, Longitude)]), dtPlaceInformation, by = c("Latitude", "Longitude"), all.x = TRUE) %>%
  merge(dtFirstLastVisit, by = c("Latitude", "Longitude"), all.x = TRUE)
DT_ALL_PLACES_VISITED[, Standortinformationen := ifelse(is.na(AdresseFull) & is.na(PLZ), 0, 1)]
DT_ALL_PLACES_VISITED[, Color := ifelse(Standortinformationen == 1, "blue", "black")]









