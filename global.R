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
library(leaftime) # animated map
library(magrittr) # pipe
library(jsonlite) # json reading
library(scico) # scale_color_scico
library(ggdark)
library(DataExplorer)
source("config.R")

DATA <- fread(paste0("data/", fileName, ".csv"), encoding = "UTF-8")
DATA[, Day := as.Date(Day)]


ACTIVITIES <- names(DATA)
ACTIVITIES <- ACTIVITIES[!ACTIVITIES %in% c("Day", "Weekday", "Mood", "Notiz")]
ACTIVITIES <- sort(ACTIVITIES)
ACTIVITIES_MOOD <- c(ACTIVITIES, "Mood")
DATA[, Month := month(Day)]
DATA[, `Weekday Number` := wday(Day, week_start = 1)]
DATA[, `Day of Month` := mday(Day)]
N <- nrow(DATA)


# DataExplorer::create_report(
#   DATA,
#   output_file = "report.html",
#   report_title = "Daylio Report",
#   y = "Mood"
# )


# correlation with mood
DT_COR <- data.table(
  Activity = ACTIVITIES
)
for (activity in ACTIVITIES) {
  if(is.numeric(DATA[[activity]]))
    DT_COR[Activity == activity, Correlation := round(cor(DATA$Mood, DATA[[activity]]), 2)]
  DT_COR[
    Activity == activity,
    `Average Mood with Activity` := round(mean(DATA[get(activity) > 0, Mood], na.rm = TRUE), 2)
  ]
  DT_COR[
    Activity == activity,
    `Average Mood without Activity` := round(mean(DATA[get(activity) <= 0, Mood], na.rm = TRUE), 2)
  ]
  DT_COR[Activity == activity, `Sum Activity` := sum(DATA[[activity]], na.rm = TRUE)]
}

# correlation of activities with each other
MAT_COR <- matrix(data = 0, nrow = length(ACTIVITIES_MOOD), ncol = length(ACTIVITIES_MOOD))
colnames(MAT_COR) <- ACTIVITIES_MOOD
rownames(MAT_COR) <- ACTIVITIES_MOOD
for (i in ACTIVITIES_MOOD) {
  for (j in ACTIVITIES_MOOD) {
    MAT_COR[i, j] <- round(cor(DATA[[i]], DATA[[j]]), 2)
  }
}


# correlation of activities with each other with lag 1 for columns
MAT_COR_LAG <- matrix(data = 0, nrow = length(ACTIVITIES_MOOD), ncol = length(ACTIVITIES_MOOD))
colnames(MAT_COR_LAG) <- ACTIVITIES_MOOD
rownames(MAT_COR_LAG) <- ACTIVITIES_MOOD
for (i in ACTIVITIES_MOOD) {
  for (j in ACTIVITIES_MOOD) {
    MAT_COR_LAG[i, j] <- round(cor(DATA[1:(N-1)][[i]], DATA[2:N][[j]]), 2)
  }
}




WEEKDAYS <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
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

getLocationData <- function(directory) {
  
  tryFormats <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%S.%OSZ",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
  )
  
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
                                     Startzeit = as.POSIXct(duration.startTimestamp, tz = "Europe/Berlin", tryFormats = tryFormats),
                                     Endzeit = as.POSIXct(duration.endTimestamp, tz = "Europe/Berlin", tryFormats = tryFormats)
                                     #Startzeit = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.startTimestamp) / 1000 + 3600,
                                     #Endzeit = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.endTimestamp) / 1000 + 3600
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
  
  if(nrow(dt) == 0)
    return(dt)
  
  dt[, DatumStart := as.Date(Startzeit)]
  dt[, DatumEnde := as.Date(Endzeit)]
  dt[, Latitude := Latitude / 10^7]
  dt[, Longitude := Longitude / 10^7]
  #View(dt[DatumStart != DatumEnde])
  minDate <- min(as.Date(dt$Startzeit))
  maxDate <- max(as.Date(dt$Endzeit))
  
  dtFill <- data.table(Day = seq(minDate, maxDate, by = "days"))
  
  dtFilled <- dtFill[dt, on = .(Day >= DatumStart, Day <= DatumEnde), c(names(dt), "Day"), with = FALSE]
  setorder(dtFilled, "Startzeit")
  #dtFilled[, I := .I, by = .(Startzeit)]
  dtFilled[, Day := Day + rowid(Startzeit) - 1]
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
#DATES_NO_LOCATION_DATA <- seq(min(DT_LOCATION$DatumStart), max(DT_LOCATION$DatumEnde), by = "days")
#DATES_NO_LOCATION_DATA <- DATES_NO_LOCATION_DATA[!DATES_NO_LOCATION_DATA %in% DT_LOCATION$Day]

if(nrow(DT_LOCATION) > 0) {
  PLZ_VISITED <- DT_LOCATION[!is.na(PLZ), .(
    Longitude = mean(Longitude),
    Latitude = mean(Latitude),
    TimesVisited = length(unique(Day)),
    FirstVisit = min(Day),
    LastVisit = max(Day)
  ), by = .(PLZ)]
  
  PLACES_VISITED <- DT_LOCATION[, .(
    Adressen = paste0(unique(Adresse), collapse = "; "),
    Adressname = paste0(unique(Adressname), collapse = "; "),
    AdresseFull = paste0(unique(AdresseFull), collapse = "; "),
    MinDate = min(Day),
    MaxDate = max(Day),
    DaysVisited = length(unique(Day))
  ), by = .(Latitude, Longitude, DistanceFromMeanLocation)]
} else {
  DT_LOCATION[, Day := NA_Date_]
  DT_LOCATION[, AdresseFull := ""]
  PLZ_VISITED <- data.table(
    Longitude = NA_real_,
    Latitude = NA_real_,
    TimesVisited = NA_integer_,
    FirstVisit = NA_Date_,
    LastVisit = NA_Date_
  )
  
  PLACES_VISITED <- data.table(
    Adressen = "",
    Adressname = "",
    AdresseFull = "",
    MinDate = NA_Date_,
    MaxDate = NA_Date_,
    DaysVisited = NA_integer_
  )
}




getMovementData <- function(directory) {
  
  tryFormats <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%S.%OSZ",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
  )
  
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
      wayPoints = waypointPath.waypoints,
      # TODO: Fix lazy hack (+3600): Time is off by one hour.
      Startzeit = as.POSIXct(duration.startTimestamp, tz = "Europe/Berlin", tryFormats = tryFormats),
      Endzeit = as.POSIXct(duration.endTimestamp, tz = "Europe/Berlin", tryFormats = tryFormats)
      #Startzeit = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.startTimestampMs) / 1000 + 3600,
      #Endzeit = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.endTimestampMs) / 1000 + 3600
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
  
  if(nrow(dt) == 0)
    return(dt)
   
  dt[, DatumStart := as.Date(Startzeit)]
  dt[, DatumEnde := as.Date(Endzeit)]
  dt[, StartLatitude := StartLatitude / 10^7]
  dt[, StartLongitude := StartLongitude / 10^7]
  dt[, EndLatitude := EndLatitude / 10^7]
  dt[, EndLongitude := EndLongitude / 10^7]
  #View(dt[DatumStart != DatumEnde])
  minDate <- min(as.Date(dt$Startzeit))
  maxDate <- max(as.Date(dt$Endzeit))
  
  dtFill <- data.table(Day = seq(minDate, maxDate, by = "days"))
  
  dtFilled <- dtFill[dt, on = .(Day >= DatumStart, Day <= DatumEnde), c(names(dt), "Day"), with = FALSE]
  setorder(dtFilled, "Startzeit")
  #dtFilled[, I := .I, by = .(Startzeit)]
  dtFilled[, Day := Day + rowid(Startzeit) - 1]
  #dtFilled[, N := .N, by = .(Startzeit)]
  dtFilled
}

DT_MOVEMENT <- getMovementData("data/GoogleMaps/Semantic Location History/")

if(nrow(DT_MOVEMENT) > 0) {
  DT_MOVEMENT[, a := sin(pi / 180 * (EndLatitude - StartLatitude)/2)^2 + cos(pi / 180 * StartLatitude) * cos(pi / 180 * EndLatitude) * sin(pi / 180 * (EndLongitude-StartLongitude)/2)^2]
  DT_MOVEMENT[, c := 2 * atan2(sqrt(a),sqrt(1-a))]
  
  DT_MOVEMENT[, `Distance by LatLon` := 6371000 * c]
  
}


if(nrow(DT_MOVEMENT) > 0 & nrow(DT_LOCATION) > 0) {
  
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
  
} else {
  DT_ALL_PLACES_VISITED <- data.table(
    Latitude = NA_real_,
    Longitude = NA_real_,
    Adresse = "",
    Adressname = "",
    AdresseFull = "",
    PLZ = NA_integer_,
    Standortinformationen = NA_integer_,
    Color = ""
  )
}





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
      if(activity == dependentActivity | var(DATA[[dependentActivity]]) < 0.1)
        next
      if(length(unique(dtCur[[dependentActivity]])) == 1) {
        dependentVal <- unique(dtCur[[dependentActivity]])
        dependentActivities <- c(dependentActivities, paste0(val, " -> ", dependentActivity, " ", dependentVal))
      }
    }
  }
  dtDependencies[Activity == activity, DependentActivities := paste0(dependentActivities, collapse = ", ")]
}




