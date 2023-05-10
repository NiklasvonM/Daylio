
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


