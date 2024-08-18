get_location_data <- function(directory) {
  tryFormats <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%S.%OSZ",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
  )

  get_location_data_single_month <- function(file_name) {
    df0 <- fromJSON(file_name, flatten = FALSE)[[1]]$placeVisit
    dt0 <- as.data.table(df0)
    if (!"centerLatE7" %in% names(dt0))
      return(data.table())
    if (!"location.name" %in% colnames(dt0))
      dt0[, location.name := "UNKNOWN"]
    dt <- dt0[!is.na(centerLatE7), .(
      Latitude = location.latitudeE7,
      Longitude = location.longitudeE7,
      Address = location.address,
      AddressName = location.name,
      StartTime = as.POSIXct(
        duration.startTimestamp,
        tz = "Europe/Berlin",
        tryFormats = tryFormats
      ),
      EndTime = as.POSIXct(
        duration.endTimestamp,
        tz = "Europe/Berlin",
        tryFormats = tryFormats
      )
    )]
    dt
  }

  dt <- Reduce(
    f = function(x, y)
      rbindlist(list(x, y)),
    x = lapply(
      X = list.files(
        path = directory,
        recursive = TRUE,
        full.names = TRUE
      ),
      FUN = get_location_data_single_month
    ),
    init = data.table()
  )

  if (nrow(dt) == 0)
    return(dt)

  dt[, DateStart := as.Date(StartTime)]
  dt[, DateEnd := as.Date(EndTime)]
  dt[, Latitude := Latitude / 10 ^ 7]
  dt[, Longitude := Longitude / 10 ^ 7]
  #View(dt[DateStart != DateEnd])
  min_date <- min(as.Date(dt$StartTime))
  max_date <- max(as.Date(dt$EndTime))

  dt_fill <- data.table(Day = seq(min_date, max_date, by = "days"))

  dt_filled <-
    dt_fill[dt, on = .(Day >= DateStart, Day <= DateEnd), c(names(dt), "Day"), with = FALSE]
  setorder(dt_filled, "StartTime")
  #dt_filled[, I := .I, by = .(StartTime)]
  dt_filled[, Day := Day + rowid(StartTime) - 1]
  #dt_filled[, N := .N, by = .(StartTime)]
  dt_filled[, AddressFull := ifelse(is.na(Address),
                                   AddressName,
                                   paste0(AddressName, " (", Address, ")"))]

  meanLong <- mean(dt_filled$Longitude)
  meanLat <- mean(dt_filled$Latitude)
  dt_filled[, DistanceFromMeanLocation := sqrt((Longitude - meanLong) ^ 2 + (Latitude - meanLat) ^
                                                2)]
  dt_filled[, POSTCODE := stringr::str_extract(Address, stringr::regex("[0-9]{5}"))]
  # 4 digits and 2 letters for Dutch POSTCODE
  dt_filled[is.na(POSTCODE), POSTCODE := stringr::str_extract(Address, stringr::regex("[0-9]{4} [a-zA-Z]{2}"))]
  #View(DT_LOCATION[is.na(POSTCODE)])
  dt_filled
}

DT_LOCATION <-
  get_location_data("data/GoogleMaps/Semantic Location History/")

if (nrow(DT_LOCATION) > 0) {
  POSTCODE_VISITED <- DT_LOCATION[!is.na(POSTCODE), .(
    Longitude = mean(Longitude),
    Latitude = mean(Latitude),
    TimesVisited = length(unique(Day)),
    FirstVisit = min(Day),
    LastVisit = max(Day)
  ), by = .(POSTCODE)]

  PLACES_VISITED <- DT_LOCATION[, .(
    Addressn = paste0(unique(Address), collapse = "; "),
    AddressName = paste0(unique(AddressName), collapse = "; "),
    AddressFull = paste0(unique(AddressFull), collapse = "; "),
    MinDate = min(Day),
    MaxDate = max(Day),
    DaysVisited = length(unique(Day))
  ), by = .(Latitude, Longitude, DistanceFromMeanLocation)]
} else {
  DT_LOCATION[, Day := NA_Date_]
  DT_LOCATION[, AddressFull := ""]
  POSTCODE_VISITED <- data.table(
    Longitude = NA_real_,
    Latitude = NA_real_,
    TimesVisited = NA_integer_,
    FirstVisit = NA_Date_,
    LastVisit = NA_Date_
  )

  PLACES_VISITED <- data.table(
    Addressn = "",
    AddressName = "",
    AddressFull = "",
    MinDate = NA_Date_,
    MaxDate = NA_Date_,
    DaysVisited = NA_integer_
  )
}

get_movement_data <- function(directory) {
  tryFormats <- c(
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%S.%OSZ",
    "%Y/%m/%d %H:%M:%OS",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d"
  )

  get_movement_single_month <- function(file_name) {
    df0 <- fromJSON(file_name, flatten = FALSE)[[1]]$activitySegment
    dt0 <- as.data.table(df0)
    if (!"startLocation.latitudeE7" %in% names(dt0))
      return(data.table())
    if (!"distance" %in% names(dt0))
      dt0[, distance := NA]
    if (!"activityType" %in% names(dt0))
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
      StartTime = as.POSIXct(
        duration.startTimestamp,
        tz = "Europe/Berlin",
        tryFormats = tryFormats
      ),
      EndTime = as.POSIXct(
        duration.endTimestamp,
        tz = "Europe/Berlin",
        tryFormats = tryFormats
      )
    #StartTime = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.startTimestampMs) / 1000 + 3600,
    #EndTime = as.POSIXct("1970-01-01", tz = "Europe/Berlin") + as.numeric(duration.endTimestampMs) / 1000 + 3600
    )]
    dt
  }

  dt <- Reduce(
    f = function(x, y)
      rbindlist(list(x, y)),
    x = lapply(
      X = list.files(
        path = directory,
        recursive = TRUE,
        full.names = TRUE
      ),
      FUN = get_movement_single_month
    ),
    init = data.table()
  )

  if (nrow(dt) == 0)
    return(dt)

  dt[, DateStart := as.Date(StartTime)]
  dt[, DateEnd := as.Date(EndTime)]
  dt[, StartLatitude := StartLatitude / 10 ^ 7]
  dt[, StartLongitude := StartLongitude / 10 ^ 7]
  dt[, EndLatitude := EndLatitude / 10 ^ 7]
  dt[, EndLongitude := EndLongitude / 10 ^ 7]
  #View(dt[DateStart != DateEnd])
  min_date <- min(as.Date(dt$StartTime))
  max_date <- max(as.Date(dt$EndTime))

  dt_fill <- data.table(Day = seq(min_date, max_date, by = "days"))

  dt_filled <-
    dt_fill[dt, on = .(Day >= DateStart, Day <= DateEnd), c(names(dt), "Day"), with = FALSE]
  setorder(dt_filled, "StartTime")
  #dt_filled[, I := .I, by = .(StartTime)]
  dt_filled[, Day := Day + rowid(StartTime) - 1]
  #dt_filled[, N := .N, by = .(StartTime)]
  dt_filled
}

DT_MOVEMENT <-
  get_movement_data("data/GoogleMaps/Semantic Location History/")

if (nrow(DT_MOVEMENT) > 0) {
  DT_MOVEMENT[, a := sin(pi / 180 * (EndLatitude - StartLatitude) / 2) ^ 2 + cos(pi / 180 * StartLatitude) * cos(pi / 180 * EndLatitude) * sin(pi / 180 * (EndLongitude -
                                                                                                                                                             StartLongitude) / 2) ^ 2]
  DT_MOVEMENT[, c := 2 * atan2(sqrt(a), sqrt(1 - a))]

  DT_MOVEMENT[, `Distance by LatLon` := 6371000 * c]

}


if (nrow(DT_MOVEMENT) > 0 & nrow(DT_LOCATION) > 0) {
  dtAllPlacesVisited <- unique(rbindlist(
    list(DT_LOCATION[, .(Latitude, Longitude, Time = StartTime)],
         DT_LOCATION[, .(Latitude, Longitude, Time = EndTime)],
         DT_MOVEMENT[, .(Latitude = StartLatitude,
                         Longitude = StartLongitude,
                         Time = StartTime)],
         DT_MOVEMENT[, .(Latitude = EndLatitude,
                         Longitude = EndLongitude,
                         Time = EndTime)])
  ))
  dt_place_information <- unique(DT_LOCATION[!is.na(POSTCODE) |
                                             !is.na(Address),
                                           .(
                                             Address = Address[!is.na(Address)][1],
                                             AddressName = AddressName[!is.na(AddressName)][1],
                                             AddressFull = AddressFull[!is.na(AddressFull)][1],
                                             POSTCODE = POSTCODE[!is.na(POSTCODE)][1]
                                           ),
                                           by = .(Latitude, Longitude)])
  dtFirstLastVisit <-
    dtAllPlacesVisited[, .(`First Visit` = min(Time),
                           `Last Visit` = max(Time)), by = .(Latitude, Longitude)]

  DT_ALL_PLACES_VISITED <-
    merge(
      unique(dtAllPlacesVisited[, .(Latitude, Longitude)]),
      dt_place_information,
      by = c("Latitude", "Longitude"),
      all.x = TRUE
    ) %>%
    merge(dtFirstLastVisit,
          by = c("Latitude", "Longitude"),
          all.x = TRUE)
  DT_ALL_PLACES_VISITED[, Standortinformationen := ifelse(is.na(AddressFull) &
                                                            is.na(POSTCODE), 0, 1)]
  DT_ALL_PLACES_VISITED[, Color := ifelse(Standortinformationen == 1, "blue", "black")]

} else {
  DT_ALL_PLACES_VISITED <- data.table(
    Latitude = NA_real_,
    Longitude = NA_real_,
    Address = "",
    AddressName = "",
    AddressFull = "",
    POSTCODE = NA_integer_,
    Standortinformationen = NA_integer_,
    Color = ""
  )
}
