# R/functions.R

read_users <- function(users_file) {
  df <- read.csv(users_file, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(character(0))
  df$name
}

save_ride_day <- function(
    user,
    date,
    distance,
    rain,
    snacks,
    mechanical,
    data_dir
) {
  file <- file.path(data_dir, paste0(user, ".csv"))
  
  date <- as.Date(date)
  distance <- as.numeric(distance)
  
  if (!file.exists(file)) {
    df <- data.frame(
      date = date,
      distance_km = distance,
      rain = rain,
      snacks = snacks,
      mechanical = mechanical
    )
    write.csv(df, file, row.names = FALSE)
    return(invisible(TRUE))
  }
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  
  if (date %in% df$date) {
    i <- which(df$date == date)
    df$distance_km[i] <- df$distance_km[i] + distance
    df$rain[i]       <- df$rain[i] | rain
    df$snacks[i]    <- df$snacks[i] | snacks
    df$mechanical[i]<- df$mechanical[i] | mechanical
  } else {
    df <- rbind(df, data.frame(
      date = date,
      distance_km = distance,
      rain = rain,
      snacks = snacks,
      mechanical = mechanical
    ))
  }
  
  df <- df[order(df$date), ]
  write.csv(df, file, row.names = FALSE)
  
  invisible(TRUE)
}
