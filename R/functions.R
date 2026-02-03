# R/functions.R



# Read user
read_users <- function(users_file) {
  df <- read.csv(users_file, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(character(0))
  df$name
}



# Log data
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


# Get leaderboard function
get_leaderboard <- function(data_dir) {
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(data.frame())
  
  today <- Sys.Date()
  first_day <- as.Date(format(today, "%Y-%m-01"))
  
  leaderboard <- lapply(files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    
    # Filter for current month
    df <- df[df$date >= first_day & df$date <= today, ]
    if (nrow(df) == 0) return(NULL)
    
    # Calculate score per day
    df$score <- 1 + 0.1 * df$distance_km +
      0.5 * as.numeric(df$rain) +
      0.5 * as.numeric(df$snacks) +
      0.5 * as.numeric(df$mechanical)
    
    data.frame(
      user = tools::file_path_sans_ext(basename(f)),
      days_ridden = nrow(df),
      total_km = sum(df$distance_km, na.rm = TRUE),
      total_score = sum(df$score, na.rm = TRUE)
    )
  })
  
  leaderboard <- do.call(rbind, leaderboard)
  if (is.null(leaderboard) || nrow(leaderboard) == 0) return(data.frame())
  
  # Rank by total_score descending
  leaderboard <- leaderboard[order(-leaderboard$total_score), ]
  leaderboard$rank <- seq_len(nrow(leaderboard))
  
  # Reorder columns
  leaderboard <- leaderboard[, c("rank", "user", "days_ridden", "total_km", "total_score")]
  
  leaderboard
}
