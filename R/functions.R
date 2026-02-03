# R/functions.R



# Read user
read_users <- function(users_file) {
  df <- read.csv(users_file, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(character(0))
  df$name
}

######################################################################################################

# Log data
save_ride_day <- function(user, date, distance, rain, snacks, mechanical, data_dir) {
  file <- file.path(data_dir, paste0(user, ".csv"))
  
  # Create row for this ride day
  new_row <- data.frame(
    date = as.Date(date),
    distance_km = distance,
    rain = rain,
    snacks = snacks,
    mechanical = mechanical
  )
  
  if (file.exists(file)) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    
    # Remove any existing row for this date
    df <- df[df$date != new_row$date, ]
    
    # Add new row
    df <- rbind(df, new_row)
    
    # Sort by date
    df <- df[order(df$date), ]
    
    # Write back
    write.csv(df, file, row.names = FALSE)
    
  } else {
    # File does not exist yet, just write the new row
    write.csv(new_row, file, row.names = FALSE)
  }
}

######################################################################################################
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
    
    data.frame(
      user = tools::file_path_sans_ext(basename(f)),
      days_ridden = nrow(df),
      total_km = sum(df$distance_km, na.rm = TRUE),
      total_score = compute_total_score(df)
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

######################################################################################################

# --- Scoring constants ---
BASE_DAY_POINTS <- 1
DISTANCE_FACTOR <- 0.005      
RAIN_MULTIPLIER <- 1.1
SNACK_MULTIPLIER <- 0.4
MECHANICAL_MULTIPLIER <- 0.4

# Scoring function
compute_total_score <- function(df) {
  df <- df[order(df$date), ]
  
  total_score <- 0
  snacks_so_far <- 0
  mechanicals_so_far <- 0
  
  for (i in seq_len(nrow(df))) {
    day <- df[i, ]
    
    # Base points
    score <- BASE_DAY_POINTS
    
    # Distance bonus
    score <- score + day$distance_km * DISTANCE_FACTOR
    
    # Rain multiplier
    if (isTRUE(day$rain)) {
      score <- score * RAIN_MULTIPLIER
    }
    
    # Snacks (diminishing)
    if (isTRUE(day$snacks)) {
      snacks_so_far <- snacks_so_far + 1
    }
    score <- score + SNACK_MULTIPLIER * log1p(snacks_so_far)
    
    # Mechanicals (diminishing)
    if (isTRUE(day$mechanical)) {
      mechanicals_so_far <- mechanicals_so_far + 1
    }
    score <- score + MECHANICAL_MULTIPLIER * log1p(mechanicals_so_far)
    
    total_score <- total_score + score
  }
  
  total_score
}
