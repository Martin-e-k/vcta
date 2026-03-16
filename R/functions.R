# R/functions.R

# Read users — returns full data frame with name and team
read_users <- function(users_file) {
  df <- read.csv(users_file, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(data.frame(name = character(), team = character()))
  if (!"team" %in% names(df)) df$team <- NA_character_
  df
}

######################################################################################################

# Log data
save_ride_day <- function(user, date, distance, rain, snacks, mechanical, data_dir) {
  file <- file.path(data_dir, paste0(user, ".csv"))
  
  new_row <- data.frame(
    date       = as.Date(date),
    distance_km = distance,
    rain       = rain,
    snacks     = snacks,
    mechanical = mechanical
  )
  
  if (file.exists(file)) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    df <- df[df$date != new_row$date, ]
    df <- rbind(df, new_row)
    df <- df[order(df$date), ]
    write.csv(df, file, row.names = FALSE)
  } else {
    write.csv(new_row, file, row.names = FALSE)
  }
}

######################################################################################################

# Returns character vector of "YYYY-MM" months present across all ride files, newest first
get_available_months <- function(data_dir) {
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(format(Sys.Date(), "%Y-%m"))
  
  months <- unique(unlist(lapply(files, function(f) {
    df <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df$date <- as.Date(df$date)
    format(df$date, "%Y-%m")
  })))
  
  sort(unique(months), decreasing = TRUE)
}

######################################################################################################

# Individual leaderboard for a given year_month "YYYY-MM"
get_leaderboard <- function(data_dir, year_month = NULL, users_df = NULL) {
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(files) == 0) return(data.frame())
  
  if (is.null(year_month)) year_month <- format(Sys.Date(), "%Y-%m")
  
  first_day <- as.Date(paste0(year_month, "-01"))
  last_day  <- min(
    seq(first_day, by = "month", length.out = 2)[2] - 1,
    Sys.Date()
  )
  
  leaderboard <- lapply(files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    df <- df[df$date >= first_day & df$date <= last_day, ]
    if (nrow(df) == 0) return(NULL)
    data.frame(
      user        = tools::file_path_sans_ext(basename(f)),
      days_ridden = nrow(df),
      total_km    = sum(df$distance_km, na.rm = TRUE),
      total_score = compute_total_score(df)
    )
  })
  
  leaderboard <- do.call(rbind, leaderboard)
  if (is.null(leaderboard) || nrow(leaderboard) == 0) return(data.frame())
  
  if (!is.null(users_df) && "team" %in% names(users_df)) {
    leaderboard <- merge(leaderboard, users_df[, c("name", "team")],
                         by.x = "user", by.y = "name", all.x = TRUE)
  } else {
    leaderboard$team <- NA_character_
  }
  
  leaderboard <- leaderboard[order(-leaderboard$total_score), ]
  leaderboard$rank <- seq_len(nrow(leaderboard))
  leaderboard[, c("rank", "user", "team", "days_ridden", "total_km", "total_score")]
}

######################################################################################################

# Team leaderboard — aggregated from individual scores
get_team_leaderboard <- function(data_dir, year_month = NULL, users_df = NULL) {
  ind <- get_leaderboard(data_dir, year_month = year_month, users_df = users_df)
  if (nrow(ind) == 0) return(data.frame())
  
  ind <- ind[!is.na(ind$team) & nzchar(ind$team), ]
  if (nrow(ind) == 0) return(data.frame())
  
  team_lb <- aggregate(cbind(total_km, total_score, days_ridden) ~ team,
                        data = ind, FUN = sum)
  
  member_counts      <- aggregate(user ~ team, data = ind, FUN = length)
  names(member_counts)[2] <- "members"
  team_lb            <- merge(team_lb, member_counts, by = "team")
  team_lb            <- team_lb[order(-team_lb$total_score), ]
  team_lb$rank       <- seq_len(nrow(team_lb))
  team_lb$total_km    <- round(team_lb$total_km, 2)
  team_lb$total_score <- round(team_lb$total_score, 2)
  team_lb[, c("rank", "team", "members", "days_ridden", "total_km", "total_score")]
}

######################################################################################################

# --- Scoring constants ---
BASE_DAY_POINTS       <- 1
DISTANCE_FACTOR       <- 0.005      
RAIN_MULTIPLIER       <- 1.1
SNACK_MULTIPLIER      <- 0.4
MECHANICAL_MULTIPLIER <- 0.4

compute_total_score <- function(df) {
  df <- df[order(df$date), ]
  total_score <- 0
  snacks_so_far      <- 0
  mechanicals_so_far <- 0
  
  for (i in seq_len(nrow(df))) {
    day   <- df[i, ]
    score <- BASE_DAY_POINTS + day$distance_km * DISTANCE_FACTOR
    if (isTRUE(day$rain))       score <- score * RAIN_MULTIPLIER
    if (isTRUE(day$snacks))     snacks_so_far      <- snacks_so_far + 1
    score <- score + SNACK_MULTIPLIER * log1p(snacks_so_far)
    if (isTRUE(day$mechanical)) mechanicals_so_far <- mechanicals_so_far + 1
    score <- score + MECHANICAL_MULTIPLIER * log1p(mechanicals_so_far)
    total_score <- total_score + score
  }
  total_score
}
