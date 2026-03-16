# Load packages
library(shiny)

# Load home cooked functions
source("R/functions.R")

# --- Paths ---
DATA_DIR   <- if(interactive()) "data/rides" else "/srv/shiny-server/vcta/data/rides"
USERS_FILE <- if(interactive()) "data/users.csv" else "/srv/shiny-server/vcta/data/users.csv"

# --- Predefined teams (edit as needed) ---
TEAMS <- c("Team Alpha", "Team Beta", "Team Gamma", "Team Delta")

# --- Ensure folders/files exist ---
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
if (!file.exists(USERS_FILE)) {
  write.csv(data.frame(name = character(), team = character()),
            USERS_FILE, row.names = FALSE)
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Bike Ride Logger 🚴"),
  uiOutput("login_ui"),
  uiOutput("app_ui"),
  uiOutput("logout_ui")
)

# --- Server ---
server <- function(input, output, session) {
  
  user              <- reactiveVal(NULL)
  leaderboard_trigger <- reactiveVal(0)
  
  # --- Reactive users data frame ---
  users_df <- reactiveFileReader(
    intervalMillis = 1000,
    session        = session,
    filePath       = USERS_FILE,
    readFunc       = function(path) read_users(path)
  )
  
  # --- Reactive leaderboard (folder-level watcher) ---
  leaderboard_data <- reactiveFileReader(
    intervalMillis = 1000,
    session        = session,
    filePath       = DATA_DIR,
    readFunc       = function(path) get_leaderboard(path)
  )
  
  # ── Login UI ──────────────────────────────────────────────────────────────
  output$login_ui <- renderUI({
    if (is.null(user())) {
      existing <- users_df()$name
      fluidPage(
        h3("Sign in"),
        selectInput("existing_user", "Existing user", choices = c("", existing)),
        hr(),
        textInput("new_user", "Or create new user"),
        selectInput("new_team", "Assign to team", choices = TEAMS),
        actionButton("login", "Enter")
      )
    }
  })
  
  # ── Main app UI ────────────────────────────────────────────────────────────
  output$app_ui <- renderUI({
    req(user())
    
    # Build month choices from available data
    months_raw <- get_available_months(DATA_DIR)
    # Format nicely: "2025-03" -> "March 2025"
    month_labels <- format(as.Date(paste0(months_raw, "-01")), "%B %Y")
    month_choices <- setNames(months_raw, month_labels)
    
    fluidPage(
      h4(paste("Logged in as", user(),
               if (!is.na(user_team())) paste0("(", user_team(), ")") else "")),
      
      # Data entry
      dateInput("date",       "Date",           Sys.Date()),
      numericInput("distance","Distance (km)",   0, min = 0, step = 0.1),
      checkboxInput("rain",       "Did it rain?"),
      checkboxInput("snacks",     "Did you bring snacks for the team?"),
      checkboxInput("mechanical", "Did you have a mechanical?"),
      actionButton("save", "Log ride"),
      br(), br(),
      
      # Month selector + individual leaderboard
      h4("Leaderboard"),
      selectInput("selected_month", "Month",
                  choices  = month_choices,
                  selected = months_raw[1]),
      h5("Individual"),
      tableOutput("leaderboard"),
      br(),
      
      # Team leaderboard
      h5("Teams"),
      tableOutput("team_leaderboard"),
      br(),
      
      uiOutput("streak_summary")
    )
  })
  
  # ── Derived: team of the logged-in user ───────────────────────────────────
  user_team <- reactive({
    req(user())
    df <- users_df()
    row <- df[df$name == user(), ]
    if (nrow(row) == 0 || is.na(row$team[1])) return(NA_character_)
    row$team[1]
  })
  
  # ── Login logic ────────────────────────────────────────────────────────────
  observeEvent(input$login, {
    name <- if (nzchar(input$new_user)) input$new_user else input$existing_user
    req(nzchar(name))
    name <- trimws(name)
    user(name)
    
    current <- read_users(USERS_FILE)
    if (!(name %in% current$name)) {
      team <- if (nzchar(input$new_user)) input$new_team else NA_character_
      new_row <- data.frame(name = name, team = team, stringsAsFactors = FALSE)
      current <- rbind(current, new_row)
      write.csv(current, USERS_FILE, row.names = FALSE)
    }
  })
  
  # ── Save ride ──────────────────────────────────────────────────────────────
  observeEvent(input$save, {
    req(user())
    save_ride_day(
      user       = user(),
      date       = input$date,
      distance   = input$distance,
      rain       = input$rain,
      snacks     = input$snacks,
      mechanical = input$mechanical,
      data_dir   = DATA_DIR
    )
    leaderboard_trigger(leaderboard_trigger() + 1)
    showNotification("Ride saved 🚴", type = "message")
  })
  
  # ── Individual leaderboard ─────────────────────────────────────────────────
  output$leaderboard <- renderTable({
    req(user(), input$selected_month)
    leaderboard_trigger()
    
    df <- get_leaderboard(DATA_DIR,
                          year_month = input$selected_month,
                          users_df   = users_df())
    if (nrow(df) == 0) return(data.frame(Message = "No rides logged for this month."))
    
    df$total_km    <- round(df$total_km, 2)
    df$total_score <- round(df$total_score, 2)
    
    # Medal ranks
    df$rank <- ifelse(df$rank == 1, paste0("<span style='color:gold;'>",     df$rank, " 🥇</span>"),
               ifelse(df$rank == 2, paste0("<span style='color:silver;'>",   df$rank, " 🥈</span>"),
               ifelse(df$rank == 3, paste0("<span style='color:#cd7f32;'>",  df$rank, " 🥉</span>"),
                      df$rank)))
    
    # Bold logged-in user
    df[df$user == user(), ] <- lapply(df[df$user == user(), ],
                                      function(x) paste0("<b>", x, "</b>"))
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE,
     sanitize.text.function = function(x) x)
  
  # ── Team leaderboard ───────────────────────────────────────────────────────
  output$team_leaderboard <- renderTable({
    req(user(), input$selected_month)
    leaderboard_trigger()
    
    df <- get_team_leaderboard(DATA_DIR,
                               year_month = input$selected_month,
                               users_df   = users_df())
    if (nrow(df) == 0) return(data.frame(Message = "No team data available."))
    
    # Medal ranks
    df$rank <- ifelse(df$rank == 1, paste0("<span style='color:gold;'>",     df$rank, " 🥇</span>"),
               ifelse(df$rank == 2, paste0("<span style='color:silver;'>",   df$rank, " 🥈</span>"),
               ifelse(df$rank == 3, paste0("<span style='color:#cd7f32;'>",  df$rank, " 🥉</span>"),
                      df$rank)))
    
    # Bold user's team
    ut <- user_team()
    if (!is.na(ut)) {
      df[df$team == ut, ] <- lapply(df[df$team == ut, ],
                                    function(x) paste0("<b>", x, "</b>"))
    }
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE,
     sanitize.text.function = function(x) x)
  
  # ── Personal summary / streak ──────────────────────────────────────────────
  output$streak_summary <- renderUI({
    req(user(), input$selected_month)
    leaderboard_trigger()
    
    file <- file.path(DATA_DIR, paste0(user(), ".csv"))
    
    # Determine date range for selected month
    first_day <- as.Date(paste0(input$selected_month, "-01"))
    last_day  <- min(
      seq(first_day, by = "month", length.out = 2)[2] - 1,
      Sys.Date()
    )
    is_current_month <- input$selected_month == format(Sys.Date(), "%Y-%m")
    total_days <- as.numeric(format(last_day, "%d"))
    
    days_ridden <- 0
    if (file.exists(file)) {
      df <- read.csv(file, stringsAsFactors = FALSE)
      df$date <- as.Date(df$date)
      df <- df[df$date >= first_day & df$date <= last_day, ]
      days_ridden <- length(unique(df$date))
    }
    
    progress <- if (total_days > 0) days_ridden / total_days else 0
    
    lb         <- get_leaderboard(DATA_DIR,
                                  year_month = input$selected_month,
                                  users_df   = users_df())
    user_row   <- lb[lb$user == user(), ]
    user_score <- if (nrow(user_row) == 0) 0 else user_row$total_score
    top_row    <- if (nrow(lb) == 0) NULL else lb[which.max(lb$total_score), ]
    top_score  <- if (is.null(top_row)) 0 else top_row$total_score
    top_name   <- if (is.null(top_row)) "" else top_row$user
    user_rank  <- which(lb$user == user())
    n_users    <- nrow(lb)
    points_behind <- round(top_score - user_score, 2)
    
    top1_msgs <- c("🔥 You're leading! Keep it up! 🚴‍♂️💨",
                   "🏆 Top of the leaderboard! Don't slow down! 🎯",
                   "Champion mode activated! 🚴‍♀️🥇💪")
    top2_msgs <- c(paste0("Only ", points_behind, " points to catch ", top_name, " — go get 'em! ⚡🚴"),
                   paste0("Close! ", points_behind, " points away from ", top_name, " — pedal fast! 🏁"),
                   paste0("Second place! Just ", points_behind, " points to take the lead! 🌟🚴‍♂️"))
    top3_msgs <- c("Almost there! Push a little more and take the podium 🏆🚴‍♀️",
                   "Third place is yours to improve — ride on! ⚡💪",
                   "Keep it up! Podium is within reach! 🚴‍♂️🥉")
    top10_msgs <- c("You're close! Keep riding to get into the top 3! 🚴‍♀️💨",
                    "Top 10! Just a bit more effort to reach the podium! 🌟🚴",
                    "Almost there — consistency pays off 🚴‍♂️✨")
    last_place_msgs <- c(paste0("Ouch! You're in last 😅. Even ", user(), " can do better! 🚲💨"),
                         "Last place! Time to pedal faster! 🐌🚴‍♂️",
                         "Don't worry, the leaderboard won't stay this way forever! 😏🚲")
    bottom_msgs <- c("Keep going! Every ride counts! 💪🚲",
                     "Everyone starts somewhere — pedal and climb up! 🌄🚴‍♀️",
                     paste0("Don't worry, even ", user(), " had to start somewhere 😏 — ride to catch up! 🚲"))
    
    msg <- if (length(user_rank) == 0 || n_users == 0) {
      "Log a ride to get on the leaderboard! 🚴"
    } else if (user_rank == 1) {
      sample(top1_msgs, 1)
    } else if (user_rank == 2) {
      sample(top2_msgs, 1)
    } else if (user_rank == 3) {
      sample(top3_msgs, 1)
    } else if (user_rank <= 10) {
      sample(top10_msgs, 1)
    } else if (user_rank == n_users) {
      sample(last_place_msgs, 1)
    } else {
      sample(bottom_msgs, 1)
    }
    
    month_label <- format(first_day, "%B %Y")
    streak_label <- if (is_current_month) "Your monthly streak 🚴" else paste("Your stats —", month_label)
    
    tags$div(
      tags$h4(streak_label),
      tags$div(
        style = "background: lightgray; width: 100%; height: 20px; border-radius: 5px;",
        tags$div(style = sprintf(
          "background: steelblue; width: %.1f%%; height: 20px; border-radius: 5px;",
          progress * 100))
      ),
      tags$p(paste0(days_ridden, "/", total_days, " days ridden in ", month_label)),
      tags$p(msg)
    )
  })
  
  # ── Logout ─────────────────────────────────────────────────────────────────
  output$logout_ui <- renderUI({
    req(user())
    actionButton("logout", "Log out")
  })
  
  observeEvent(input$logout, {
    user(NULL)
  })
}

# --- Run app ---
shinyApp(ui, server)
