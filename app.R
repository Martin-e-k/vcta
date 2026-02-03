# Load packages
library(shiny)

# Load home cooked functions
source("R/functions.R")

# --- Paths ---
DATA_DIR <- if(interactive()) "data/rides" else "/srv/shiny-server/vcta/data/rides"
USERS_FILE <- if(interactive()) "data/users.csv" else "/srv/shiny-server/vcta/data/users.csv"

# --- Ensure folders/files exist ---
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
if (!file.exists(USERS_FILE)) write.csv(data.frame(name = character()), USERS_FILE, row.names = FALSE)


# --- UI ---
ui <- fluidPage(
  titlePanel("VC Bike Ride Logger 🚴"),
  uiOutput("login_ui"),
  uiOutput("app_ui"),
  uiOutput("logout_ui")   # <- new UI output for logout
)

# --- Server ---
server <- function(input, output, session) {
  
  user <- reactiveVal(NULL)
  leaderboard_trigger <- reactiveVal(0)
  
  # --- Reactive list of users ---
  users <- reactiveFileReader(
    intervalMillis = 1000,        # check every 1 second
    session = session,
    filePath = USERS_FILE,
    readFunc = function(path) {
      df <- read.csv(path, stringsAsFactors = FALSE)
      if (nrow(df) == 0) return(character(0))
      df$name
    }
  )
  
  # --- Reactive leaderboard ---
  leaderboard_data <- reactiveFileReader(
    intervalMillis = 1000,             # checks every second
    session = session,
    filePath = DATA_DIR,                # folder containing user CSVs
    readFunc = function(path) {
      get_leaderboard(path)
    }
  )
  
  
  # --- Login UI ---
  output$login_ui <- renderUI({
    if (is.null(user())) {
      fluidPage(
        h3("Sign in"),
        selectInput("existing_user", "Existing user", choices = c("", users())),
        textInput("new_user", "Or create new user"),
        actionButton("login", "Enter")
      )
    }
  })
  
  # --- Main app UI ---
  output$app_ui <- renderUI({
    req(user())
    
    fluidPage(
      h4(paste("Logged in as", user())),
      
      # Data entry
      dateInput("date", "Date", Sys.Date()),
      numericInput("distance", "Distance (km)", 0, min = 0, step = 0.1),
      checkboxInput("rain", "Did it rain?"),
      checkboxInput("snacks", "Did you bring snacks for the team?"),
      checkboxInput("mechanical", "Did you have a mechanical?"),
      actionButton("save", "Log ride"),
      br(), br(),
      
      # Leaderboard
      h4("Leaderboard (Current Month)"),
      tableOutput("leaderboard"),
      br(),
      
      uiOutput("streak_summary")
    )
  })
  
  
  # --- Login logic ---
  observeEvent(input$login, {
    name <- if (nzchar(input$new_user)) input$new_user else input$existing_user
    req(nzchar(name))
    name <- trimws(name)
    user(name)
    
    # --- Add new user if not present ---
    current_users <- read.csv(USERS_FILE, stringsAsFactors = FALSE)
    if (!(name %in% current_users$name)) {
      current_users <- rbind(current_users, data.frame(name = name))
      write.csv(current_users, USERS_FILE, row.names = FALSE)
    }
  })
  
  # Plot user rides
  output$rides_plot <- renderPlot({
    req(user())  # ensures a user is logged in before plotting
    
    # File path for current user
    file <- file.path(DATA_DIR, paste0(user(), ".csv"))
    if (!file.exists(file)) return(NULL)  # nothing to plot yet
    
    # Load user's rides
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    
    # Filter to current month
    today <- Sys.Date()
    df <- df[df$date >= as.Date(format(today, "%Y-%m-01")) &
               df$date <= today, ]
    
    if (nrow(df) == 0) return(NULL)  # no rides this month yet
    
    # Load ggplot2 if not already
    library(ggplot2)
    
    # Create a simple bar chart
    ggplot(df, aes(x = date, y = distance_km)) +
      geom_col(fill = "steelblue") +
      labs(
        x = "Date",
        y = "Distance (km)",
        title = paste("Rides in", format(today, "%B %Y"))
      ) +
      theme_minimal()
  })
  
  
  # --- Save ride ---
  observeEvent(input$save, {
    req(user())
    
    # Call the reusable function
    save_ride_day(
      user = user(),
      date = input$date,
      distance = input$distance,
      rain = input$rain,
      snacks = input$snacks,
      mechanical = input$mechanical,
      data_dir = DATA_DIR
    )
    
    # Increment trigger to update leaderboard
    leaderboard_trigger(leaderboard_trigger() + 1)
    
    # Show a simple notification
    showNotification("Ride saved 🚴", type = "message")
  })
  
  # --- Show leaderboard ---
  output$leaderboard <- renderTable({
    req(user())
    
    # Make reactive to trigger
    leaderboard_trigger()
    
    # Get leaderboard
    df <- get_leaderboard(DATA_DIR)
    
    if (nrow(df) == 0) return(df)
    
    # Round numeric columns
    df$total_km <- round(df$total_km, 2)
    df$total_score <- round(df$total_score, 2)
    
    # Highlight top 3 ranks with medals
    df$rank <- ifelse(df$rank == 1, paste0("<span style='color:gold;'>", df$rank, " 🥇</span>"),
                      ifelse(df$rank == 2, paste0("<span style='color:silver;'>", df$rank, " 🥈</span>"),
                             ifelse(df$rank == 3, paste0("<span style='color:#cd7f32;'>", df$rank, " 🥉</span>"),
                                    df$rank)))
    
    # Bold logged-in user
    df[df$user == user(), ] <- lapply(df[df$user == user(), ], function(x) paste0("<b>", x, "</b>"))
    
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, sanitize.text.function = function(x) x)
  
  
  # --- Show personal summary ---
  # --- Show personal summary ---
  output$streak_summary <- renderUI({
    req(user())
    
    # --- Load current user rides ---
    file <- file.path(DATA_DIR, paste0(user(), ".csv"))
    if (!file.exists(file)) return("No rides logged yet!")
    
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    
    # Filter current month
    today <- Sys.Date()
    df <- df[df$date >= as.Date(format(today, "%Y-%m-01")) &
               df$date <= today, ]
    
    days_ridden <- length(unique(df$date))
    total_days <- as.numeric(format(today, "%d"))
    progress <- days_ridden / total_days
    
    # --- Leaderboard data (reactive) ---
    leaderboard_trigger()  # make reactive
    lb <- get_leaderboard(DATA_DIR)
    
    # Current user score
    user_row <- lb[lb$user == user(), ]
    user_score <- if(nrow(user_row) == 0) 0 else user_row$total_score
    
    # Top scorer
    top_row <- lb[which.max(lb$total_score), ]
    top_score <- top_row$total_score
    top_name <- top_row$user
    
    # Determine rank
    user_rank <- which(lb$user == user())
    
    # Determine points behind
    points_behind <- round(top_score - user_score, 2)
    
    # --- Emoji-enhanced motivational messages ---
    top1_msgs <- c(
      "🔥 You’re leading! Keep it up! 🚴‍♂️💨",
      "🏆 Top of the leaderboard! Don’t slow down! 🎯",
      "Champion mode activated! 🚴‍♀️🥇💪"
    )
    
    top2_msgs <- c(
      paste0("Only ", points_behind, " points to catch ", top_name, " — go get ’em! ⚡🚴"),
      paste0("Close! ", points_behind, " points away from ", top_name, " — pedal fast! 🏁"),
      paste0("Second place! Just ", points_behind, " points to take the lead! 🌟🚴‍♂️")
    )
    
    top3_msgs <- c(
      "Almost there! Push a little more and take the podium 🏆🚴‍♀️",
      "Third place is yours to improve — ride on! ⚡💪",
      "Keep it up! Podium is within reach! 🚴‍♂️🥉"
    )
    
    top10_msgs <- c(
      "You’re close! Keep riding to get into the top 3! 🚴‍♀️💨",
      "Top 10! Just a bit more effort to reach the podium! 🌟🚴",
      "Almost there — consistency pays off 🚴‍♂️✨"
    )
    
    bottom_msgs <- c(
      paste0("Don’t worry, even ", user(), " had to start somewhere 😏 — ride to catch up! 🚲"),
      "Everyone starts somewhere — pedal and climb up! 🌄🚴‍♀️",
      "Keep going! Every ride counts! 💪🚲"
    )
    
    msg <- if (user_rank == 1) {
      sample(top1_msgs, 1)
    } else if (user_rank == 2) {
      sample(top2_msgs, 1)
    } else if (user_rank == 3) {
      sample(top3_msgs, 1)
    } else if (user_rank <= 10) {
      sample(top10_msgs, 1)
    } else {
      sample(bottom_msgs, 1)
    }
    
    # --- UI ---
    tags$div(
      tags$h4("Your monthly streak 🚴"),
      tags$div(
        style = "background: lightgray; width: 100%; height: 20px; border-radius: 5px;",
        tags$div(
          style = sprintf("background: steelblue; width: %.1f%%; height: 20px; border-radius: 5px;", progress*100)
        )
      ),
      tags$p(paste0(days_ridden, "/", total_days, " days ridden this month")),
      tags$p(msg)
    )
  })
  
  
  output$logout_ui <- renderUI({
    req(user())   # only show if logged in
    actionButton("logout", "Log out")
  })
  
  # --- Logout logic ---
  observeEvent(input$logout, {
    user(NULL)  # resets logged-in user
  })
  
  
}

# --- Run app ---
shinyApp(ui, server)
