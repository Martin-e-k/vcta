# Load packages
library(shiny)

# Load home cooked functions

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
  uiOutput("app_ui")
)

# --- Server ---
server <- function(input, output, session) {
  
  user <- reactiveVal(NULL)
  
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
      
      # Current month rides plot
      h4("Current month rides"),
      plotOutput("rides_plot", height = "300px"),
      br(),
      
      actionButton("logout", "Log out")
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
    
    # Optional: show a simple notification
    showNotification("Ride saved 🚴", type = "message")
  })
  
  # --- Show leaderboard ---
  output$leaderboard <- renderTable({
    req(user())
    
    # Get leaderboard
    df <- get_leaderboard(DATA_DIR)
    
    if (nrow(df) == 0) return(df)
    
    # Highlight top 3 ranks with medals
    df$rank <- ifelse(df$rank == 1, paste0("<span style='color:gold;'>", df$rank, " 🥇</span>"),
                      ifelse(df$rank == 2, paste0("<span style='color:silver;'>", df$rank, " 🥈</span>"),
                             ifelse(df$rank == 3, paste0("<span style='color:#cd7f32;'>", df$rank, " 🥉</span>"),
                                    df$rank)))
    
    # Bold all cells for the logged-in user
    df[df$user == user(), ] <- lapply(df[df$user == user(), ], function(x) paste0("<b>", x, "</b>"))
    
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, sanitize.text.function = function(x) x)
  
  
  # --- Logout ---
  observeEvent(input$logout, {
    user(NULL)
  })
  
}

# --- Run app ---
shinyApp(ui, server)
