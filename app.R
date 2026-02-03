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
      
      # Ride entry
      dateInput("date", "Date", Sys.Date()),
      numericInput("distance", "Distance (km)", 0, min = 0, step = 0.1),
      
      checkboxInput("rain", "Did it rain?"),
      checkboxInput("snacks", "Did you bring snacks for the team?"),
      checkboxInput("mechanical", "Did you have a mechanical?"),
      
      actionButton("save", "Log ride"),
      br(), br(),
      
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
  
  # --- Logout ---
  observeEvent(input$logout, {
    user(NULL)
  })
  
}

# --- Run app ---
shinyApp(ui, server)
