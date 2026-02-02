# This is a shiny app made to have a fun internal competition similar to the Vi cykler til arbejde
# Users can log bikerides and see who biked the most and perhaps win a small price

library(shiny)

# Set paths
DATA_DIR <- if(interactive()) "data/rides" else "/srv/shiny-server/vcta/data/rides"
USERS_FILE <- if(interactive()) "data/users.csv" else "/srv/shiny-server/vcta/data/users.csv"


# --- 2️⃣ Ensure folders/files exist ---
if (!dir.exists(DATA_DIR)) dir.create(DATA_DIR, recursive = TRUE)
if (!file.exists(USERS_FILE)) write.csv(data.frame(name = character()), USERS_FILE, row.names = FALSE)

# --- 3️⃣ Helper function to read users ---
read_users <- function() {
  df <- read.csv(USERS_FILE, stringsAsFactors = FALSE)
  if (nrow(df) == 0) return(character(0))
  df$name
}

# --- 4️⃣ UI ---
ui <- fluidPage(
  titlePanel("KBA cykler til arbejde 🚴"),
  uiOutput("main_ui")
)

# --- 5️⃣ Server ---
server <- function(input, output, session) {
  
  user <- reactiveVal(NULL)  # store logged-in user
  
  users <- reactive({
    read_users()
  })
  
  # --- 5a. Render login / main UI ---
  output$main_ui <- renderUI({
    if (is.null(user())) {
      # LOGIN PAGE
      fluidPage(
        h3("Sign in"),
        selectInput("existing_user", "Existing user", choices = c("", users())),
        textInput("new_user", "Or create new user"),
        actionButton("login", "Enter")
      )
    } else {
      # MAIN APP
      fluidPage(
        h4(paste("Logged in as", user())),
        dateInput("date", "Date", Sys.Date()),
        numericInput("distance", "Distance (km)", 0, min = 0),
        numericInput("duration", "Duration (minutes)", 0, min = 0),
        actionButton("save", "Log ride"),
        br(), br(),
        actionButton("logout", "Log out")
      )
    }
  })
  
  # --- 5b. Login logic ---
  observeEvent(input$login, {
    name <- if (nzchar(input$new_user)) input$new_user else input$existing_user
    req(nzchar(name))  # make sure something is entered
    
    name <- tolower(trimws(name))
    user(name)
    
    if (!(name %in% users())) {
      # Append new user
      write.table(
        data.frame(name = name),
        USERS_FILE,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE
      )
    }
  })
  
  # --- 5c. Save ride ---
  observeEvent(input$save, {
    req(user())
    file <- file.path(DATA_DIR, paste0(user(), ".csv"))
    
    row <- data.frame(
      name = user(),
      date = input$date,
      distance = input$distance,
      duration = input$duration
    )
    
    if (file.exists(file)) {
      write.table(row, file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    } else {
      write.csv(row, file, row.names = FALSE)
    }
  })
  
  # --- 5d. Logout ---
  observeEvent(input$logout, {
    user(NULL)
  })
}

# --- 6️⃣ Run app ---
shinyApp(ui, server)
